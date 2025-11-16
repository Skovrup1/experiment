package test

import "../compiler/lexer"
import "../compiler/parser"

import "core:fmt"
import "core:math/rand"
import "core:strings"

gen_identifier :: proc() -> string {
	names := []string{"x", "y", "z", "i", "j", "k", "acc", "sum", "result", "n", "m"}
	idx := rand.int31_max(i32(len(names)))
	return names[idx]
}

gen_type_name :: proc() -> string {
	types := []string{"S32", "S64", "U32", "U64"}
	idx := rand.int31_max(i32(len(types)))
	return types[idx]
}

gen_integer :: proc() -> i64 {
	return i64(rand.int31_max(100))
}

add_synthetic_token :: proc(p: ^parser.Parser, tokens: ^[dynamic]lexer.Token, text: string, kind: lexer.TokenKind) -> lexer.TokenIndex {
	start := lexer.TokenIndex(len(p.source))
	p.source = strings.concatenate({p.source, text})
	end := lexer.TokenIndex(len(p.source))
	token := lexer.Token{kind, start, end}
	append(tokens, token)
	return lexer.TokenIndex(len(tokens) - 1)
}

gen_expr :: proc(p: ^parser.Parser, tokens: ^[dynamic]lexer.Token, depth: int = 0, min_prec: int = 0) -> parser.NodeIndex {
	if depth > 3 {
		if rand.int31_max(2) == 0 {
			val := gen_integer()
			str := fmt.tprintf("%d", val)
			token := add_synthetic_token(p, tokens, str, .Number)
			return parser.add_node(p, parser.Node{.Integer, parser.INVALID_DATA, token})
		} else {
			name := gen_identifier()
			token := add_synthetic_token(p, tokens, name, .Identifier)
			return parser.add_node(p, parser.Node{.Identifier, parser.INVALID_DATA, token})
		}
	}

	left: parser.NodeIndex
	if rand.int31_max(2) == 0 {
		val := gen_integer()
		str := fmt.tprintf("%d", val)
		token := add_synthetic_token(p, tokens, str, .Number)
		left = parser.add_node(p, parser.Node{.Integer, parser.INVALID_DATA, token})
	} else {
		name := gen_identifier()
		token := add_synthetic_token(p, tokens, name, .Identifier)
		left = parser.add_node(p, parser.Node{.Identifier, parser.INVALID_DATA, token})
	}

	if depth < 3 && rand.int31_max(2) == 0 {
		// choose an operator with precedence >= min_prec
		choice := rand.int31_max(4)
		
		switch choice {
		case 0:
			if min_prec <= 1 {
				right := gen_expr(p, tokens, depth + 1, 1)
				assign_expr := parser.AssignExpr{left, right}
				data := parser.encode_data(p, assign_expr)
				token := add_synthetic_token(p, tokens, "=", .Equal)
				return parser.add_node(p, parser.Node{.Assignment, data, token})
			}
		case 1:
			if min_prec <= 2 {
				right := gen_expr(p, tokens, depth + 1, 3)  // Right side has higher precedence
				less_expr := parser.LessExpr{left, right}
				data := parser.encode_data(p, less_expr)
				token := add_synthetic_token(p, tokens, "<", .Less)
				return parser.add_node(p, parser.Node{.Less, data, token})
			}
		case 2: 
			if min_prec <= 3 {
				right := gen_expr(p, tokens, depth + 1, 3)
				add_expr := parser.AddExpr{left, right}
				data := parser.encode_data(p, add_expr)
				token := add_synthetic_token(p, tokens, "+", .Plus)
				return parser.add_node(p, parser.Node{.Addition, data, token})
			}
		case 3:
			if min_prec <= 4 {
				// sometimes allow lower precedence on right to create cases like (a + b) * c
				// but only if we're not too deep to avoid complex nested expressions
				next_prec := (depth < 2 && rand.int31_max(3) == 0) ? 0 : 4
				right := gen_expr(p, tokens, depth + 1, next_prec)
				mul_expr := parser.MulExpr{left, right}
				data := parser.encode_data(p, mul_expr)
				token := add_synthetic_token(p, tokens, "*", .Asterisk)
				return parser.add_node(p, parser.Node{.Multiplication, data, token})
			}
		}
	}

	return left
}

gen_stmt :: proc(p: ^parser.Parser, tokens: ^[dynamic]lexer.Token, depth: int = 0) -> parser.NodeIndex {
	if depth > 2 {
		value := gen_expr(p, tokens, 0)
		return_stmt := parser.ReturnStmt{value}
		data := parser.encode_data(p, return_stmt)
		token := add_synthetic_token(p, tokens, "return", .Return)
		return parser.add_node(p, parser.Node{.Return, data, token})
	}

	choice := rand.int31_max(4)
	switch choice {
	case 0: // variable declaration
		name := gen_identifier()
		token := add_synthetic_token(p, tokens, name, .Identifier)
		value := gen_expr(p, tokens, 0)
		var_stmt := parser.VarStmt{parser.INVALID_NODE, value}
		data := parser.encode_data(p, var_stmt)
		return parser.add_node(p, parser.Node{.Variable, data, token})
	
	case 1: // for statement
		// Generate initial statement: i := 0
		init_name := gen_identifier()
		init_token := add_synthetic_token(p, tokens, init_name, .Identifier)
		init_value := gen_expr(p, tokens, 0)
		init_stmt := parser.VarStmt{parser.INVALID_NODE, init_value}
		init_data := parser.encode_data(p, init_stmt)
		initial := parser.add_node(p, parser.Node{.Variable, init_data, init_token})
		
		// Generate condition: i < n
		condition := gen_expr(p, tokens, 0, 0)
		
		// Generate update: i = i + 1
		update := gen_expr(p, tokens, 0, 0)
		
		// Generate body block
		body_stmt_count := 1 + rand.int31_max(3)
		body_stmts := make([dynamic]parser.NodeIndex)
		for i in 0..<body_stmt_count {
			append(&body_stmts, gen_stmt(p, tokens, depth + 1))
		}
		body_block := parser.BlockStmt{body_stmts[:]}
		body_data := parser.encode_data(p, body_block)
		body_token := add_synthetic_token(p, tokens, "{", .LeftBrace)
		body := parser.add_node(p, parser.Node{.Block, body_data, body_token})
		
		// Create for statement
		for_stmt := parser.ForStmt{initial, condition, update, body}
		data := parser.encode_data(p, for_stmt)
		token := add_synthetic_token(p, tokens, "for", .For)
		return parser.add_node(p, parser.Node{.For, data, token})
	
	case 2: // block statement, don't generate nested blocks for now
		value := gen_expr(p, tokens, 0)
		return_stmt := parser.ReturnStmt{value}
		data := parser.encode_data(p, return_stmt)
		token := add_synthetic_token(p, tokens, "return", .Return)
		return parser.add_node(p, parser.Node{.Return, data, token})
	
	case: // return statement
		value := gen_expr(p, tokens, 0)
		return_stmt := parser.ReturnStmt{value}
		data := parser.encode_data(p, return_stmt)
		token := add_synthetic_token(p, tokens, "return", .Return)
		return parser.add_node(p, parser.Node{.Return, data, token})
	}
}

gen_parameter :: proc(p: ^parser.Parser, tokens: ^[dynamic]lexer.Token) -> parser.NodeIndex {
	param_name := gen_identifier()
	param_token := add_synthetic_token(p, tokens, param_name, .Identifier)
	
	type_name := gen_type_name()
	type_token := add_synthetic_token(p, tokens, type_name, .Identifier)
	type_node := parser.add_node(p, parser.Node{.Primitive, parser.INVALID_DATA, type_token})
	
	param_stmt := parser.ParamStmt{type_node, parser.INVALID_NODE}
	data := parser.encode_data(p, param_stmt)
	
	return parser.add_node(p, parser.Node{.Parameter, data, param_token})
}

gen_procedure :: proc(p: ^parser.Parser, tokens: ^[dynamic]lexer.Token) -> parser.NodeIndex {
	name := gen_identifier()
	token := add_synthetic_token(p, tokens, "def", .Def)
	id_token := add_synthetic_token(p, tokens, name, .Identifier)
	
	param_count := rand.int31_max(4)
	params := make([dynamic]parser.NodeIndex)
	for i in 0..<param_count {
		append(&params, gen_parameter(p, tokens))
	}
	
	ret_type_name := gen_type_name()
	ret_type_token := add_synthetic_token(p, tokens, ret_type_name, .Identifier)
	ret_type := parser.add_node(p, parser.Node{.Primitive, parser.INVALID_DATA, ret_type_token})
	
	stmt_count := 1 + rand.int31_max(4)
	stmts := make([dynamic]parser.NodeIndex)
	for i in 0..<stmt_count {
		append(&stmts, gen_stmt(p, tokens, 0))
	}
	
	block_stmt := parser.BlockStmt{stmts[:]}
	block_data := parser.encode_data(p, block_stmt)
	body_token := add_synthetic_token(p, tokens, "{", .LeftBrace)
	body := parser.add_node(p, parser.Node{.Block, block_data, body_token})
	
	proc_stmt := parser.ProcStmt{id_token, ret_type, body, params[:]}
	proc_data := parser.encode_data(p, proc_stmt)
	
	return parser.add_node(p, parser.Node{.Procedure, proc_data, token})
}
