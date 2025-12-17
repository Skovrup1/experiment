package parser

import "../lexer"

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"

NodeKind :: enum u8 {
	Invalid = 0,
	Module,
	Identifier,
	True,
	False,
	Integer,
	Float,
	Block,
	Variable,
	Parameter,
	Procedure,
	If,
	For,
	Return,
	ExprStmt,
	Call,
	Assignment,
	Addition,
	Multiplication,
	Equal,
	Less,
}

ModuleDecl :: struct {
	statements: []NodeIndex,
}

VarDecl :: struct {
	type:  NodeIndex,
	value: NodeIndex,
}

BlockStmt :: struct {
	statements: []NodeIndex,
}

ProcDecl :: struct {
	name:        lexer.TokenIndex,
	return_type: NodeIndex,
	body:        NodeIndex,
	parameters:  []NodeIndex,
}

ParamDecl :: struct {
	type:  NodeIndex,
	value: NodeIndex,
}

IfExpr :: struct {
	condition: NodeIndex,
	then_body: NodeIndex,
	else_body: NodeIndex,
}

ForStmt :: struct {
	initial:   NodeIndex,
	condition: NodeIndex,
	update:    NodeIndex,
	body:      NodeIndex,
}

ReturnStmt :: struct {
	value: NodeIndex,
}

ExprStmt :: struct {
	inner: NodeIndex,
}

CallExpr :: struct {
	callee:    NodeIndex,
	arguments: []NodeIndex,
}

AssignExpr :: struct {
	left:  NodeIndex,
	right: NodeIndex,
}

MulExpr :: struct {
	left:  NodeIndex,
	right: NodeIndex,
}

AddExpr :: struct {
	left:  NodeIndex,
	right: NodeIndex,
}

EqualExpr :: struct {
	left:  NodeIndex,
	right: NodeIndex,
}

LessExpr :: struct {
	left:  NodeIndex,
	right: NodeIndex,
}

encode_data :: proc(data_buf: ^[dynamic]u32, data: $T) -> DataIndex {
	data := data
	start := DataIndex(len(data_buf))
	length := size_of(T) / size_of(u32)

	slice := mem.slice_ptr(cast(^u32)&data, length)
	append(data_buf, ..slice)

	return start
}

decode_data :: proc(data_buf: []u32, index: DataIndex, $T: typeid) -> T {
	length := size_of(T) / size_of(u32)
	assert(int(index) + length <= len(data_buf), "decode index out of bounds")

	slice := data_buf[int(index):int(index) + length]
	return (cast(^T)raw_data(slice))^
}

DataIndex :: distinct u32
INVALID_DATA :: max(DataIndex)

Node :: struct #packed {
	kind:  NodeKind,
	data:  DataIndex,
	token: lexer.TokenIndex,
}

NodeIndex :: distinct u32
INVALID_NODE :: max(NodeIndex)

ParserError :: struct {
	message:  string,
	position: lexer.TokenIndex,
}

MAX_PARSER_ERRORS :: 8

Parser :: struct {
	source:    string,
	tokens:    []lexer.Token,
	nodes:     [dynamic]Node,
	data:      [dynamic]u32,
	errors:    [dynamic]ParserError,
	cursor:    lexer.TokenIndex,
	lookahead: lexer.TokenIndex,
}

make_parser :: proc(source: string, tokens: []lexer.Token) -> Parser {
	nodes := make([dynamic]Node, 0, len(tokens))
	data := make([dynamic]u32, 0, len(tokens))
	errors := make([dynamic]ParserError)
	cursor :: 0
	lookahead :: 1
	return Parser{source, tokens, nodes, data, errors, cursor, lookahead}
}

next :: proc(p: ^Parser) -> lexer.TokenIndex {
	p.cursor = p.lookahead
	p.lookahead += 1

	return p.cursor
}

peek :: proc(p: ^Parser) -> lexer.TokenKind {
	return p.tokens[p.cursor].kind
}

peek_next :: proc(p: ^Parser) -> lexer.TokenKind {
	return p.tokens[p.lookahead].kind
}

expect :: proc(p: ^Parser, kind: lexer.TokenKind, loc := #caller_location) {
	if peek(p) == kind {
		next(p)
	} else {
		add_error(p, fmt.tprintf("expected %v, got %v", kind, peek(p)), p.cursor)
	}
}

allow :: proc(p: ^Parser, kind: lexer.TokenKind) -> bool {
	if peek(p) == kind {
		next(p)
		return true
	} else {
		return false
	}
}

starts_expr :: proc(p: ^Parser) -> bool {
	#partial switch peek(p) {
	case .Identifier, .True, .False, .Integer, .Float, .LeftParen:
		return true
	case:
		return false
	}
}

add_node :: proc(p: ^Parser, node: Node) -> NodeIndex {
	append(&p.nodes, node)
	return NodeIndex(len(p.nodes) - 1)
}

add_error :: proc(p: ^Parser, message: string, position: lexer.TokenIndex) {
	error := ParserError{message, position}
	append(&p.errors, error)
}

parse_atom :: proc(p: ^Parser) -> NodeIndex {
	#partial switch peek(p) {
	case .Identifier:
		token := p.cursor
		expect(p, .Identifier)
		return add_node(p, Node{.Identifier, INVALID_DATA, token})
	case .True:
		token := p.cursor
		expect(p, .True)
		return add_node(p, Node{.True, INVALID_DATA, token})
	case .False:
		token := p.cursor
		expect(p, .False)
		return add_node(p, Node{.False, INVALID_DATA, token})
	case .Integer:
		token := p.cursor
		expect(p, .Integer)
		return add_node(p, Node{.Integer, INVALID_DATA, token})
	case .Float:
		token := p.cursor
		expect(p, .Float)
		return add_node(p, Node{.Float, INVALID_DATA, token})
	case .LeftParen:
		next(p)
		expr := parse_expr(p)
		expect(p, .RightParen)
		return expr
	}

	add_error(p, fmt.tprintf("unexpected token: %v", peek(p)), p.cursor)
	return INVALID_NODE
}

postfix_prec :: proc(kind: lexer.TokenKind) -> int {
	#partial switch kind {
	case .LeftParen:
		return 7
	case:
		return 0
	}
}

is_binary_op :: proc(kind: lexer.TokenKind) -> bool {
	#partial switch kind {
	case .Equal, .Plus, .Asterisk, .EqualEqual, .Less:
		return true
	case:
		return false
	}
}

infix_prec :: proc(kind: lexer.TokenKind) -> int {
	#partial switch kind {
	case .Equal:
		return 1
	case .EqualEqual, .Less:
		return 2
	case .Plus:
		return 3
	case .Asterisk:
		return 4
	case:
		return 0
	}
}

is_right_associative :: proc(kind: lexer.TokenKind) -> bool {
	#partial switch kind {
	case .Equal:
		return true
	case:
		return false
	}
}

parse_expr :: proc(p: ^Parser, min_prec := 0) -> NodeIndex {
	left := parse_atom(p)

	for postfix_prec(peek(p)) > min_prec {
		#partial switch peek(p) {
		case .LeftParen:
			token := p.cursor
			next(p)
			args := make([dynamic]NodeIndex)
			if !allow(p, .RightParen) {
				append(&args, parse_expr(p))
				for allow(p, .Comma) {
					append(&args, parse_expr(p))
				}
				expect(p, .RightParen)
			}
			call_expr := CallExpr{left, args[:]}
			data := encode_data(&p.data, call_expr)
			left = add_node(p, Node{.Call, data, token})
		case:
			add_error(p, fmt.tprintf("unexpected postfix operator: %v", peek(p)), p.cursor)
		}
	}

	// is this is_binary_op call necessary?
	for is_binary_op(peek(p)) && infix_prec(peek(p)) >= min_prec {
		token := p.cursor
		op_kind := peek(p)
		prec := infix_prec(op_kind)

		next_min_prec: int
		if is_right_associative(op_kind) {
			next_min_prec = prec
		} else {
			next_min_prec = prec + 1
		}

		next(p)
		right := parse_expr(p, next_min_prec)

		#partial switch op_kind {
		case .Equal:
			assign_expr := AssignExpr{left, right}
			data := encode_data(&p.data, assign_expr)
			left = add_node(p, Node{.Assignment, data, token})
		case .Plus:
			add_expr := AddExpr{left, right}
			data := encode_data(&p.data, add_expr)
			left = add_node(p, Node{.Addition, data, token})
		case .Asterisk:
			mul_expr := MulExpr{left, right}
			data := encode_data(&p.data, mul_expr)
			left = add_node(p, Node{.Multiplication, data, token})
		case .EqualEqual:
			equal_expr := EqualExpr{left, right}
			data := encode_data(&p.data, equal_expr)
			left = add_node(p, Node{.Equal, data, token})
		case .Less:
			less_expr := LessExpr{left, right}
			data := encode_data(&p.data, less_expr)
			left = add_node(p, Node{.Less, data, token})
		case:
			for node in p.nodes {
				fmt.println("NODE: %v", node)
			}
			add_error(p, fmt.tprintf("unexpected binary operator: %v", op_kind), p.cursor)
		}
	}

	return left
}

parse_type :: proc(p: ^Parser) -> NodeIndex {
	#partial switch peek(p) {
	case .Identifier:
		token := p.cursor
		next(p)
		return add_node(p, Node{.Identifier, INVALID_DATA, token})
	case:
		add_error(p, fmt.tprintf("unexpected type token: %v", peek(p)), p.cursor)
		return INVALID_NODE
	}
}

parse_param :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	expect(p, .Identifier)
	expect(p, .Colon)
	type := parse_type(p)

	param_stmt := ParamDecl{type, INVALID_NODE}
	data := encode_data(&p.data, param_stmt)

	return add_node(p, Node{.Parameter, data, token})
}

parse_toplevel :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor

	#partial switch peek(p) {
	case .Identifier:
		#partial switch peek_next(p) {
		case .Colon:
			next(p)
			next(p)
			type := parse_type(p)
			expect(p, .Equal)
			value := parse_expr(p)
			expect(p, .Semicolon)

			var_stmt := VarDecl{INVALID_NODE, value}
			data := encode_data(&p.data, var_stmt)

			return add_node(p, Node{.Variable, data, token})
		case .ColonEqual:
			next(p)
			next(p)
			value := parse_expr(p)
			expect(p, .Semicolon)

			var_stmt := VarDecl{INVALID_NODE, value}
			data := encode_data(&p.data, var_stmt)

			return add_node(p, Node{.Variable, data, token})
		case .ColonColon:
			next(p)
			next(p)

			if peek(p) == .LeftParen {
				next(p)

				parameters := make([dynamic]NodeIndex)
				if peek(p) != .RightParen {
					append(&parameters, parse_param(p))
					for allow(p, .Comma) {
						append(&parameters, parse_param(p))
					}
				}

				expect(p, .RightParen)

				return_type := INVALID_NODE
				if allow(p, .Arrow) {
					return_type = parse_type(p)
				}

				body := parse_toplevel(p)

				proc_stmt := ProcDecl{token, return_type, body, parameters[:]}
				data := encode_data(&p.data, proc_stmt)

				return add_node(p, Node{.Procedure, data, token})
			} else {
				value := parse_expr(p)
				expect(p, .Semicolon)

				var_stmt := VarDecl{INVALID_NODE, value}
				data := encode_data(&p.data, var_stmt)

				return add_node(p, Node{.Variable, data, token})
			}
		case:
			// expression statement
			expression := parse_expr(p)
			expect(p, .Semicolon)

			expr_stmt := ExprStmt{expression}
			data := encode_data(&p.data, expr_stmt)

			return add_node(p, Node{.ExprStmt, data, token})
		}
	case .If:
		next(p)

		condition := parse_expr(p)
		then_body := parse_toplevel(p)

		else_body := INVALID_NODE
		if allow(p, .Else) {
			else_body = parse_toplevel(p)
		}

		if_expr := IfExpr{condition, then_body, else_body}
		data := encode_data(&p.data, if_expr)

		return add_node(p, Node{.If, data, token})
	case .For:
		next(p)
		initial := parse_toplevel(p)
		condition := parse_expr(p)
		expect(p, .Semicolon)
		update := parse_expr(p)
		body := parse_toplevel(p)

		for_stmt := ForStmt{initial, condition, update, body}
		data := encode_data(&p.data, for_stmt)

		return add_node(p, Node{.For, data, token})
	case .Return:
		next(p)
		value := INVALID_NODE
		if starts_expr(p) {
			value = parse_expr(p)
		}
		expect(p, .Semicolon)

		return_stmt := ReturnStmt{value}
		data := encode_data(&p.data, return_stmt)

		return add_node(p, Node{.Return, data, token})
	case .LeftBrace:
		next(p)
		stmts := make([dynamic]NodeIndex)
		for peek(p) != .RightBrace && peek(p) != .Eof {
			append(&stmts, parse_toplevel(p))
		}
		expect(p, .RightBrace)

		block_stmt := BlockStmt{stmts[:]}
		data := encode_data(&p.data, block_stmt)

		return add_node(p, Node{.Block, data, token})
	case:
		// expression statement
		expression := parse_expr(p)
		expect(p, .Semicolon)

		expr_stmt := ExprStmt{expression}
		data := encode_data(&p.data, expr_stmt)

		return add_node(p, Node{.ExprStmt, data, token})
	}

	return INVALID_NODE
}

parse :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor

	stmts := make([dynamic]NodeIndex)
	for peek(p) != .Eof && len(p.errors) < MAX_PARSER_ERRORS {
		append(&stmts, parse_toplevel(p))
	}

	module_decl := ModuleDecl{stmts[:]}
	data := encode_data(&p.data, module_decl)

	return add_node(p, Node{.Module, data, token})
}

program_to_string :: proc(p: ^Parser) -> string {
	builder := strings.builder_make()

	print_node :: proc(
		p: ^Parser,
		b: ^strings.Builder,
		index: NodeIndex,
		indent := 0,
		parent_prec := 0,
		omit_stmt_suffix := false,
		emit_indent := true,
	) {
		print_indent :: proc(builder: ^strings.Builder, indent: int) {
			for _ in 0 ..< indent {
				strings.write_byte(builder, ' ')
			}
		}

		if index == INVALID_NODE {
			if emit_indent {
				print_indent(b, indent)
			}
			strings.write_string(b, "invalid node")
			return
		}

		next_indent := indent + 4

		node := p.nodes[index]
		token := p.tokens[node.token]
		node_prec := infix_prec(token.kind) if infix_prec(token.kind) != 0 else 99
		needs_parens := node_prec < parent_prec

		if needs_parens {
			strings.write_byte(b, '(')
		}

		#partial switch node.kind {
		case .Identifier:
			token := p.tokens[node.token]
			strings.write_string(b, p.source[token.start:token.end])
		case .True, .False:
			token := p.tokens[node.token]
			strings.write_string(b, p.source[token.start:token.end])
		case .Integer:
			token := p.tokens[node.token]
			strings.write_string(b, p.source[token.start:token.end])
		case .Float:
			token := p.tokens[node.token]
			strings.write_string(b, p.source[token.start:token.end])
		case .Variable:
			var_stmt := decode_data(p.data[:], node.data, VarDecl)
			token := p.tokens[node.token]
			if emit_indent {
				print_indent(b, indent)
			}
			strings.write_string(b, p.source[token.start:token.end])
			strings.write_string(b, " := ")
			print_node(p, b, var_stmt.value)
			if !omit_stmt_suffix {
				strings.write_string(b, "; ")
			}
		case .Block:
			strings.write_string(b, " {")
			block_stmt := decode_data(p.data[:], node.data, BlockStmt)
			for stmt in block_stmt.statements {
				strings.write_rune(b, '\n')
				print_node(p, b, stmt, next_indent)
			}
			if len(block_stmt.statements) > 0 {
				strings.write_rune(b, '\n')
			    print_indent(b, indent)
			}
			strings.write_string(b, "} ")
		case .Parameter:
			param_stmt := decode_data(p.data[:], node.data, ParamDecl)
			token := p.tokens[node.token]
			strings.write_string(b, p.source[token.start:token.end])
			strings.write_string(b, ": ")
			print_node(p, b, param_stmt.type)
		case .Procedure:
			proc_stmt := decode_data(p.data[:], node.data, ProcDecl)
			if emit_indent {
				print_indent(b, indent)
			}
			id_token := p.tokens[proc_stmt.name]
			strings.write_string(b, p.source[id_token.start:id_token.end])
			strings.write_string(b, " :: (")
			for param, i in proc_stmt.parameters {
				if i > 0 do strings.write_string(b, ", ")
				print_node(p, b, param)
			}
			strings.write_string(b, ")")
			if proc_stmt.return_type != INVALID_NODE {
				strings.write_string(b, " -> ")
				print_node(p, b, proc_stmt.return_type)
			}
			print_node(p, b, proc_stmt.body, indent)
		case .If:
			if_expr := decode_data(p.data[:], node.data, IfExpr)
			if emit_indent {
				print_indent(b, indent)
			}
			strings.write_string(b, "if ")
			print_node(p, b, if_expr.condition)
			print_node(p, b, if_expr.then_body, indent)
			if if_expr.else_body != INVALID_NODE {
				strings.write_string(b, "else")
				print_node(p, b, if_expr.else_body, indent)
			}
		case .For:
			for_stmt := decode_data(p.data[:], node.data, ForStmt)
			if emit_indent {
				print_indent(b, indent)
			}
			strings.write_string(b, "for ")
			print_node(p, b, for_stmt.initial, indent, parent_prec, true, false)
			strings.write_string(b, "; ")
			print_node(p, b, for_stmt.condition)
			strings.write_string(b, "; ")
			print_node(p, b, for_stmt.update, indent, parent_prec, true, false)
			print_node(p, b, for_stmt.body, indent)
		case .Return:
			return_stmt := decode_data(p.data[:], node.data, ReturnStmt)
			if emit_indent {
				print_indent(b, indent)
			}
			strings.write_string(b, "return")
			if return_stmt.value != INVALID_NODE {
				strings.write_string(b, " ")
				print_node(p, b, return_stmt.value)
			}
			if !omit_stmt_suffix {
				strings.write_string(b, "; ")
			}
		case .ExprStmt:
			expr_stmt := decode_data(p.data[:], node.data, ExprStmt)
			if emit_indent {
				print_indent(b, indent)
			}
			print_node(p, b, expr_stmt.inner)
			if !omit_stmt_suffix {
				strings.write_string(b, "; ")
			}
		case .Module:
			module_decl := decode_data(p.data[:], node.data, ModuleDecl)
			for stmt, i in module_decl.statements {
				if i > 0 do strings.write_rune(b, '\n')
				print_node(p, b, stmt, indent)
			}
		case .Call:
			call_expr := decode_data(p.data[:], node.data, CallExpr)
			print_node(p, b, call_expr.callee)
			strings.write_byte(b, '(')
			for arg, i in call_expr.arguments {
				if i > 0 do strings.write_string(b, ", ")
				print_node(p, b, arg)
			}
			strings.write_byte(b, ')')
		case .Assignment:
			assign_expr := decode_data(p.data[:], node.data, AssignExpr)
			print_node(p, b, assign_expr.left, indent, node_prec + 1)
			strings.write_string(b, " = ")
			print_node(p, b, assign_expr.right, indent, node_prec)
		case .Addition:
			add_expr := decode_data(p.data[:], node.data, AddExpr)
			print_node(p, b, add_expr.left, indent, node_prec)
			strings.write_string(b, " + ")
			print_node(p, b, add_expr.right, indent, node_prec + 1)
		case .Multiplication:
			mul_expr := decode_data(p.data[:], node.data, MulExpr)
			print_node(p, b, mul_expr.left, indent, node_prec)
			strings.write_string(b, " * ")
			print_node(p, b, mul_expr.right, indent, node_prec + 1)
		case .Equal:
			less_expr := decode_data(p.data[:], node.data, LessExpr)
			print_node(p, b, less_expr.left, indent, node_prec)
			strings.write_string(b, " == ")
			print_node(p, b, less_expr.right, indent, node_prec + 1)
		case .Less:
			less_expr := decode_data(p.data[:], node.data, LessExpr)
			print_node(p, b, less_expr.left, indent, node_prec)
			strings.write_string(b, " < ")
			print_node(p, b, less_expr.right, indent, node_prec + 1)
		}

		if needs_parens {
			strings.write_byte(b, ')')
		}
	}

	print_node(p, &builder, NodeIndex(len(p.nodes) - 1))
	return strings.to_string(builder)
}
