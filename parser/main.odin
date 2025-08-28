package parser

import "../scanner"

import "core:fmt"
import "core:strconv"

Module :: struct {
	token: scanner.TokenIndex,
	nodes: []NodeIndex,
}

IdentLit :: struct {
	token: scanner.TokenIndex,
}

IntLit :: struct {
	token: scanner.TokenIndex,
}

VarDecl :: struct {
	token: scanner.TokenIndex,
	expr:  NodeIndex,
}

FuncDecl :: struct {
	token:  scanner.TokenIndex,
	params: []NodeIndex,
	body:   NodeIndex,
}

ExprStmt :: struct {
	token: scanner.TokenIndex,
	expr:  NodeIndex,
}

BlockStmt :: struct {
	token: scanner.TokenIndex,
	stmts: []NodeIndex,
}

ReturnStmt :: struct {
	token: scanner.TokenIndex,
	expr:  NodeIndex,
}

IfStmt :: struct {
	token: scanner.TokenIndex,
	cond:  NodeIndex,
	then:  NodeIndex,
	else_: NodeIndex,
}

ForStmt :: struct {
	token: scanner.TokenIndex,
	init:  NodeIndex,
	cond:  NodeIndex,
	incr:  NodeIndex,
	body:  NodeIndex,
}

WhileStmt :: struct {
	token: scanner.TokenIndex,
	cond:  NodeIndex,
	body:  NodeIndex,
}

CallExpr :: struct {
	token: scanner.TokenIndex,
	ident: scanner.TokenIndex,
	args:  []NodeIndex,
}

UnaryExpr :: struct {
	token: scanner.TokenIndex,
	expr:  NodeIndex,
}

BinaryExpr :: struct {
	token: scanner.TokenIndex,
	left:  NodeIndex,
	right: NodeIndex,
}

Node :: union {
	Module,
	IdentLit,
	//StringLit,
	//RealLit,
	//BoolLit,
	IntLit,
	FuncDecl,
	VarDecl,
	ExprStmt,
	BlockStmt,
	ReturnStmt,
	IfStmt,
	ForStmt,
	WhileStmt,
	//BreakStmt,
	//ContinueStmt,
	CallExpr,
	//MemberExpr,
	//RefExpr,
	//AccessExpr,
	UnaryExpr,
	BinaryExpr,
}

NodeIndex :: distinct u32
INVALID_NODE :: max(NodeIndex)

Parser :: struct {
	source:    []u8,
	scan:      scanner.Scanner,
	tokens:    [dynamic]scanner.Token,
	nodes:     [dynamic]Node,
	cursor:    scanner.TokenIndex,
	lookahead: scanner.TokenIndex,
}

make_parser :: proc(source: []u8) -> Parser {
	scan := scanner.make_scanner(source)
	tokens := make([dynamic]scanner.Token)
	nodes := make([dynamic]Node)

	append(&tokens, scanner.next_token(&scan))
	cursor := scanner.TokenIndex(len(tokens) - 1)
	append(&tokens, scanner.next_token(&scan))
	lookahead := scanner.TokenIndex(len(tokens) - 1)

	return Parser{source, scan, tokens, nodes, cursor, lookahead}
}

next_token :: proc(p: ^Parser) -> scanner.TokenIndex {
	tok := scanner.next_token(&p.scan)

	if tok.kind == .Error {
		panic("error")
	}

	append(&p.tokens, tok)
	return scanner.TokenIndex(len(p.tokens) - 1)
}

add_node :: proc(p: ^Parser, node: Node) -> NodeIndex {
	append(&p.nodes, node)
	return NodeIndex(len(p.nodes) - 1)
}

parse :: proc(p: ^Parser) -> []Node {
	#partial switch peek(p) {
	case .Module:
		token := p.cursor
		next(p)
		expect(p, .Identifier)
		expect(p, .LBrace)

		stmts := make([dynamic]NodeIndex, 0, 1)
		for peek(p) != .RBrace {
			append(&stmts, parse_decl_or_stmt(p))
		}
		next(p)
		add_node(p, Module{token, stmts[:]})
	case:
		panic(fmt.tprintf("invalid module: %v", peek(p)))
	}

	return p.nodes[:]
}

parse_atom :: proc(p: ^Parser) -> NodeIndex {
	atom := peek(p)
	#partial switch atom {
	case .Identifier:
		token := p.cursor
		next(p)
		return add_node(p, IdentLit{token})
	case .String:
		panic("todo")
	case .Real:
		panic("todo")
	case .Integer:
		token := p.cursor
		next(p)
		return add_node(p, IntLit{token})
	case .True, .False:
		panic("todo")
	case .Minus, .Not:
		token := p.cursor
		next(p)
		expr := parse_expr(p, prefix_prec(atom))
		return add_node(p, UnaryExpr{token, expr})
	case .LParen:
		panic("todo")
	case:
		panic(fmt.tprintf("invalid atom: %v", atom))
	}
}

prefix_prec :: proc(op: scanner.TokenKind) -> int {
	#partial switch op {
	case .Minus, .Not:
		return 6
	case:
		return 0
	}
}

infix_prec :: proc(op: scanner.TokenKind) -> int {
	#partial switch op {
	case .Assign,
	     .PlusEqual,
	     .MinusEqual,
	     .MulEqual,
	     .DivEqual,
	     .ModEqual,
	     .AmpersandEqual,
	     .PipeEqual,
	     .TildeEqual,
	     .LShiftEqual,
	     .RShiftEqual:
		return 0
	case .Or:
		return 1
	case .And:
		return 2
	case .Equal, .NotEqual, .Less, .LessEqual, .Greater, .GreaterEqual:
		return 3
	case .Plus, .Minus, .Pipe, .Tilde:
		return 4
	case .Mul, .Div, .Mod, .Ampersand, .LShift, .RShift:
		return 5
	case .Hat:
		return 7
	case .Period:
		return 8
	case:
		return 0
	}
}

postfix_prec :: proc(op: scanner.TokenKind) -> int {
	#partial switch op {
	case .LBracket, .LParen:
		return 8
	case:
		return 0
	}
}

is_binary_op :: proc(op: scanner.TokenKind) -> bool {
	#partial switch op {
	case .Plus,
	     .Minus,
	     .Mul,
	     .Div,
	     .Mod,
	     .Equal,
	     .NotEqual,
	     .Less,
	     .LessEqual,
	     .Greater,
	     .GreaterEqual,
	     .Or,
	     .And,
	     .Pipe,
	     .Tilde,
	     .Hat,
	     .Ampersand,
	     .LShift,
	     .RShift,
	     .Period,
	     .Assign,
	     .PlusEqual,
	     .MinusEqual,
	     .MulEqual,
	     .DivEqual,
	     .ModEqual,
	     .AmpersandEqual,
	     .PipeEqual,
	     .TildeEqual,
	     .LShiftEqual,
	     .RShiftEqual:
		return true
	case:
		return false
	}
}

is_right_assoc :: proc(op: scanner.TokenKind) -> bool {
	#partial switch op {
	case .Assign,
	     .PlusEqual,
	     .MinusEqual,
	     .MulEqual,
	     .DivEqual,
	     .ModEqual,
	     .AmpersandEqual,
	     .PipeEqual,
	     .TildeEqual,
	     .LShiftEqual,
	     .RShiftEqual,
	     .Hat:
		return true
	case:
		return false
	}
}

parse_expr :: proc(p: ^Parser, min_prec := 0) -> NodeIndex {
	left := parse_atom(p)
	op := peek(p)
	prec := infix_prec(op)

	for is_binary_op(op) && prec >= min_prec {
		token := p.cursor
		next(p)

		next_prec := prec if is_right_assoc(op) else prec + 1

		right := parse_expr(p, next_prec)
		left = add_node(p, BinaryExpr{token, left, right})

		op = peek(p)
	}

	return left
}

parse_block_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	next(p)
	stmts := make([dynamic]NodeIndex, 0, 1)
	for peek(p) != .RBrace {
		append(&stmts, parse_decl_or_stmt(p))
	}
	expect(p, .RBrace)
	return add_node(p, BlockStmt{token, stmts[:]})
}

parse_return_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	next(p)
	expr := parse_expr(p)
	expect(p, .Semicolon)
	return add_node(p, ReturnStmt{token, expr})
}

parse_if_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	next(p)
	cond := parse_expr(p)
	then := parse_decl_or_stmt(p)
	else_ := INVALID_NODE
	if allow(p, .Else) {
		else_ = parse_decl_or_stmt(p)
	}
	return add_node(p, IfStmt{token, cond, then, else_})
}


parse_for_init :: proc(p: ^Parser) -> NodeIndex {
	// This is a special case for for loop initializers.
	// It can be a variable declaration or a simple expression.
	// It does NOT consume a trailing semicolon.
	if peek(p) == .Identifier && peek_next(p) == .Var {
		token := p.cursor
		next(p) // consume identifier
		next(p) // consume Var `:=`
		expr := parse_expr(p)
		return add_node(p, VarDecl{token, expr})
	}

	// Otherwise, it's just a simple expression.
	return parse_expr(p)
}

parse_for_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	next(p)

	// 1. Parse optional initializer.
	// If the next token is not a semicolon, we have an initializer.
	init: NodeIndex = INVALID_NODE
	if peek(p) != .Semicolon {
		init = parse_for_init(p)
	}

	// 2. Expect the first semicolon.
	expect(p, .Semicolon)

	// 3. Parse optional condition.
	cond: NodeIndex = INVALID_NODE
	if peek(p) != .Semicolon {
		cond = parse_expr(p)
	}

	// 4. Expect the second semicolon.
	expect(p, .Semicolon)

	// 5. Parse optional increment expression.
	// If the next token is not the start of the body block, we have an incrementer.
	incr: NodeIndex = INVALID_NODE
	if peek(p) != .LBrace {
		incr = parse_expr(p)
	}

	// 6. Parse the mandatory body.
	body := parse_decl_or_stmt(p)

	return add_node(p, ForStmt{token, init, cond, incr, body})
}

parse_while_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	next(p)
	cond := parse_expr(p)
	body := parse_decl_or_stmt(p)
	return add_node(p, WhileStmt{token, cond, body})
}

parse_func_decl :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	expect(p, .LParen)
	params := make([dynamic]NodeIndex)
	if peek(p) != .RParen {
		for {
			append(&params, parse_expr(p))
			if peek(p) == .Comma {
				next(p)
			} else {
				break
			}
		}
	}
	next(p)

	expect(p, .Minus)
	expect(p, .Greater)
	expect(p, .I32)

	body := parse_decl_or_stmt(p)

	return add_node(p, FuncDecl{token, params[:], body})
}

parse_var_decl :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	expect(p, .Var)
	expr := parse_expr(p)
	expect(p, .Semicolon)

	return add_node(p, VarDecl{token, expr})
}

parse_decl_or_stmt :: proc(p: ^Parser) -> NodeIndex {
	#partial switch peek(p) {
	case .Identifier:
		#partial switch peek_next(p) {
		case .Const:
			token := p.cursor
			next(p)
			next(p)
			return parse_func_decl(p, token)
		case .Var:
			token := p.cursor
			next(p)
			return parse_var_decl(p, token)
		case:
			// It's an expression statement that starts with an identifier
			expr := parse_expr(p)
			expect(p, .Semicolon)
			return add_node(p, ExprStmt{p.cursor, expr})
		}
	case .LBrace:
		return parse_block_stmt(p)
	case .Return:
		return parse_return_stmt(p)
	case .If:
		return parse_if_stmt(p)
	case .For:
		return parse_for_stmt(p)
	case .While:
		return parse_while_stmt(p)
	case .Break:
		panic("todo")
	case .Continue:
		panic("todo")
	case:
		// assume it must be expr stmt
		expr := parse_expr(p)
		expect(p, .Semicolon)
		return add_node(p, ExprStmt{666, expr})
	}
}

skip_comments :: proc(p: ^Parser) {
	for peek(p) == .LineComment || peek(p) == .BlockComment {
		tok := next_token(p)
		p.cursor = p.lookahead
		p.lookahead = tok
	}
}

next :: proc(p: ^Parser) -> scanner.TokenIndex {
	tok := next_token(p)
	p.cursor = p.lookahead
    p.lookahead = tok

    skip_comments(p)

	return p.cursor
}

peek :: proc(p: ^Parser) -> scanner.TokenKind {
	return p.tokens[p.cursor].kind
}

peek_next :: proc(p: ^Parser) -> scanner.TokenKind {
	return p.tokens[p.lookahead].kind
}

expect :: proc(p: ^Parser, expected: scanner.TokenKind, loc := #caller_location) {
	if peek(p) != expected {
		panic(fmt.tprintf("expected token %v, but got %v, at %v", expected, peek(p), loc))
	}

	next(p)
}

allow :: proc(p: ^Parser, allowed: scanner.TokenKind, loc := #caller_location) -> bool {
	if peek(p) == allowed {
		next(p)
		return true
	}

	return false
}

token_to_string :: proc(p: ^Parser, token: scanner.TokenIndex) -> string {
	token := p.tokens[token]
	start := token.start
	end := token.end
	return string(p.source[start:end])
}

token_to_int :: proc(p: ^Parser, token: scanner.TokenIndex) -> int {
	value_str := token_to_string(p, token)
	value := strconv.atoi(value_str)
	return value
}

print_tree :: proc(p: ^Parser) {
	print_node :: proc(p: ^Parser, index: NodeIndex, indent := 0) {
		print_indent :: proc(indent: int) {
			for _ in 0 ..< indent {
				fmt.print("  ")
			}
		}

		if index == INVALID_NODE {
			return
		}

		node := p.nodes[index]
		switch v in node {
		case Module:
			print_indent(indent)
			fmt.println("Module")
			for node in v.nodes {
				print_node(p, node, indent + 2)
			}
		case IdentLit:
			print_indent(indent)
			fmt.println("IdentLit:", token_to_string(p, v.token))
		case IntLit:
			print_indent(indent)
			fmt.println("IntLit:", token_to_int(p, v.token))
		case FuncDecl:
			print_indent(indent)
			fmt.println("FuncDecl:", token_to_string(p, v.token))
			for param in v.params {
				print_node(p, param, indent + 2)
			}
			print_node(p, v.body, indent + 2)
		case VarDecl:
			print_indent(indent)
			fmt.println("VarDecl:", token_to_string(p, v.token))
			print_node(p, v.expr, indent + 2)
		case ExprStmt:
			print_indent(indent)
			fmt.println("ExprStmt")
			print_node(p, v.expr, indent + 2)
		case BlockStmt:
			print_indent(indent)
			fmt.println("BlockStmt")
			for stmt in v.stmts {
				print_node(p, stmt, indent + 2)
			}
		case ReturnStmt:
			print_indent(indent)
			fmt.println("ReturnStmt")
			print_node(p, v.expr, indent + 2)
		case IfStmt:
			print_indent(indent)
			fmt.println("IfStmt")
			print_node(p, v.cond, indent + 2)
			print_node(p, v.then, indent + 2)
			print_node(p, v.else_, indent + 2)
		case ForStmt:
			print_indent(indent)
			fmt.println("ForStmt")
			print_node(p, v.init, indent + 2)
			print_node(p, v.cond, indent + 2)
			print_node(p, v.incr, indent + 2)
			print_node(p, v.body, indent + 2)
		case WhileStmt:
			print_indent(indent)
			fmt.println("WhileStmt")
			print_node(p, v.cond, indent + 2)
			print_node(p, v.body, indent + 2)
		case CallExpr:
			print_indent(indent)
			fmt.println("CallExpr:", token_to_string(p, v.ident))
			for arg in v.args {
				print_node(p, arg, indent + 2)
			}
		case BinaryExpr:
			print_indent(indent)
			fmt.println("BinaryExpr:", token_to_string(p, v.token))
			print_node(p, v.left, indent + 2)
			print_node(p, v.right, indent + 2)
		case UnaryExpr:
			print_indent(indent)
			fmt.println("UnaryExpr:", token_to_string(p, v.token))
			print_node(p, v.expr, indent + 2)
		}
	}

	print_node(p, NodeIndex(len(p.nodes) - 1))
}
