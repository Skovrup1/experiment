package parser

import "../scanner"

import "core:fmt"
import "core:strconv"

// Macro :: struct {}

Module :: struct {
	token: scanner.TokenIndex,
	nodes: []NodeIndex,
}

TypeSpec :: struct {
	token:  scanner.TokenIndex,
	is_ptr: bool,
}

IdentLit :: struct {
	token: scanner.TokenIndex,
}

StringLit :: struct {
	token: scanner.TokenIndex,
}

RealLit :: struct {
	token: scanner.TokenIndex,
}

IntLit :: struct {
	token: scanner.TokenIndex,
}

BoolLit :: struct {
	token: scanner.TokenIndex,
}

StructLit :: struct {
	token:  scanner.TokenIndex,
	values: []NodeIndex,
}

ArrayLit :: struct {
	token:  scanner.TokenIndex,
	values: []NodeIndex,
}

VarDecl :: struct {
	token: scanner.TokenIndex,
	type:  NodeIndex,
	expr:  NodeIndex,
}

ConstDecl :: struct {
	token: scanner.TokenIndex,
	type:  NodeIndex,
	expr:  NodeIndex,
}

ParamDecl :: struct {
	token: scanner.TokenIndex,
	type:  NodeIndex,
	expr:  NodeIndex,
}

MemberDecl :: struct {
	token: scanner.TokenIndex,
	type:  NodeIndex,
	expr:  NodeIndex,
}

ProcDecl :: struct {
	token:       scanner.TokenIndex,
	return_type: NodeIndex,
	body:        NodeIndex,
	params:      []NodeIndex,
}

StructDecl :: struct {
	token:   scanner.TokenIndex,
	members: []NodeIndex,
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

LoopStmt :: struct {
	token: scanner.TokenIndex,
	init:  NodeIndex,
	cond:  NodeIndex,
	incr:  NodeIndex,
	body:  NodeIndex,
}

BreakStmt :: struct {
	token: scanner.TokenIndex,
}

ContinueStmt :: struct {
	token: scanner.TokenIndex,
}

CallExpr :: struct {
	token:  scanner.TokenIndex,
	callee: NodeIndex,
	args:   []NodeIndex,
}

MemberExpr :: struct {
	token: scanner.TokenIndex,
	ident: NodeIndex,
	expr:  NodeIndex,
}

IndexExpr :: struct {
	token:  scanner.TokenIndex,
	base:   NodeIndex,
	offset: NodeIndex,
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
	TypeSpec,
	IdentLit,
	StringLit,
	RealLit,
	IntLit,
	BoolLit,
	StructLit,
	ArrayLit,
	VarDecl,
	ConstDecl,
	ParamDecl,
	MemberDecl,
	ProcDecl,
	StructDecl,
	ExprStmt,
	BlockStmt,
	ReturnStmt,
	IfStmt,
	LoopStmt,
	BreakStmt,
	ContinueStmt,
	CallExpr,
	MemberExpr,
	IndexExpr,
	UnaryExpr,
	BinaryExpr,
}

NodeIndex :: distinct u32
INVALID_NODE :: max(NodeIndex)

// note: change to flags
ParseMode :: enum {
	IgnoreComments,
	ParseComments,
}

Parser :: struct {
	source:    []u8,
	scan:      scanner.Scanner,
	tokens:    [dynamic]scanner.Token,
	nodes:     [dynamic]Node,
	cursor:    scanner.TokenIndex,
	lookahead: scanner.TokenIndex,
	errors:    i32,
	mode:      ParseMode,
}

make_parser :: proc(source: []u8) -> Parser {
	scan := scanner.make_scanner(source)
	tokens := make([dynamic]scanner.Token)
	nodes := make([dynamic]Node)

	parser := Parser{source, scan, tokens, nodes, 0, 0, 0, .IgnoreComments}

	next(&parser)
	next(&parser)

	return parser
}

skip_comments :: proc(p: ^Parser) {
	for peek(p) == .LineComment || peek(p) == .BlockComment {
		next(p)
	}
}

consume_comments :: proc(p: ^Parser) {
	if p.mode == .ParseComments {
		panic("todo: parse comments")
	} else {
		skip_comments(p)
	}
}

next :: proc(p: ^Parser) -> scanner.TokenIndex {
	tok := scanner.next_token(&p.scan)

	fmt.println(tok)

	if tok.kind == .Error {
		panic("error from scanner")
	}

	append(&p.tokens, tok)
	tok_index := scanner.TokenIndex(len(p.tokens) - 1)

	p.cursor = p.lookahead
	p.lookahead = tok_index

	consume_comments(p)

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

add_node :: proc(p: ^Parser, node: Node) -> NodeIndex {
	append(&p.nodes, node)
	return NodeIndex(len(p.nodes) - 1)
}

//parse_file_level :: proc() {
//}
parse :: proc(p: ^Parser) -> []Node {
	#partial switch peek(p) {
	case .Module:
		token := p.cursor
		next(p)
		expect(p, .Identifier)
		expect(p, .LBrace)

		stmts := make([dynamic]NodeIndex, 0, 1)
		for peek(p) != .RBrace && peek(p) != .Eof {
			append(&stmts, parse_decl_or_stmt(p))
		}
		expect(p, .RBrace)
		add_node(p, Module{token, stmts[:]})
	case:
		token := p.cursor
		stmts := make([dynamic]NodeIndex, 0, 1)

		for peek(p) != .Eof {
			append(&stmts, parse_decl_or_stmt(p))
		}
		add_node(p, Module{token, stmts[:]})
	}

	return p.nodes[:]
}

parse_type :: proc(p: ^Parser) -> NodeIndex {
	is_ptr := allow(p, .Mul)
	//is_array

	#partial switch peek(p) {
	case .Identifier:
		token := p.cursor
		next(p)
		return add_node(p, TypeSpec{token, is_ptr})
	case:
		panic(fmt.tprintf("invalid type identifier: %v", peek(p)))
	}
}

parse_struct_lit :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	values := make([dynamic]NodeIndex, 0, 1)
	for peek(p) != .RBrace {
		append(&values, parse_expr(p))
		if peek(p) == .Comma {
			next(p)
		} else {
			break
		}
	}
	next(p)

	return add_node(p, StructLit{token, values[:]})
}

parse_atom :: proc(p: ^Parser) -> NodeIndex {
	atom := peek(p)
	#partial switch atom {
	case .Identifier:
		token := p.cursor
		next(p)
		return add_node(p, IdentLit{token})
	case .StructLit:
		token := p.cursor
		next(p)
		return parse_struct_lit(p, token)
	case .String:
		token := p.cursor
		next(p)
		return add_node(p, StringLit{token})
	case .Real:
		token := p.cursor
		next(p)
		return add_node(p, RealLit{token})
	case .Integer:
		token := p.cursor
		next(p)
		return add_node(p, IntLit{token})
	case .True, .False:
		token := p.cursor
		next(p)
		return add_node(p, BoolLit{token})
	case .LBrace:
		token := p.cursor
		next(p)
		return parse_struct_lit(p, token)
	case .LBracket:
		token := p.cursor
		next(p)

		values := make([dynamic]NodeIndex, 0, 1)
		for peek(p) != .RBracket {
			append(&values, parse_expr(p))
			if peek(p) == .Comma {
				next(p)
			} else {
				break
			}
		}
		next(p)

		return add_node(p, ArrayLit{token, values[:]})
	// prefix unary expressions
	case .Minus, .Not, .Ampersand, .Tilde:
		token := p.cursor
		next(p)
		expr := parse_expr(p, prefix_prec(atom))
		return add_node(p, UnaryExpr{token, expr})
	case .LParen:
		next(p)
		inner_expr := parse_expr(p)
		expect(p, .RParen)
		return inner_expr
	case:
		panic(fmt.tprintf("invalid atom: %v", atom))
	}
}

prefix_prec :: proc(op: scanner.TokenKind) -> int {
	#partial switch op {
	case .Minus, .Not, .Tilde:
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
	case .LParen, .LBracket, .Period, .Deref:
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
	     .Power,
	     .Equal,
	     .NotEqual,
	     .Less,
	     .LessEqual,
	     .Greater,
	     .GreaterEqual,
	     .Or,
	     .And,
	     .Pipe,
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
	     .PowerEqual,
	     .AmpersandEqual,
	     .PipeEqual,
	     .HatEqual,
	     .TildeEqual,
	     .LShiftEqual,
	     .RShiftEqual:
		return true
	case:
		return false
	}
}

is_right_associative :: proc(op: scanner.TokenKind) -> bool {
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

	for postfix_prec(peek(p)) > min_prec {
		#partial switch peek(p) {
		case .LParen:
			token := p.cursor
			next(p)
			args := make([dynamic]NodeIndex, 0, 1)
			for peek(p) != .RParen {
				append(&args, parse_expr(p))
				if peek(p) == .Comma {
					next(p)
				} else {
					break
				}
			}
			expect(p, .RParen)
			left = add_node(p, CallExpr{token, left, args[:]})
		case .Period:
			token := p.cursor
			next(p)
			ident := parse_atom(p)
			left = add_node(p, MemberExpr{token, left, ident})
		case .Deref:
			token := p.cursor
			next(p)
			left = add_node(p, UnaryExpr{token, left})
		case .LBracket:
			token := p.cursor
			next(p)
			expr := parse_expr(p)
			expect(p, .RBracket)
			left = add_node(p, IndexExpr{token, left, expr})
		case:
			break
		}
	}

	for is_binary_op(peek(p)) && infix_prec(peek(p)) >= min_prec {
		token := p.cursor
		prec := infix_prec(peek(p))

		next_min_prec: int
		if is_right_associative(peek(p)) {
			next_min_prec = prec
		} else {
			next_min_prec = prec + 1
		}

		next(p)
		right := parse_expr(p, next_min_prec)

		left = add_node(p, BinaryExpr{token, left, right})
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
	no_expr := allow(p, .Semicolon)
	expr := INVALID_NODE
	if !no_expr {
		expr = parse_expr(p)
		expect(p, .Semicolon)
	}
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

parse_for_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	next(p)

	init := INVALID_NODE
	cond := INVALID_NODE
	incr := INVALID_NODE

	skip_all := peek(p) == .LBrace
	if !skip_all {
		if peek(p) != .Semicolon {
			if peek(p) == .Identifier && peek_next(p) == .Var {
				token := p.cursor
				next(p)
				next(p)
				expr := parse_expr(p)
				// panic("handle types")
				init = add_node(p, VarDecl{token, INVALID_NODE, expr})
			} else {
				init = parse_expr(p)
			}
		}
		expect(p, .Semicolon)

		if peek(p) != .Semicolon {
			cond = parse_expr(p)
		}
		expect(p, .Semicolon)

		if peek(p) != .LBrace {
			incr = parse_expr(p)
		}
	}

	body := parse_decl_or_stmt(p)

	return add_node(p, LoopStmt{token, init, cond, incr, body})
}

parse_param_decl :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	expect(p, .Identifier)

	#partial switch peek(p) {
	case .Var:
		next(p)
		expr := parse_expr(p)
		return add_node(p, ParamDecl{token, INVALID_NODE, expr})
	case .Colon:
		next(p)
		type := parse_type(p)
		expr := INVALID_NODE
		if allow(p, .Assign) {
			expr = parse_expr(p)
		}
		return add_node(p, ParamDecl{token, type, expr})
	case:
		panic(fmt.tprintf("invalid param decl: %v\n", peek(p)))
	}
}

parse_proc_decl :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	params := make([dynamic]NodeIndex)
	if peek(p) != .RParen {
		for {
			append(&params, parse_param_decl(p))
			if peek(p) == .Comma {
				next(p)
			} else {
				break
			}
		}
	}
	next(p)

	expect(p, .Arrow)
	return_type := parse_type(p)

	body := parse_decl_or_stmt(p)

	return add_node(p, ProcDecl{token, return_type, body, params[:]})
}

parse_member_decl :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	expect(p, .Identifier)

	#partial switch peek(p) {
	case .Var:
		next(p)
		expr := parse_expr(p)
		return add_node(p, ParamDecl{token, INVALID_NODE, expr})
	case .Colon:
		next(p)
		type := parse_type(p)
		expr := INVALID_NODE
		if allow(p, .Assign) {
			expr = parse_expr(p)
		}
		return add_node(p, MemberDecl{token, type, expr})
	case:
		panic(fmt.tprintf("invalid param decl: %v\n", peek(p)))
	}
}

parse_struct_decl :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	expect(p, .LBrace)

	members := make([dynamic]NodeIndex, 0, 1)
	for peek(p) != .RBrace {
		append(&members, parse_member_decl(p))
		if peek(p) == .Comma {
			next(p)
		} else {
			break
		}
	}
	next(p)

	return add_node(p, StructDecl{token, members[:]})
}

parse_const_decl :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	expr := parse_expr(p)
	expect(p, .Semicolon)

	return add_node(p, ConstDecl{token, INVALID_NODE, expr})
}

parse_var_decl :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	expr := parse_expr(p)
	expect(p, .Semicolon)

	return add_node(p, VarDecl{token, INVALID_NODE, expr})
}

parse_type_annotation :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	type := parse_type(p)

	#partial switch peek(p) {
	case .Assign:
		next(p)
		expr := parse_expr(p)
		expect(p, .Semicolon)
		return add_node(p, VarDecl{token, type, expr})
	case .Colon:
		next(p)
		expr := parse_expr(p)
		expect(p, .Semicolon)
		return add_node(p, ConstDecl{token, type, expr})
	case .Semicolon:
		next(p)
		return add_node(p, VarDecl{token, type, INVALID_NODE})
	case:
		panic(fmt.tprintf("invalid type annotation: %v", peek(p)))
	}
}

parse_decl_or_stmt :: proc(p: ^Parser) -> NodeIndex {
	#partial switch peek(p) {
	case .Identifier:
		#partial switch peek_next(p) {
		case .Const:
			token := p.cursor
			next(p)
			next(p)

			#partial switch peek(p) {
			case .LParen:
				next(p)
				return parse_proc_decl(p, token)
			case .Struct:
				next(p)
				return parse_struct_decl(p, token)
			case:
				return parse_const_decl(p, token)
			}
		case .Var:
			token := p.cursor
			next(p)
			next(p)
			return parse_var_decl(p, token)
		case .Colon:
			token := p.cursor
			next(p)
			next(p)
			return parse_type_annotation(p, token)
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
	case .Loop:
		return parse_for_stmt(p)
	case .Break:
		token := p.cursor
		next(p)
		expect(p, .Semicolon)
		return add_node(p, BreakStmt{token})
	case .Continue:
		token := p.cursor
		next(p)
		expect(p, .Semicolon)
		return add_node(p, BreakStmt{token})
	case:
		// assume it must be expr stmt
		token := p.cursor
		expr := parse_expr(p)
		expect(p, .Semicolon)
		return add_node(p, ExprStmt{token, expr})
	}
}

token_to_string :: proc(p: ^Parser, token: scanner.TokenIndex) -> string {
	token := p.tokens[token]
	start := token.start
	end := token.end
	return string(p.source[start:end])
}

token_to_float :: proc(p: ^Parser, token: scanner.TokenIndex) -> f64 {
	value_str := token_to_string(p, token)
	value := strconv.atof(value_str)
	return value
}

token_to_int :: proc(p: ^Parser, token: scanner.TokenIndex) -> int {
	value_str := token_to_string(p, token)
	value := strconv.atoi(value_str)
	return value
}

token_to_bool :: proc(p: ^Parser, token: scanner.TokenIndex) -> bool {
	value_str := token_to_string(p, token)
	if value_str == "true" {
		return true
	} else if value_str == "false" {
		return false
	} else {
		panic(fmt.tprintf("invalid can't convert token to bool: %v", value_str))
	}
}

print_tree :: proc(p: ^Parser) {
	print_node :: proc(p: ^Parser, index: NodeIndex, indent := 0) {
		print_indent :: proc(indent: int) {
			for _ in 0 ..< indent {
				fmt.print(" ")
			}
		}

		if index == INVALID_NODE {
			return
		}

		next_indent := indent + 2

		node := p.nodes[index]
		#partial switch v in node {
		case Module:
			print_indent(indent)
			fmt.println("Module")
			for node in v.nodes {
				print_node(p, node, next_indent)
			}
		case TypeSpec:
			print_indent(indent)
			fmt.print("TypeSpec: ")
			if v.is_ptr {
				fmt.print("*")
			}
			fmt.println(token_to_string(p, v.token))
		case IdentLit:
			print_indent(indent)
			fmt.println("IdentLit:", token_to_string(p, v.token))
		case StringLit:
			print_indent(indent)
			fmt.println("StringLit:", token_to_string(p, v.token))
		case RealLit:
			print_indent(indent)
			fmt.println("RealLit:", token_to_float(p, v.token))
		case IntLit:
			print_indent(indent)
			fmt.println("IntLit:", token_to_int(p, v.token))
		case BoolLit:
			print_indent(indent)
			fmt.println("BoolLit:", token_to_bool(p, v.token))
		case StructLit:
			print_indent(indent)
			fmt.println("StructLit:")
			for value in v.values {
				print_node(p, value, next_indent)
			}
		case ArrayLit:
			print_indent(indent)
			fmt.println("ArrayLit:")
			for value in v.values {
				print_node(p, value, next_indent)
			}
		case VarDecl:
			print_indent(indent)
			fmt.println("VarDecl:", token_to_string(p, v.token))
			if v.type != INVALID_NODE {
				print_node(p, v.type, next_indent)
			}
			print_node(p, v.expr, next_indent)
		case ConstDecl:
			print_indent(indent)
			fmt.println("ConstDecl:", token_to_string(p, v.token))
			if v.type != INVALID_NODE {
				print_node(p, v.type, next_indent)
			}
			print_node(p, v.expr, next_indent)
		case ParamDecl:
			print_indent(indent)
			fmt.println("ParamDecl:", token_to_string(p, v.token))
			if v.type != INVALID_NODE {
				print_node(p, v.type, next_indent)
			}
			print_node(p, v.expr, next_indent)
		case MemberDecl:
			print_indent(indent)
			fmt.println("MemberDecl:", token_to_string(p, v.token))
			if v.type != INVALID_NODE {
				print_node(p, v.type, next_indent)
			}
			print_node(p, v.expr, next_indent)
		case ProcDecl:
			print_indent(indent)
			fmt.println("ProcDecl:", token_to_string(p, v.token))
			print_node(p, v.return_type, next_indent)
			for param in v.params {
				print_node(p, param, next_indent)
			}
			print_node(p, v.body, next_indent)
		case StructDecl:
			print_indent(indent)
			fmt.println("StructDecl:", token_to_string(p, v.token))
			for member in v.members {
				print_node(p, member, next_indent)
			}
		case ExprStmt:
			print_indent(indent)
			fmt.println("ExprStmt")
			print_node(p, v.expr, next_indent)
		case BlockStmt:
			print_indent(indent)
			fmt.println("BlockStmt:")
			for stmt in v.stmts {
				print_node(p, stmt, next_indent)
			}
		case ReturnStmt:
			print_indent(indent)
			fmt.println("ReturnStmt:")
			print_node(p, v.expr, next_indent)
		case IfStmt:
			print_indent(indent)
			fmt.println("IfStmt:")
			print_node(p, v.cond, next_indent)
			print_node(p, v.then, next_indent)
			print_node(p, v.else_, next_indent)
		case LoopStmt:
			print_indent(indent)
			fmt.println("LoopStmt:")
			print_node(p, v.init, next_indent)
			print_node(p, v.cond, next_indent)
			print_node(p, v.incr, next_indent)
			print_node(p, v.body, next_indent)
		case BreakStmt:
			print_indent(indent)
			fmt.println("BreakStmt:")
		case ContinueStmt:
			print_indent(indent)
			fmt.println("ContinueStmt:")
		case CallExpr:
			print_indent(indent)
			fmt.println("CallExpr:")
			print_node(p, v.callee, next_indent)
			for arg in v.args {
				print_node(p, arg, next_indent)
			}
		case MemberExpr:
			print_indent(indent)
			fmt.println("MemberExpr:")
			print_node(p, v.ident, next_indent)
			print_node(p, v.expr, next_indent)
		case IndexExpr:
			print_indent(indent)
			fmt.println("IndexExpr:")
			print_node(p, v.base, next_indent)
			print_node(p, v.offset, next_indent)
		case BinaryExpr:
			print_indent(indent)
			fmt.println("BinaryExpr:", token_to_string(p, v.token))
			print_node(p, v.left, next_indent)
			print_node(p, v.right, next_indent)
		case UnaryExpr:
			print_indent(indent)
			fmt.println("UnaryExpr:", token_to_string(p, v.token))
			print_node(p, v.expr, next_indent)
		}
	}

	print_node(p, NodeIndex(len(p.nodes) - 1))
}
