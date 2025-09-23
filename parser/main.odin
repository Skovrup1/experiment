package parser

import "../scanner"

import "core:fmt"
import path "core:path/slashpath"
import "core:strconv"
import "core:strings"

// Macro :: struct {}

Module :: struct {
	name:  string,
	nodes: []NodeIndex,
}

Import :: struct {
	token: scanner.TokenIndex,
}

PrimType :: struct {
	token: scanner.TokenIndex,
}

RefType :: struct {
	token: scanner.TokenIndex,
	type:  NodeIndex,
}

StructType :: struct {
	token: scanner.TokenIndex,
	types: []NodeIndex,
}

TupleType :: struct {
	token: scanner.TokenIndex,
	types: []NodeIndex,
}

ArrayType :: struct {
	token: scanner.TokenIndex,
	types: []NodeIndex,
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

ArrayLit :: struct {
	token:  scanner.TokenIndex,
	values: []NodeIndex,
}

StructLit :: struct {
	token:  scanner.TokenIndex,
	values: []NodeIndex,
}

UnionLit :: struct {
	token:  scanner.TokenIndex,
	values: []NodeIndex,
}

TupleLit :: struct {
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

EnumMemberDecl :: struct {
	token: scanner.TokenIndex,
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

UnionDecl :: struct {
	token:   scanner.TokenIndex,
	members: []NodeIndex,
}

EnumDecl :: struct {
	token:   scanner.TokenIndex,
	members: []NodeIndex,
}

DestructVarDecl :: struct {
	token:    scanner.TokenIndex,
	// note: consider TokenIndex
	elements: []NodeIndex,
	type:     NodeIndex,
	expr:     NodeIndex,
}

DestructConstDecl :: struct {
	token:    scanner.TokenIndex,
	// note: consider TokenIndex
	elements: []NodeIndex,
	type:     NodeIndex,
	expr:     NodeIndex,
}

DestructAssign :: struct {
	token:    scanner.TokenIndex,
	elements: []NodeIndex,
	expr:     NodeIndex,
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
	Import,
	PrimType,
	RefType,
	TupleType,
	StructType,
	ArrayType,
	IdentLit,
	StringLit,
	RealLit,
	IntLit,
	BoolLit,
	ArrayLit,
	StructLit, // note: no way for the parser to tell the difference UnionLit and StructLit
	TupleLit,
	VarDecl,
	ConstDecl,
	ParamDecl,
	MemberDecl,
	EnumMemberDecl,
	ProcDecl,
	StructDecl,
	UnionDecl,
	EnumDecl,
	DestructVarDecl,
	DestructConstDecl,
	DestructAssign,
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

Parser :: struct {
	source:      []u8,
	source_path: string,
	tokens:      []scanner.Token,
	nodes:       [dynamic]Node,
	cursor:      scanner.TokenIndex,
	lookahead:   scanner.TokenIndex,
}

make_parser :: proc(source: []u8, tokens: []scanner.Token, source_path: string) -> Parser {
	nodes := make([dynamic]Node)

	parser := Parser{source, source_path, tokens[:], nodes, 0, 1}

	return parser
}

next :: proc(p: ^Parser) -> scanner.TokenIndex {
	p.cursor = p.lookahead
	p.lookahead += 1

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
		token_start_pos := p.tokens[p.cursor].start
		line, column := scanner.get_position(p.source, token_start_pos)
		panic(
			fmt.tprintf(
				"expected token %v, but got %v at line %d, column %d (%v)",
				expected,
				peek(p),
				line,
				column,
				loc,
			),
		)
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
	fmt.println(node)
	append(&p.nodes, node)
	return NodeIndex(len(p.nodes) - 1)
}

parse :: proc(p: ^Parser) -> []Node {
	parse_file_level(p)
	return p.nodes[:]
}

get_module_name :: proc(source_path: string) -> string {
	filename := path.base(source_path)

	if pos := strings.index(filename, "."); pos != -1 {
		return filename[:pos]
	}
	return filename
}

parse_file_level :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	stmts := make([dynamic]NodeIndex, 0, 1)

	for peek(p) != .Eof {
		append(&stmts, parse_stmt(p))
	}
	return add_node(p, Module{get_module_name(p.source_path), stmts[:]})
}

parse_type :: proc(p: ^Parser, loc := #caller_location) -> NodeIndex {
	token := p.cursor

	#partial switch peek(p) {
	case .Mul:
		next(p)
		type := parse_type(p)
		return add_node(p, RefType{token, type})
	case .LParen:
		next(p)

		types := make([dynamic]NodeIndex, 0, 1)
		for peek(p) != .RParen {
			append(&types, parse_type(p))
			if peek(p) == .Comma {
				next(p)
			} else {
				break
			}
		}
		next(p)

		return add_node(p, TupleType{token, types[:]})
	case .LBrace:
		types := make([dynamic]NodeIndex, 0, 1)
		for peek(p) != .RParen {
			append(&types, parse_type(p))
			if peek(p) == .Comma {
				next(p)
			} else {
				break
			}
		}
		next(p)

		return add_node(p, StructType{token, types[:]})
	case .LBracket:
		panic("no array types")
	case .Identifier:
		next(p)
		return add_node(p, PrimType{token})
	case:
		panic(fmt.tprintf("invalid type: %v, at %v", peek(p), loc))
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
		token := p.cursor
		next(p)
		expr := parse_expr(p)

		if peek(p) != .Comma {
			expect(p, .RParen)
			return expr
		}

		next(p) // comma

		exprs := make([dynamic]NodeIndex, 0, 2)
		append(&exprs, expr)
		for peek(p) != .RParen {
			append(&exprs, parse_expr(p))
			if peek(p) == .Comma {
				next(p)
			} else {
				break
			}
		}
		next(p)

		return add_node(p, TupleLit{token, exprs[:]})
	case:
		token_start_pos := p.tokens[p.cursor].start
		line, column := scanner.get_position(p.source, token_start_pos)
		panic(fmt.tprintf("invalid atom: %v at line %d, column %d", atom, line, column))
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
		append(&stmts, parse_stmt(p))
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
	then := parse_stmt(p)
	else_ := INVALID_NODE
	if allow(p, .Else) {
		else_ = parse_stmt(p)
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

	body := parse_stmt(p)

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

	body := parse_stmt(p)

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
		panic(fmt.tprintf("invalid member decl: %v\n", peek(p)))
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

parse_union_decl :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
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

	return add_node(p, UnionDecl{token, members[:]})
}

parse_enum_member_decl :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	expect(p, .Identifier)

	expr := INVALID_NODE
	if allow(p, .Assign) {
		parse_expr(p)
	}

	return add_node(p, EnumMemberDecl{token, expr})
}

parse_enum_decl :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	if allow(p, .LParen) {
		expect(p, .Identifier)
		expect(p, .RParen)
	}
	expect(p, .LBrace)

	members := make([dynamic]NodeIndex, 0, 1)
	for peek(p) != .RBrace {
		append(&members, parse_enum_member_decl(p))
		if peek(p) == .Comma {
			next(p)
		} else {
			break
		}
	}
	next(p)

	return add_node(p, EnumDecl{token, members[:]})
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

parse_destructuring :: proc(p: ^Parser, token: scanner.TokenIndex) -> NodeIndex {
	pattern := parse_atom(p)
	patterns := make([dynamic]NodeIndex, 0, 2)
	append(&patterns, pattern)

	for peek(p) == .Comma {
		next(p)
		append(&patterns, parse_atom(p))
	}

	if allow(p, .Colon) {
		type_node := parse_type(p)

		if allow(p, .Assign) {
			expr_node := parse_expr(p)
			expect(p, .Semicolon)
			return add_node(p, DestructVarDecl{token, patterns[:], type_node, expr_node})
		} else if allow(p, .Colon) {
			expr_node := parse_expr(p)
			expect(p, .Semicolon)
			return add_node(p, DestructConstDecl{token, patterns[:], type_node, expr_node})
		} else {
			panic("expected = or : after type annotation in destructuring")
		}
	} else if allow(p, .Var) {
		expr_node := parse_expr(p)
		expect(p, .Semicolon)
		return add_node(p, DestructVarDecl{token, patterns[:], INVALID_NODE, expr_node})
	} else if allow(p, .Const) {
		expr_node := parse_expr(p)
		expect(p, .Semicolon)
		return add_node(p, DestructConstDecl{token, patterns[:], INVALID_NODE, expr_node})
	} else if allow(p, .Assign) {
		expr_node := parse_expr(p)
		expect(p, .Semicolon)
		return add_node(p, DestructAssign{token, patterns[:], expr_node})
	} else {
		panic("expected :, :=, ::, or = after destructuring pattern")
	}
}

parse_identifier_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor

	#partial switch peek_next(p) {
	case .Var:
		next(p)
		next(p)

		return parse_var_decl(p, token)
	case .Const:
		next(p)
		next(p)

		#partial switch peek(p) {
		case .LParen:
			next(p)
			return parse_proc_decl(p, token)
		case .Enum:
			next(p)
			return parse_enum_decl(p, token)
		case .Struct:
			next(p)
			return parse_struct_decl(p, token)
		case .Union:
			next(p)
			return parse_union_decl(p, token)
		}

		return parse_const_decl(p, token)
	case .Colon:
		next(p)
		next(p)

		return parse_type_annotation(p, token)
	case .Comma:
		return parse_destructuring(p, token)
	case:
		expr := parse_expr(p)
		expect(p, .Semicolon)
		return add_node(p, ExprStmt{token, expr})
	}
}

parse_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor

	#partial switch peek(p) {
	case .Identifier:
		return parse_identifier_stmt(p)
	case .LBrace:
		return parse_block_stmt(p)
	case .Return:
		return parse_return_stmt(p)
	case .If:
		return parse_if_stmt(p)
	case .Loop:
		return parse_for_stmt(p)
	case .Break:
		next(p)
		expect(p, .Semicolon)
		return add_node(p, BreakStmt{token})
	case .Continue:
		next(p)
		expect(p, .Semicolon)
		return add_node(p, ContinueStmt{token})
	case .Import:
		next(p)
		expect(p, .String)
		expect(p, .Semicolon)
		return add_node(p, Import{token})
	case:
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
			fmt.println("Module:", v.name)
			for node in v.nodes {
				print_node(p, node, next_indent)
			}
		case PrimType:
			print_indent(indent)
			fmt.println("PrimType:", token_to_string(p, v.token))
		case RefType:
			print_indent(indent)
			fmt.println("RefType:")
			print_node(p, v.type, next_indent)
		case TupleType:
			print_indent(indent)
			fmt.println("TupleType:")
			for type in v.types {
				print_node(p, type, next_indent)
			}
		case StructType:
			print_indent(indent)
			fmt.println("StructType:")
			for type in v.types {
				print_node(p, type, next_indent)
			}
		case ArrayType:
			print_indent(indent)
			fmt.println("ArrayType:")
			for type in v.types {
				print_node(p, type, next_indent)
			}
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
		case TupleLit:
			print_indent(indent)
			fmt.println("TupleLit:")
			for value in v.values {
				print_node(p, value, next_indent)
			}
		case VarDecl:
			print_indent(indent)
			fmt.println("VarDecl:", token_to_string(p, v.token))
			print_node(p, v.type, next_indent)
			print_node(p, v.expr, next_indent)
		case ConstDecl:
			print_indent(indent)
			fmt.println("ConstDecl:", token_to_string(p, v.token))
			print_node(p, v.type, next_indent)
			print_node(p, v.expr, next_indent)
		case ParamDecl:
			print_indent(indent)
			fmt.println("ParamDecl:", token_to_string(p, v.token))
			print_node(p, v.type, next_indent)
			print_node(p, v.expr, next_indent)
		case MemberDecl:
			print_indent(indent)
			fmt.println("MemberDecl:", token_to_string(p, v.token))
			print_node(p, v.type, next_indent)
			print_node(p, v.expr, next_indent)
		case EnumMemberDecl:
			print_indent(indent)
			fmt.println("EnumMemberDecl:", token_to_string(p, v.token))
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
		case UnionDecl:
			print_indent(indent)
			fmt.println("UnionDecl:", token_to_string(p, v.token))
			for member in v.members {
				print_node(p, member, next_indent)
			}
		case EnumDecl:
			print_indent(indent)
			fmt.println("EnumDecl:", token_to_string(p, v.token))
			for member in v.members {
				print_node(p, member, next_indent)
			}
		case DestructVarDecl:
			print_indent(indent)
			fmt.println("DestructVarDecl:", token_to_string(p, v.token))
			for element in v.elements {
				print_node(p, element, next_indent)
			}
			if v.type != INVALID_NODE {
				print_indent(next_indent)
				fmt.println("type:")
				print_node(p, v.type, next_indent + 2)
			}
			print_indent(next_indent)
			fmt.println("expr:")
			print_node(p, v.expr, next_indent + 2)
		case DestructConstDecl:
			print_indent(indent)
			fmt.println("DestructConstDecl:", token_to_string(p, v.token))
			for element in v.elements {
				print_node(p, element, next_indent)
			}
			print_indent(next_indent)
			fmt.println("type:")
			print_node(p, v.type, next_indent + 2)
			print_indent(next_indent)
			fmt.println("expr:")
			print_node(p, v.expr, next_indent + 2)
		case DestructAssign:
			print_indent(indent)
			fmt.println("DestructAssign:", token_to_string(p, v.token))
			for element in v.elements {
				print_node(p, element, next_indent)
			}
			print_indent(next_indent)
			fmt.println("expr:")
			print_node(p, v.expr, next_indent + 2)
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
