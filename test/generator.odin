package test

import "../compiler/lexer"
import "../compiler/parser"
import "../compiler/sema"

import "core:fmt"
import "core:math/rand"
import "core:strings"

// Keep generation biased toward valid, typeable programs that sema accepts.
// We generate only simple expressions/statements using the supported base
// types and maintain a scope of typed bindings while we build AST nodes.

letters := [?]u8 {
	'a',
	'b',
	'c',
	'd',
	'e',
	'f',
	'g',
	'h',
	'i',
	'j',
	'k',
	'l',
	'm',
	'n',
	'o',
	'p',
	'q',
	'r',
	's',
	't',
	'u',
	'v',
	'w',
	'x',
	'y',
	'z',
	'A',
	'B',
	'C',
	'D',
	'E',
	'F',
	'G',
	'H',
	'I',
	'J',
	'K',
	'L',
	'M',
	'N',
	'O',
	'P',
	'Q',
	'R',
	'S',
	'T',
	'U',
	'V',
	'W',
	'X',
	'Y',
	'Z',
}

letters_and_digits := [?]u8 {
	'a',
	'b',
	'c',
	'd',
	'e',
	'f',
	'g',
	'h',
	'i',
	'j',
	'k',
	'l',
	'm',
	'n',
	'o',
	'p',
	'q',
	'r',
	's',
	't',
	'u',
	'v',
	'w',
	'x',
	'y',
	'z',
	'A',
	'B',
	'C',
	'D',
	'E',
	'F',
	'G',
	'H',
	'I',
	'J',
	'K',
	'L',
	'M',
	'N',
	'O',
	'P',
	'Q',
	'R',
	'S',
	'T',
	'U',
	'V',
	'W',
	'X',
	'Y',
	'Z',
	'0',
	'1',
	'2',
	'3',
	'4',
	'5',
	'6',
	'7',
	'8',
	'9',
}

ValueType :: sema.BaseType

numeric_types := []ValueType{sema.BaseType.U32, sema.BaseType.S32, sema.BaseType.F32}
value_types := []ValueType{sema.BaseType.B32, sema.BaseType.U32, sema.BaseType.S32, sema.BaseType.F32}
bool_type :: ValueType(sema.BaseType.B32)

ProcSig :: struct {
	name:        string,
	return_type: ValueType,
	param_types: []ValueType,
}

Context :: struct {
	p:      ^parser.Parser,
	tokens: ^[dynamic]lexer.Token,
	scopes: [dynamic]map[string]ValueType,
	procs:  [dynamic]ProcSig,
	names:  map[string]bool,
	proc_depth: int,
}

gen_identifier :: proc() -> string {
	buf := make([dynamic]u8)
	append(&buf, rand.choice(letters[:]))

	for i in 0 ..< 4 {
		append(&buf, rand.choice(letters_and_digits[:]))
	}

	return string(buf[:])
}

gen_type_name :: proc() -> string {
	bound :: len(sema.base_type_strings)
	return sema.base_type_strings[rand.uint32() % bound]
}

gen_bool :: proc() -> bool {
	return bool(rand.uint32() % 2)
}

gen_integer :: proc() -> i32 {
	return rand.int31()
}

gen_float :: proc() -> f32 {
	return rand.float32()
}

make_context :: proc(p: ^parser.Parser, tokens: ^[dynamic]lexer.Token) -> Context {
	names := make(map[string]bool)
	ctx := Context{p, tokens, {}, {}, names, 0}
	return ctx
}

add_synthetic_token :: proc(
	ctx: ^Context,
	text: string,
	kind: lexer.TokenKind,
) -> lexer.TokenIndex {
	start := lexer.TokenIndex(len(ctx.p.source))
	ctx.p.source = strings.concatenate({ctx.p.source, text})
	end := lexer.TokenIndex(len(ctx.p.source))
	token := lexer.Token{kind, start, end}
	append(ctx.tokens, token)
	return lexer.TokenIndex(len(ctx.tokens) - 1)
}

push_scope :: proc(ctx: ^Context) {
	append(&ctx.scopes, make(map[string]ValueType))
}

pop_scope :: proc(ctx: ^Context) {
	assert(len(ctx.scopes) > 0)
	pop(&ctx.scopes)
}

add_binding :: proc(ctx: ^Context, name: string, type: ValueType) {
	assert(len(ctx.scopes) > 0)
	scope := ctx.scopes[len(ctx.scopes) - 1]
	scope[name] = type
}

ensure_global_scope :: proc(ctx: ^Context) {
	if len(ctx.scopes) == 0 {
		push_scope(ctx)
	} else {
		for len(ctx.scopes) > 1 {
			pop(&ctx.scopes)
		}
	}
}

unique_name :: proc(ctx: ^Context) -> string {
	for {
		name := gen_identifier()
		if _, exists := ctx.names[name]; !exists {
			ctx.names[name] = true
			return name
		}
	}
}

rand_value_type :: proc(list: []ValueType) -> ValueType {
	return list[rand.int31_max(i32(len(list)))]
}

pick_proc_for_type :: proc(ctx: ^Context, type: ValueType) -> (^ProcSig, bool) {
	for proc_sig, i in ctx.procs {
		if proc_sig.return_type == type {
			return &ctx.procs[i], true
		}
	}
	return nil, false
}

pick_existing_name :: proc(ctx: ^Context, type: ValueType) -> (string, bool) {
	for scope_idx := len(ctx.scopes) - 1; scope_idx >= 0; scope_idx -= 1 {
		scope := ctx.scopes[scope_idx]
		for name, t in scope {
			if t == type {
				return name, true
			}
		}
	}
	return "", false
}

type_token :: proc(ctx: ^Context, type: ValueType) -> lexer.TokenIndex {
	type_name := sema.base_type_strings[int(type)]
	return add_synthetic_token(ctx, type_name, .Identifier)
}

make_type_node :: proc(ctx: ^Context, type: ValueType) -> parser.NodeIndex {
	token := type_token(ctx, type)
	return parser.add_node(ctx.p, parser.Node{.Identifier, parser.INVALID_DATA, token})
}

gen_literal :: proc(ctx: ^Context, type: ValueType) -> parser.NodeIndex {
	#partial switch type {
	case sema.BaseType.B32:
		if rand.int31_max(2) == 0 {
			token := add_synthetic_token(ctx, "true", .True)
			return parser.add_node(ctx.p, parser.Node{.True, parser.INVALID_DATA, token})
		} else {
			token := add_synthetic_token(ctx, "false", .False)
			return parser.add_node(ctx.p, parser.Node{.False, parser.INVALID_DATA, token})
		}
	case sema.BaseType.F32:
		val := gen_float()
		token := add_synthetic_token(ctx, fmt.tprintf("%f", val), .Float)
		return parser.add_node(ctx.p, parser.Node{.Float, parser.INVALID_DATA, token})
	case sema.BaseType.U32, sema.BaseType.S32:
		val := gen_integer()
		token := add_synthetic_token(ctx, fmt.tprintf("%d", val), .Integer)
		return parser.add_node(ctx.p, parser.Node{.Integer, parser.INVALID_DATA, token})
	case sema.BaseType.Nil:
		token := add_synthetic_token(ctx, "nil", .Identifier)
		return parser.add_node(ctx.p, parser.Node{.Identifier, parser.INVALID_DATA, token})
	case:
		token := add_synthetic_token(ctx, "0", .Integer)
		return parser.add_node(ctx.p, parser.Node{.Integer, parser.INVALID_DATA, token})
	}
}

gen_identifier_expr :: proc(ctx: ^Context, type: ValueType) -> (parser.NodeIndex, bool) {
	if name, ok := pick_existing_name(ctx, type); ok {
		token := add_synthetic_token(ctx, name, .Identifier)
		return parser.add_node(ctx.p, parser.Node{.Identifier, parser.INVALID_DATA, token}), true
	}
	return parser.INVALID_NODE, false
}

gen_call_expr :: proc(ctx: ^Context, type: ValueType, depth: int) -> (parser.NodeIndex, bool) {
	if depth > 1 {
		return parser.INVALID_NODE, false
	}

	proc_sig, ok := pick_proc_for_type(ctx, type)
	if !ok || proc_sig == nil || len(proc_sig.param_types) > 4 {
		return parser.INVALID_NODE, false
	}

	arg_nodes := make([dynamic]parser.NodeIndex, 0, len(proc_sig.param_types))
	for param_type in proc_sig.param_types {
		append(&arg_nodes, gen_expr(ctx, param_type, depth + 1))
	}

	callee_token := add_synthetic_token(ctx, proc_sig.name, .Identifier)
	callee := parser.add_node(ctx.p, parser.Node{.Identifier, parser.INVALID_DATA, callee_token})

	call := parser.CallExpr{callee, arg_nodes[:]}
	data := parser.encode_data(&ctx.p.data, call)
	return parser.add_node(ctx.p, parser.Node{.Call, data, callee_token}), true
}

gen_numeric_expr :: proc(ctx: ^Context, type: ValueType, depth: int) -> parser.NodeIndex {
	if depth > 1 {
		if node, ok := gen_identifier_expr(ctx, type); ok {
			return node
		}
		return gen_literal(ctx, type)
	}

	choice := rand.int31_max(3)
	switch choice {
	case 0:
		if node, ok := gen_identifier_expr(ctx, type); ok {
			return node
		}
		return gen_literal(ctx, type)
	case 1:
		if node, ok := gen_call_expr(ctx, type, depth); ok {
			return node
		}
		// fallthrough to simple literal if no call target
		fallthrough
	case 2:
		left := gen_numeric_expr(ctx, type, depth + 1)
		right := gen_numeric_expr(ctx, type, depth + 1)
		token := add_synthetic_token(ctx, "+", .Plus)
		data := parser.encode_data(&ctx.p.data, parser.AddExpr{left, right})
		return parser.add_node(ctx.p, parser.Node{.Addition, data, token})
	case:
		left := gen_numeric_expr(ctx, type, depth + 1)
		right := gen_numeric_expr(ctx, type, depth + 1)
		token := add_synthetic_token(ctx, "*", .Asterisk)
		data := parser.encode_data(&ctx.p.data, parser.MulExpr{left, right})
		return parser.add_node(ctx.p, parser.Node{.Multiplication, data, token})
	}
}

gen_bool_expr :: proc(ctx: ^Context, depth: int) -> parser.NodeIndex {
	if depth > 1 {
		if node, ok := gen_identifier_expr(ctx, bool_type); ok {
			return node
		}
		return gen_literal(ctx, bool_type)
	}

	choice := rand.int31_max(3)
	switch choice {
	case 0:
		if node, ok := gen_identifier_expr(ctx, bool_type); ok {
			return node
		}
		return gen_literal(ctx, bool_type)
	case 1:
		if node, ok := gen_call_expr(ctx, bool_type, depth); ok {
			return node
		}
		// fallthrough to comparison if no call candidate
		fallthrough
	case 2:
		// compare two numeric expressions to produce a boolean
		num_type := numeric_types[rand.int31_max(i32(len(numeric_types)))]
		left := gen_numeric_expr(ctx, num_type, depth + 1)
		right := gen_numeric_expr(ctx, num_type, depth + 1)
		token := add_synthetic_token(ctx, "<", .Less)
		data := parser.encode_data(&ctx.p.data, parser.LessExpr{left, right})
		return parser.add_node(ctx.p, parser.Node{.Less, data, token})
	}

	// default: equality for boolean variety
	num_type := numeric_types[rand.int31_max(i32(len(numeric_types)))]
	left := gen_numeric_expr(ctx, num_type, depth + 1)
	right := gen_numeric_expr(ctx, num_type, depth + 1)
	token := add_synthetic_token(ctx, "==", .EqualEqual)
	data := parser.encode_data(&ctx.p.data, parser.EqualExpr{left, right})
	return parser.add_node(ctx.p, parser.Node{.Equal, data, token})
}

gen_expr :: proc(ctx: ^Context, type: ValueType, depth: int = 0) -> parser.NodeIndex {
	#partial switch type {
	case sema.BaseType.B32:
		return gen_bool_expr(ctx, depth)
	case sema.BaseType.U32, sema.BaseType.S32, sema.BaseType.F32:
		return gen_numeric_expr(ctx, type, depth)
	case:
		return gen_literal(ctx, type)
	}
}

gen_assignment_expr :: proc(ctx: ^Context, type: ValueType) -> (parser.NodeIndex, bool) {
	name, ok := pick_existing_name(ctx, type)
	if !ok {
		return parser.INVALID_NODE, false
	}

	left_token := add_synthetic_token(ctx, name, .Identifier)
	left := parser.add_node(ctx.p, parser.Node{.Identifier, parser.INVALID_DATA, left_token})
	right := gen_expr(ctx, type, 1)
	token := add_synthetic_token(ctx, "=", .Equal)
	data := parser.encode_data(&ctx.p.data, parser.AssignExpr{left, right})
	return parser.add_node(ctx.p, parser.Node{.Assignment, data, token}), true
}

gen_return :: proc(ctx: ^Context, ret_type: ValueType) -> parser.NodeIndex {
	value := parser.INVALID_NODE
	if ret_type != sema.BaseType.Nil {
		value = gen_expr(ctx, ret_type, 0)
	}

	data := parser.encode_data(&ctx.p.data, parser.ReturnStmt{value})
	token := add_synthetic_token(ctx, "return", .Return)
	return parser.add_node(ctx.p, parser.Node{.Return, data, token})
}

gen_var_decl :: proc(ctx: ^Context, type: ValueType) -> (parser.NodeIndex, string) {
	name := unique_name(ctx)
	value := gen_expr(ctx, type, 0)
	type_node := make_type_node(ctx, type)
	data := parser.encode_data(&ctx.p.data, parser.VarDecl{type_node, value})
	token := add_synthetic_token(ctx, name, .Identifier)

	add_binding(ctx, name, type)
	return parser.add_node(ctx.p, parser.Node{.Variable, data, token}), name
}

gen_global_var :: proc(ctx: ^Context) -> parser.NodeIndex {
	ensure_global_scope(ctx)
	t := rand_value_type(value_types)
	node, _ := gen_var_decl(ctx, t)
	return node
}

gen_for :: proc(ctx: ^Context, ret_type: ValueType) -> parser.NodeIndex {
	// simple numeric counter loop with a guaranteed typed return inside body
	iter_type := numeric_types[rand.int31_max(i32(len(numeric_types)))]

	push_scope(ctx)
	initial, iter_name := gen_var_decl(ctx, iter_type)

	limit := gen_numeric_expr(ctx, iter_type, 1)
	token := add_synthetic_token(ctx, "<", .Less)
	iter_token := add_synthetic_token(ctx, iter_name, .Identifier)
	iter_expr := parser.add_node(ctx.p, parser.Node{.Identifier, parser.INVALID_DATA, iter_token})
	cond_data := parser.encode_data(&ctx.p.data, parser.LessExpr{iter_expr, limit})
	condition := parser.add_node(ctx.p, parser.Node{.Less, cond_data, token})

	update_expr, ok := gen_assignment_expr(ctx, iter_type)
	if !ok {
		iter_token := add_synthetic_token(ctx, iter_name, .Identifier)
		iter_target := parser.add_node(ctx.p, parser.Node{.Identifier, parser.INVALID_DATA, iter_token})
		increment := gen_numeric_expr(ctx, iter_type, 1)
		assign_token := add_synthetic_token(ctx, "=", .Equal)
		assign_data := parser.encode_data(&ctx.p.data, parser.AssignExpr{iter_target, increment})
		update_expr = parser.add_node(ctx.p, parser.Node{.Assignment, assign_data, assign_token})
	}

	body_stmts := make([dynamic]parser.NodeIndex)
	// add one extra statement before the return to keep body non-trivial
	var_stmt, _ := gen_var_decl(ctx, iter_type)
	append(&body_stmts, var_stmt)
	append(&body_stmts, gen_return(ctx, ret_type))
	body_block := parser.BlockStmt{body_stmts[:]}
	body_data := parser.encode_data(&ctx.p.data, body_block)
	body_token := add_synthetic_token(ctx, "{", .LeftBrace)
	body := parser.add_node(ctx.p, parser.Node{.Block, body_data, body_token})

	pop_scope(ctx)

	for_stmt := parser.ForStmt{initial, condition, update_expr, body}
	data := parser.encode_data(&ctx.p.data, for_stmt)
	token = add_synthetic_token(ctx, "for", .For)
	return parser.add_node(ctx.p, parser.Node{.For, data, token})
}

gen_block :: proc(ctx: ^Context, ret_type: ValueType, depth: int = 0) -> parser.NodeIndex {
	push_scope(ctx)

	stmts := make([dynamic]parser.NodeIndex)

	// optionally inject a nested procedure (non-capturing; uses globals and parameters only)
	if depth == 0 && rand.int31_max(3) == 0 {
		sig := build_proc_sig(ctx)
		append(&ctx.procs, sig)
		append(&stmts, gen_procedure(ctx, sig))
	}

	stmt_count := 1 + rand.int31_max(2)
	for i in 0 ..< stmt_count {
		append(&stmts, gen_stmt(ctx, ret_type, depth + 1))
	}
	if rand.int31_max(3) == 0 {
		append(&stmts, gen_return(ctx, ret_type))
	}

	block_stmt := parser.BlockStmt{stmts[:]}
	data := parser.encode_data(&ctx.p.data, block_stmt)
	token := add_synthetic_token(ctx, "{", .LeftBrace)
	node := parser.add_node(ctx.p, parser.Node{.Block, data, token})

	pop_scope(ctx)
	return node
}

gen_if :: proc(ctx: ^Context, ret_type: ValueType) -> parser.NodeIndex {
	condition := gen_bool_expr(ctx, 0)

	then_body := gen_block(ctx, ret_type, 1)
	else_body := parser.INVALID_NODE
	if rand.int31_max(2) == 0 {
		else_body = gen_block(ctx, ret_type, 1)
	}

	if_expr := parser.IfExpr{condition, then_body, else_body}
	data := parser.encode_data(&ctx.p.data, if_expr)
	token := add_synthetic_token(ctx, "if", .If)
	return parser.add_node(ctx.p, parser.Node{.If, data, token})
}

gen_stmt :: proc(ctx: ^Context, ret_type: ValueType, depth: int = 0) -> parser.NodeIndex {
	choice := rand.int31_max(4)
	switch choice {
	case 0:
		// new variable using any value type except Nil
		t := numeric_types[rand.int31_max(i32(len(numeric_types)))]
		stmt, _ := gen_var_decl(ctx, t)
		return stmt
	case 1:
		// assignment or expression statement
		t := numeric_types[rand.int31_max(i32(len(numeric_types)))]
		if node, ok := gen_assignment_expr(ctx, t); ok {
			data := parser.encode_data(&ctx.p.data, parser.ExprStmt{node})
			token := add_synthetic_token(ctx, ";", .Semicolon)
			return parser.add_node(ctx.p, parser.Node{.ExprStmt, data, token})
		}
		stmt, _ := gen_var_decl(ctx, t)
		return stmt
	case 2:
		if depth < 2 {
			return gen_if(ctx, ret_type)
		}
		stmt, _ := gen_var_decl(ctx, numeric_types[rand.int31_max(i32(len(numeric_types)))])
		return stmt
	case 3:
		if depth < 1 && ctx.proc_depth < 2 {
			sig := build_proc_sig(ctx)
			append(&ctx.procs, sig)
			return gen_procedure(ctx, sig)
		}
		return gen_for(ctx, ret_type)
	case:
		return gen_for(ctx, ret_type)
	}
}

gen_parameter :: proc(ctx: ^Context, type_choice: ValueType) -> parser.NodeIndex {
	param_name := unique_name(ctx)
	param_token := add_synthetic_token(ctx, param_name, .Identifier)

	type_node := make_type_node(ctx, type_choice)

	param_stmt := parser.ParamDecl{type_node, parser.INVALID_NODE}
	data := parser.encode_data(&ctx.p.data, param_stmt)

	add_binding(ctx, param_name, type_choice)
	return parser.add_node(ctx.p, parser.Node{.Parameter, data, param_token})
}

build_proc_sig :: proc(ctx: ^Context) -> ProcSig {
	name := unique_name(ctx)

	param_count := rand.int31_max(3)
	param_types := make([dynamic]ValueType)
	for i in 0 ..< param_count {
		append(&param_types, rand_value_type(value_types))
	}

	ret_type := rand_value_type(value_types)
	if rand.int31_max(6) == 0 {
		ret_type = sema.BaseType.Nil
	}

	return ProcSig{name, ret_type, param_types[:]}
}

gen_procedure :: proc(ctx: ^Context, sig: ProcSig) -> parser.NodeIndex {
	// save current scopes and ensure we include only global scope for procedure body
	orig_scopes := ctx.scopes
	defer ctx.scopes = orig_scopes
	ensure_global_scope(ctx)
	push_scope(ctx)
	old_depth := ctx.proc_depth
	ctx.proc_depth += 1
	defer ctx.proc_depth = old_depth

	// add parameters to scope while building nodes
	params := make([dynamic]parser.NodeIndex)
	for _, i in sig.param_types {
		append(&params, gen_parameter(ctx, sig.param_types[i]))
	}

	ret_type_node := make_type_node(ctx, sig.return_type)

	stmt_count := 1 + rand.int31_max(3)
	stmts := make([dynamic]parser.NodeIndex)
	for i in 0 ..< stmt_count {
		append(&stmts, gen_stmt(ctx, sig.return_type))
	}
	append(&stmts, gen_return(ctx, sig.return_type))

	block_stmt := parser.BlockStmt{stmts[:]}
	block_data := parser.encode_data(&ctx.p.data, block_stmt)
	body_token := add_synthetic_token(ctx, "{", .LeftBrace)
	body := parser.add_node(ctx.p, parser.Node{.Block, block_data, body_token})

	id_token := add_synthetic_token(ctx, sig.name, .Identifier)
	token := add_synthetic_token(ctx, sig.name, .ColonColon)

	proc_stmt := parser.ProcDecl{id_token, ret_type_node, body, params[:]}
	proc_data := parser.encode_data(&ctx.p.data, proc_stmt)

	return parser.add_node(ctx.p, parser.Node{.Procedure, proc_data, token})
}

gen_module :: proc(p: ^parser.Parser, tokens: ^[dynamic]lexer.Token) -> parser.NodeIndex {
	ctx := make_context(p, tokens)
	ensure_global_scope(&ctx)

	// create signatures up front so calls can target any procedure
	proc_count := 1 + rand.int31_max(2)
	for _ in 0 ..< proc_count {
		append(&ctx.procs, build_proc_sig(&ctx))
	}

	// add some typed globals before procedures
	global_count := 1 + rand.int31_max(2)
	stmts := make([dynamic]parser.NodeIndex)
	for i in 0 ..< global_count {
		append(&stmts, gen_global_var(&ctx))
	}

	for sig in ctx.procs {
		append(&stmts, gen_procedure(&ctx, sig))
	}

	module_decl := parser.ModuleDecl{stmts[:]}
	data := parser.encode_data(&p.data, module_decl)

	return parser.add_node(p, parser.Node{.Module, data, 0})
}
