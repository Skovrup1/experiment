package sema

import "../lexer"
import "../parser"

import "core:fmt"
import "core:slice"

BaseType :: enum u8 {
	Nil = 0,
	B32,
	U32,
	S32,
	F32,
}

base_type_strings := [?]string{"Nil", "B32", "U32", "S32", "F32"}

TypeKind :: enum u8 {
	Primitive,
	Procedure,
}

PrimitiveType :: struct {
	inner: BaseType,
}

ProcedureType :: struct {
	return_type: TypeIndex,
	param_names: []StringIndex,
	param_types: []TypeIndex,
}

TypeData :: struct #raw_union {
	primitive: PrimitiveType,
	procedure: ProcedureType,
}

Type :: struct {
	kind:            TypeKind,
	using type_data: TypeData,
}

SemaError :: struct {
	message:  string,
	position: parser.NodeIndex,
}

SymbolKind :: enum u8 {
	Variable,
	Parameter,
	Procedure,
}

SymbolState :: enum u8 {
	Declared,
	Typed,
}

Symbol :: struct {
	kind:  SymbolKind,
	name:  StringIndex,
	type:  TypeIndex,
	state: SymbolState,
}

Scope :: map[StringIndex]SymbolIndex

StringIndex :: distinct u32
INVALID_STRING :: max(StringIndex)

TypeIndex :: distinct u32
INVALID_TYPE :: max(TypeIndex)

SymbolIndex :: distinct u32
INVALID_SYMBOL :: max(SymbolIndex)

NodeContext :: enum u8 {
	ExprValue,
	ExprDiscard,
	ExprCallee,
	Stmt,
}

Analyzer :: struct {
	source:            string,
	tokens:            []lexer.Token,
	nodes:             []parser.Node,
	node_data:         []u32,
	types:             [dynamic]Type,
	type_map:          map[u64]TypeIndex,
	return_type_stack: [dynamic]TypeIndex,
	strings:           [dynamic]string,
	string_map:        map[string]StringIndex,
	symbols:           [dynamic]Symbol,
	scopes:            [dynamic]Scope,
	errors:            [dynamic]SemaError,
}

make_analyzer :: proc(
	source: string,
	tokens: []lexer.Token,
	nodes: []parser.Node,
	node_data: []u32,
) -> Analyzer {
	types := make([dynamic]Type)
	type_map := make(map[u64]TypeIndex)
	return_type_stack := make([dynamic]TypeIndex, 0, 16)

	strings := make([dynamic]string)
	string_map := make(map[string]StringIndex)

	symbols := make([dynamic]Symbol)

	scopes := make([dynamic]Scope, 0, 16)

	errors := make([dynamic]SemaError)

	a := Analyzer {
		source,
		tokens,
		nodes,
		node_data,
		types,
		type_map,
		return_type_stack,
		strings,
		string_map,
		symbols,
		scopes,
		errors,
	}

	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.Nil}}})
	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.B32}}})
	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.U32}}})
	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.S32}}})
	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.F32}}})

	return a
}

add_error :: proc(a: ^Analyzer, message: string, position := parser.INVALID_NODE) {
	error := SemaError{message, position}
	append(&a.errors, error)
}

add_string :: proc(a: ^Analyzer, str: string) -> StringIndex {
	append(&a.strings, str)
	return StringIndex(len(a.strings) - 1)
}

get_or_add_string :: proc(a: ^Analyzer, str: string) -> StringIndex {
	if index, exists := a.string_map[str]; exists {
		return index
	}

	index := add_string(a, str)
	a.string_map[str] = index
	return index
}

get_type_from_name :: proc(name: string) -> (TypeIndex, bool) {
	for _, i in BaseType {
		if base_type_strings[i] == name {
			return TypeIndex(i), true
		}
	}

	return 0, false
}

lookup_symbol :: proc(a: ^Analyzer, name: string) -> (SymbolIndex, bool) {
	string_index, string_exists := a.string_map[name]
	if !string_exists {
		return INVALID_SYMBOL, false
	}

	#reverse for scope in a.scopes {
		if symbol_index, exists := scope[string_index]; exists {
			return symbol_index, true
		}
	}

	return INVALID_SYMBOL, false
}

add_symbol_to_scope :: proc(a: ^Analyzer, scope: ^Scope, symbol: Symbol) -> SymbolIndex {
	append(&a.symbols, symbol)
	symbol_index := SymbolIndex(len(a.symbols) - 1)

	if _, ok := scope[symbol.name]; ok {
		add_error(a, fmt.tprintf("redeclaration of %v", symbol.name), parser.INVALID_NODE)
	}

	scope[symbol.name] = symbol_index
	return symbol_index
}

add_symbol_to_current_scope :: proc(a: ^Analyzer, symbol: Symbol) -> SymbolIndex {
	current_scope := &a.scopes[len(a.scopes) - 1]
	return add_symbol_to_scope(a, current_scope, symbol)
}

lookup_type :: proc(a: Analyzer, node_index: parser.NodeIndex) -> TypeIndex {
	node := a.nodes[node_index]
	#partial switch node.kind {
	case .Identifier:
		token := a.tokens[node.token]
		name := a.source[token.start:token.end]
		if type_kind, ok := get_type_from_name(name); ok {
			return TypeIndex(type_kind)
		}
	}

	panic(fmt.tprintf("failed to lookup %v", node_index))
}

compare_type :: proc(a: Type, b: Type) -> bool {
	if a.kind != b.kind {
		return false
	}

	switch a.kind {
	case .Primitive:
		return a.primitive == b.primitive
	case .Procedure:
		ap := a.procedure
		bp := a.procedure

		if ap.return_type != bp.return_type {
			return false
		}

		if !slice.equal(ap.param_names, bp.param_names) {
			return false
		}

		if !slice.equal(ap.param_types, bp.param_types) {
			return false
		}
	}

	return true
}

hash_type :: proc(type: Type) -> u64 {
	acc := u64(14695981039346656037)
	mix :: proc(acc: u64, data: $T) -> u64 {
		return (acc ~ u64(data)) * 1099511628211
	}

	acc = mix(acc, type.kind)

	#partial switch type.kind {
	case .Primitive:
		acc = mix(acc, type.primitive.inner)
	case .Procedure:
		p := type.procedure

		acc = mix(acc, p.return_type)

		for param_name in p.param_names {
			acc = mix(acc, param_name)
		}

		for param_type in p.param_types {
			acc = mix(acc, param_type)
		}
	}

	return acc
}

add_type :: proc(a: ^Analyzer, type: Type) -> TypeIndex {
	append(&a.types, type)
	return TypeIndex(len(a.types) - 1)
}

get_or_add_type :: proc(a: ^Analyzer, type: Type) -> TypeIndex {
	key := hash_type(type)

	if index, exists := a.type_map[key]; exists {
		if compare_type(a.types[index], type) {
			return index
		}

		panic("hash collision in type table")
	}

	index := add_type(a, type)
	a.type_map[key] = index
	return index
}

build_procedure_symbol :: proc(a: ^Analyzer, proc_decl: parser.ProcDecl) -> Symbol {
	proc_token := a.tokens[proc_decl.name]
	proc_name := a.source[proc_token.start:proc_token.end]

	param_types := make([dynamic]TypeIndex, 0, len(proc_decl.parameters))
	param_names := make([dynamic]StringIndex, 0, len(proc_decl.parameters))
	for param_node_index in proc_decl.parameters {
		param_node := a.nodes[param_node_index]
		assert(param_node.kind == .Parameter)

		param_token := a.tokens[param_node.token]
		param_name := a.source[param_token.start:param_token.end]
		append(&param_names, get_or_add_string(a, param_name))

		param_decl := parser.decode_data(a.node_data, param_node.data, parser.ParamDecl)

		if param_decl.type == parser.INVALID_NODE {
			panic("todo infer param type from value")
		}

		param_type := lookup_type(a^, param_decl.type)
		append(&param_types, param_type)
	}

	return_type := INVALID_TYPE
	if proc_decl.return_type == parser.INVALID_NODE {
		return_type = TypeIndex(BaseType.Nil)
	} else {
		return_type = lookup_type(a^, proc_decl.return_type)
	}

	proc_type := get_or_add_type(
		a,
		{.Procedure, {procedure = {return_type, param_names[:], param_types[:]}}},
	)

	return Symbol{.Procedure, get_or_add_string(a, proc_name), proc_type, .Typed}
}

enter_procedure :: proc(a: ^Analyzer, node: parser.Node) {
	proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)
	proc_token := a.tokens[node.token]
	proc_name := a.source[proc_token.start:proc_token.end]

	if symbol_index, ok := lookup_symbol(a, proc_name); ok {
		symbol := a.symbols[symbol_index]
		proc_type := a.types[symbol.type]
		assert(proc_type.kind == .Procedure)

		append(&a.scopes, make(Scope))
		append(&a.return_type_stack, proc_type.procedure.return_type)

		for param_name, i in proc_type.procedure.param_names {
			param_type := proc_type.procedure.param_types[i]

			symbol := Symbol{.Parameter, param_name, param_type, .Typed}

			add_symbol_to_current_scope(a, symbol)
		}

		check_node(a, proc_decl.body, .Stmt)

		pop(&a.scopes)
		pop(&a.return_type_stack)
	} else {
		panic("failed to enter procedure")
	}
}

declare_globals :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	assert(node_index != parser.INVALID_NODE)

	node := a.nodes[node_index]
	token := a.tokens[node.token]

	#partial switch node.kind {
	case .Variable:
		var_name := a.source[token.start:token.end]

		symbol := Symbol{.Variable, get_or_add_string(a, var_name), INVALID_TYPE, .Declared}

		add_symbol_to_current_scope(a, symbol)
	case .Procedure:
		proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)
		symbol := build_procedure_symbol(a, proc_decl)

		add_symbol_to_current_scope(a, symbol)
	case:
		panic("oops")
	}
}

declare_procedures :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	assert(node_index != parser.INVALID_NODE)

	node := a.nodes[node_index]

	if node.kind == .Procedure {
		proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)
		symbol := build_procedure_symbol(a, proc_decl)

		add_symbol_to_current_scope(a, symbol)
	}
}

infer_globals :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	assert(node_index != parser.INVALID_NODE)

	node := a.nodes[node_index]
	token := a.tokens[node.token]

	if node.kind == .Variable {
		var_name := a.source[token.start:token.end]
		var_decl := parser.decode_data(a.node_data, node.data, parser.VarDecl)

		if symbol_index, ok := lookup_symbol(a, var_name); ok {
			symbol := &a.symbols[symbol_index]

			type := check_node(a, var_decl.value, .ExprValue)

			symbol.type = type
			symbol.state = .Typed
		}
	}
}

check_node :: proc(a: ^Analyzer, node_index: parser.NodeIndex, ctx: NodeContext) -> TypeIndex {
	assert(node_index != parser.INVALID_NODE)

	node := a.nodes[node_index]
	token := a.tokens[node.token]

	switch node.kind {
	case .True, .False:
		return TypeIndex(BaseType.B32)

	case .Integer:
		return TypeIndex(BaseType.S32)

	case .Float:
		return TypeIndex(BaseType.F32)

	case .Identifier:
		name := a.source[token.start:token.end]

		symbol_index, ok := lookup_symbol(a, name)

		if !ok {
			add_error(a, fmt.tprintf("undeclared indentifier, %v", name), node_index)
			return INVALID_TYPE
		}

		symbol := a.symbols[symbol_index]

		if symbol.state != .Typed {
			add_error(a, fmt.tprintf("use of untyped global %v", name), node_index)
			return INVALID_TYPE
		}

		if symbol.kind == .Procedure && ctx == .ExprValue {
			add_error(a, "procedure used as a value", node_index)
			return INVALID_TYPE
		}

		return symbol.type
	case .Call:
		call_expr := parser.decode_data(a.node_data, node.data, parser.CallExpr)
		callee_type_index := check_node(a, call_expr.callee, .ExprCallee)
		if callee_type_index == INVALID_TYPE {
			return INVALID_TYPE
		}

		callee_type := a.types[callee_type_index]
		if callee_type.kind != .Procedure {
			add_error(a, "calling non-procedure")
			return INVALID_TYPE
		}

		if len(call_expr.arguments) != len(callee_type.procedure.param_types) {
			add_error(a, "argument count mismatch")
		}

		// todo: check each value in the slices

		if ctx == .ExprValue {
			return callee_type.procedure.return_type
		}

		return INVALID_TYPE

	case .Variable:
		var_name := a.source[token.start:token.end]
		var_decl := parser.decode_data(a.node_data, node.data, parser.VarDecl)

		if var_decl.value == parser.INVALID_NODE {
			return INVALID_TYPE
		}

		var_type := check_node(a, var_decl.value, .ExprValue)

		symbol := Symbol {
			kind  = .Variable,
			name  = get_or_add_string(a, var_name),
			type  = var_type,
			state = .Typed,
		}

		add_symbol_to_current_scope(a, symbol)

		return INVALID_TYPE
	case .Assignment:
		assign_expr := parser.decode_data(a.node_data, node.data, parser.AssignExpr)

		left := check_node(a, assign_expr.left, .ExprValue)
		right := check_node(a, assign_expr.right, .ExprValue)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		return left
	case .Addition:
		add_expr := parser.decode_data(a.node_data, node.data, parser.AddExpr)

		left := check_node(a, add_expr.left, .ExprValue)
		right := check_node(a, add_expr.right, .ExprValue)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		return left
	case .Multiplication:
		mul_expr := parser.decode_data(a.node_data, node.data, parser.MulExpr)

		left := check_node(a, mul_expr.left, .ExprValue)
		right := check_node(a, mul_expr.right, .ExprValue)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		return left
	case .Equal:
		assign_expr := parser.decode_data(a.node_data, node.data, parser.EqualExpr)

		left := check_node(a, assign_expr.left, .ExprValue)
		right := check_node(a, assign_expr.right, .ExprValue)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		return TypeIndex(BaseType.B32)
	case .Less:
		less_expr := parser.decode_data(a.node_data, node.data, parser.LessExpr)

		left := check_node(a, less_expr.left, .ExprValue)
		right := check_node(a, less_expr.right, .ExprValue)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		return TypeIndex(BaseType.B32)
	case .Block:
		append(&a.scopes, make(Scope))

		block_stmt := parser.decode_data(a.node_data, node.data, parser.BlockStmt)

		for stmt in block_stmt.statements {
			declare_procedures(a, stmt)
		}

		for stmt in block_stmt.statements {
			check_node(a, stmt, .Stmt)
		}

		pop(&a.scopes)

		return INVALID_TYPE
	case .Return:
		if len(a.return_type_stack) == 0 {
			add_error(a, "return outside procedure", node_index)
			return INVALID_TYPE
		}

		expected_type := a.return_type_stack[len(a.return_type_stack) - 1]

		return_stmt := parser.decode_data(a.node_data, node.data, parser.ReturnStmt)

		if return_stmt.value == parser.INVALID_NODE && expected_type != TypeIndex(BaseType.Nil) {
			add_error(a, "missing return value", node_index)
			return INVALID_TYPE
		}

		if expected_type == TypeIndex(BaseType.Nil) {
			return INVALID_TYPE
		}

		expr_type := check_node(a, return_stmt.value, .ExprValue)
		if expected_type != expr_type {
			add_error(
				a,
				fmt.tprintf("mismatch type %v != %v", expected_type, expr_type),
				node_index,
			)
			return INVALID_TYPE
		}

		return expected_type
	case .Procedure:
		enter_procedure(a, node)

		return INVALID_TYPE
	case .ExprStmt:
		expr_stmt := parser.decode_data(a.node_data, node.data, parser.ExprStmt)
		check_node(a, expr_stmt.inner, .ExprDiscard)
		return INVALID_TYPE
	case .If:
		if_expr := parser.decode_data(a.node_data, node.data, parser.IfExpr)

		condition := check_node(a, if_expr.condition, .ExprValue)
		if condition != TypeIndex(BaseType.B32) {
			add_error(a, "condition must be boolean")
		}

		check_node(a, if_expr.then_body, .Stmt)
		if if_expr.else_body != parser.INVALID_NODE {
			check_node(a, if_expr.else_body, .Stmt)
		}

		return INVALID_TYPE
	case .For:
		for_stmt := parser.decode_data(a.node_data, node.data, parser.ForStmt)

		append(&a.scopes, make(Scope))

		check_node(a, for_stmt.initial, .Stmt)

		condition := check_node(a, for_stmt.condition, .ExprValue)
		if condition != TypeIndex(BaseType.B32) {
			add_error(a, "condition must be boolean")
		}

		check_node(a, for_stmt.update, .Stmt)
		check_node(a, for_stmt.body, .Stmt)

		pop(&a.scopes)

		return INVALID_TYPE
	case .Invalid, .Module, .Parameter:
		panic(fmt.tprintf("unhandled process, %v", node.kind))
	}

	return INVALID_TYPE
}

analyze :: proc(a: ^Analyzer) {
	root_index := parser.NodeIndex(len(a.nodes) - 1)
	root := a.nodes[root_index]
	module_decl := parser.decode_data(a.node_data, root.data, parser.ModuleDecl)

	append(&a.scopes, make(Scope))

	for stmt in module_decl.statements {
		declare_globals(a, stmt)
	}

	for stmt in module_decl.statements {
		infer_globals(a, stmt)
	}

	for stmt in module_decl.statements {
		node := a.nodes[stmt]
		if node.kind == .Procedure {
			enter_procedure(a, node)
		}
	}

	pop(&a.scopes)
}
