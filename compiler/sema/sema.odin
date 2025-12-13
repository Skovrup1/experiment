package sema

import "../lexer"
import "../parser"

import "core:fmt"
import "core:slice"

BaseType :: enum u8 {
	Nil,
	B32,
	U32,
	S32,
}

base_type_strings := [?]string{"Nil", "B32", "U32", "S32"}

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
	Type,
}

Symbol :: struct {
	kind: SymbolKind,
	name: StringIndex,
	type: TypeIndex,
}

Scope :: map[StringIndex]SymbolIndex

StringIndex :: distinct u32
INVALID_STRING :: max(StringIndex)

TypeIndex :: distinct u32
INVALID_TYPE :: max(TypeIndex)

SymbolIndex :: distinct u32
INVALID_SYMBOL :: max(SymbolIndex)

Analyzer :: struct {
	source:            string,
	tokens:            []lexer.Token,
	nodes:             []parser.Node,
	node_data:         []u32,
	types:             [dynamic]Type,
	type_map:          map[u64]TypeIndex,
	return_type_stack: [dynamic]TypeIndex,
	node_types:        []TypeIndex,
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
	errors := make([dynamic]SemaError)

	types := make([dynamic]Type)
	type_map := make(map[u64]TypeIndex)
	return_type_stack := make([dynamic]TypeIndex, 0, 16)
	node_types := make([]TypeIndex, len(nodes))

	strings := make([dynamic]string)
	string_map := make(map[string]StringIndex)

	symbols := make([dynamic]Symbol)
	scopes := make([dynamic]Scope, 0, 16)

	a := Analyzer {
		source,
		tokens,
		nodes,
		node_data,
		types,
		type_map,
		return_type_stack,
		node_types,
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

	return a
}

check :: proc(
	a: ^Analyzer,
	node_index: parser.NodeIndex,
	expected_type: TypeIndex,
	loc := #caller_location,
) {
	node := a.nodes[node_index]

	inferred_type := infer(a, node_index)
	if inferred_type != expected_type {
		panic(
			fmt.tprintf(
				"type mismatch: expected type '%v', but got '%v' at %v\n",
				expected_type,
				inferred_type,
				loc,
			),
		)
	}
}

get_type_from_name :: proc(name: string) -> (TypeIndex, bool) {
	for _, index in BaseType {
		if base_type_strings[index] == name {
			return TypeIndex(index), true
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

infer :: proc(a: ^Analyzer, node_index: parser.NodeIndex) -> (inferred: TypeIndex) {
	node := a.nodes[node_index]
	token := a.tokens[node.token]

	#partial switch node.kind {
	//case .Bool: // untyped bool
	case .Integer:
		inferred = TypeIndex(BaseType.S32) // untyped integer
	//case .Float: // untyped float
	case .Identifier:
		name := a.source[token.start:token.end]

		if type_index, ok := get_type_from_name(name); ok {
			inferred = type_index
		} else if symbol_index, ok := lookup_symbol(a, name); ok {
			inferred = a.symbols[symbol_index].type
		} else {
			add_error(a, "not able to infer the type", node_index)
		}
	case:
		add_error(a, fmt.tprintf("not able to infer this type = %v", node.kind), node_index)
	}

	a.node_types[node_index] = inferred
	return inferred
}

add_error :: proc(a: ^Analyzer, message: string, position: parser.NodeIndex) {
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

get_base_type :: proc(name: string) -> (BaseType, bool) {
	switch name {
	case "B32":
		return BaseType.B32, true
	case "U32":
		return BaseType.U32, true
	case "S32":
		return BaseType.S32, true
	}

	return BaseType.Nil, false
}

lookup_type :: proc(a: Analyzer, node_index: parser.NodeIndex) -> (TypeIndex, bool) {
	if (node_index == parser.INVALID_NODE) {
		return INVALID_TYPE, false
	}

	node := a.nodes[node_index]
	#partial switch node.kind {
	case .Primitive:
		token := a.tokens[node.token]
		name := a.source[token.start:token.end]
		if type_kind, ok := get_base_type(name); ok {
			return TypeIndex(type_kind), true
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

add_symbol_to_current_scope :: proc(a: ^Analyzer, symbol: Symbol) {
	append(&a.symbols, symbol)
	symbol_index := SymbolIndex(len(a.symbols) - 1)
	current_scope := &a.scopes[len(a.scopes) - 1]
	current_scope[symbol.name] = symbol_index
}

collect :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	node := a.nodes[node_index]
	token := a.tokens[node.token]

	#partial switch node.kind {
	case .Parameter:
		param_name := a.source[token.start:token.end]

		param_decl := parser.decode_data(a.node_data, node.data, parser.ParamDecl)

		type, ok := lookup_type(a^, param_decl.type)
		if !ok {
			type = infer(a, param_decl.value)
		}

		symbol := Symbol{.Parameter, get_or_add_string(a, param_name), type}
		add_symbol_to_current_scope(a, symbol)
	case .Variable:
		var_name := a.source[token.start:token.end]

		var_decl := parser.decode_data(a.node_data, node.data, parser.VarDecl)

		type, ok := lookup_type(a^, var_decl.type)
		if !ok {
			type = infer(a, var_decl.value)
		}

		symbol := Symbol{.Variable, get_or_add_string(a, var_name), type}
		add_symbol_to_current_scope(a, symbol)
	case .Return:
		expected_type := pop(&a.return_type_stack)

		return_stmt := parser.decode_data(a.node_data, node.data, parser.ReturnStmt)

		if return_stmt.value != parser.INVALID_NODE {
			check(a, return_stmt.value, expected_type)
		} else {
			panic("error: missing return value")
		}
	case .Procedure:
		proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)
		proc_token := a.tokens[proc_decl.name]
		proc_name := a.source[proc_token.start:proc_token.end]

		param_types := make([dynamic]TypeIndex, 0, len(proc_decl.parameters))
		param_names := make([dynamic]StringIndex, 0, len(proc_decl.parameters))
		for param_node_index in proc_decl.parameters {
			param_node := a.nodes[param_node_index]
			assert(param_node.kind == .Parameter)

			param_decl := parser.decode_data(a.node_data, param_node.data, parser.ParamDecl)

			param_token := a.tokens[param_node.token]
			param_name := a.source[param_token.start:param_token.end]
			append(&param_names, get_or_add_string(a, param_name))

			param_type, param_ok := lookup_type(a^, param_decl.type)
			if !param_ok && param_decl.value != parser.INVALID_NODE {
				panic("todo param value type-inference")
			}
			append(&param_types, param_type)
		}

		return_type, return_ok := lookup_type(a^, proc_decl.return_type)
		if !return_ok {
			panic("missing return type!")
		}

		append(&a.return_type_stack, return_type)

		type_index := get_or_add_type(
			a,
			{.Procedure, {procedure = {return_type, param_names[:], param_types[:]}}},
		)

		symbol := Symbol{.Procedure, get_or_add_string(a, proc_name), type_index}
		add_symbol_to_current_scope(a, symbol)
	case:
		panic(fmt.tprintf("unhandled collect, %v", node.kind))
	}
}

process :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	assert(node_index != parser.INVALID_NODE)

	node := a.nodes[node_index]
	token := a.tokens[node.token]

	#partial switch node.kind {
	case .Block:
		append(&a.scopes, make(Scope))

		block_stmt := parser.decode_data(a.node_data, node.data, parser.BlockStmt)

		for stmt in block_stmt.statements {
			process(a, stmt)
		}

		pop(&a.scopes)
	case .Return:
		collect(a, node_index)
	case .Parameter:
		panic("todo param")
	case .Variable:
		collect(a, node_index)
	case .Procedure:
		append(&a.scopes, make(Scope))

		proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)

		for param_node_index in proc_decl.parameters {
			collect(a, param_node_index)
		}

		process(a, proc_decl.body)

		pop(&a.scopes)
	case:
		panic(fmt.tprintf("unhandled process, %v", node.kind))
	}
}

analyze :: proc(a: ^Analyzer) {
	root_index := parser.NodeIndex(len(a.nodes) - 1)
	append(&a.scopes, make(Scope))
	collect(a, root_index)
	process(a, root_index)
}
