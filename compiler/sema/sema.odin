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
	types := make([dynamic]Type)
	type_map := make(map[u64]TypeIndex)
	return_type_stack := make([dynamic]TypeIndex, 0, 16)
	node_types := make([]TypeIndex, len(nodes))

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
		add_error(
			a,
			fmt.tprintf("type mismatch %v != %v at %v", expected_type, inferred_type, loc),
			node_index,
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
        case .True, .False:
                inferred = TypeIndex(BaseType.B32) // untyped bool
        case .Integer:
                inferred = TypeIndex(BaseType.S32) // untyped integer
        case .Float:
                inferred = TypeIndex(BaseType.F32) // untyped float
        case .If:
                if_expr := parser.decode_data(a.node_data, node.data, parser.IfExpr)

                check(a, if_expr.condition, TypeIndex(BaseType.B32))

                then_type := infer(a, if_expr.then_body)

                if if_expr.else_body != parser.INVALID_NODE {
                        else_type := infer(a, if_expr.else_body)

                        if then_type != else_type {
                                add_error(a, "type mismatch in if expression branches", node_index)
                        }

                        inferred = then_type
                } else {
                        inferred = TypeIndex(BaseType.Nil)
                }
        case .Identifier:
                name := a.source[token.start:token.end]

		//if type_index, ok := get_type_from_name(name); ok {}

		if symbol_index, ok := lookup_symbol(a, name); ok {
			inferred = a.symbols[symbol_index].type
		} else {
			add_error(a, fmt.tprintf("undeclared indentifier, %v", name), node_index)
		}
	case .Call:
		call_expr := parser.decode_data(a.node_data, node.data, parser.CallExpr)

		callee_type_index := infer(a, call_expr.callee)
		callee_type := a.types[callee_type_index]

		#partial switch callee_type.kind {
		case .Primitive:
			panic("callee primitive")
		case .Procedure:
			panic("callee procedure")
		case:
			panic(fmt.tprintf("unhandled callee_type, %v", callee_type.kind))
		}
	case .Assignment:
		assign_expr := parser.decode_data(a.node_data, node.data, parser.AssignExpr)

		right := infer(a, assign_expr.right)
		check(a, assign_expr.left, right)

		inferred = right
	case .Addition:
		add_expr := parser.decode_data(a.node_data, node.data, parser.AddExpr)

		right := infer(a, add_expr.right)
		check(a, add_expr.left, right)

		inferred = right
	case .Multiplication:
		mul_expr := parser.decode_data(a.node_data, node.data, parser.MulExpr)

		right := infer(a, mul_expr.right)
		check(a, mul_expr.left, right)

		inferred = right
	case .Equal:
		equal_expr := parser.decode_data(a.node_data, node.data, parser.EqualExpr)

		right := infer(a, equal_expr.right)
		check(a, equal_expr.left, right)

		inferred = TypeIndex(BaseType.B32)
	case .Less:
		less_expr := parser.decode_data(a.node_data, node.data, parser.LessExpr)

		right := infer(a, less_expr.right)
		check(a, less_expr.left, right)

		inferred = TypeIndex(BaseType.B32)
	case:
		panic(fmt.tprintf("unhandled infer, %v", node.kind))
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

lookup_type :: proc(a: Analyzer, node_index: parser.NodeIndex) -> (TypeIndex, bool) {
	if (node_index == parser.INVALID_NODE) {
		return INVALID_TYPE, false
	}

	node := a.nodes[node_index]
	#partial switch node.kind {
	case .Identifier:
		token := a.tokens[node.token]
		name := a.source[token.start:token.end]
		if type_kind, ok := get_type_from_name(name); ok {
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
                        if param_decl.value == parser.INVALID_NODE {
                                add_error(a, "parameter missing type", node_index)
                                type = INVALID_TYPE
                        } else {
                                type = infer(a, param_decl.value)
                        }
                }

                if param_decl.value != parser.INVALID_NODE && type != INVALID_TYPE {
                        check(a, param_decl.value, type)
                }

                symbol := Symbol{.Parameter, get_or_add_string(a, param_name), type}
                add_symbol_to_current_scope(a, symbol)
        case .Variable:
                var_name := a.source[token.start:token.end]

                var_decl := parser.decode_data(a.node_data, node.data, parser.VarDecl)

                type, ok := lookup_type(a^, var_decl.type)
                if !ok {
                        if var_decl.value == parser.INVALID_NODE {
                                add_error(a, "variable missing type", node_index)
                                type = INVALID_TYPE
                        } else {
                                type = infer(a, var_decl.value)
                        }
                }

                if var_decl.value != parser.INVALID_NODE && type != INVALID_TYPE {
                        check(a, var_decl.value, type)
                }

                symbol := Symbol{.Variable, get_or_add_string(a, var_name), type}
                add_symbol_to_current_scope(a, symbol)
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
                        add_error(a, "missing return type!", node_index)
                }

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
        case .Variable:
                name := a.source[token.start:token.end]
                current_scope := a.scopes[len(a.scopes) - 1]

                if string_index, exists := a.string_map[name]; exists {
                        if _, exists := current_scope[string_index]; exists {
                                return
                        }
                }

                collect(a, node_index)
        case .Return:
                if len(a.return_type_stack) == 0 {
                        add_error(a, "return outside of procedure", node_index)
                        return
                }

                expected_type := a.return_type_stack[len(a.return_type_stack) - 1]

                return_stmt := parser.decode_data(a.node_data, node.data, parser.ReturnStmt)

                if return_stmt.value != parser.INVALID_NODE {
                        check(a, return_stmt.value, expected_type)
                } else {
                        add_error(a, "missing return value", node_index)
                }
        case .Procedure:
                append(&a.scopes, make(Scope))

                proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)

                return_type, _ := lookup_type(a^, proc_decl.return_type)
                append(&a.return_type_stack, return_type)

                for param_node_index in proc_decl.parameters {
                        collect(a, param_node_index)
                }

                process(a, proc_decl.body)

                pop(&a.return_type_stack)
                pop(&a.scopes)
        case .If:
                if_expr := parser.decode_data(a.node_data, node.data, parser.IfExpr)

                check(a, if_expr.condition, TypeIndex(BaseType.B32))

                process(a, if_expr.then_body)

                if if_expr.else_body != parser.INVALID_NODE {
                        process(a, if_expr.else_body)
                }
        case .Parameter:
        case:
                panic(fmt.tprintf("unhandled process, %v", node.kind))
        }
}

analyze :: proc(a: ^Analyzer) {
        append(&a.scopes, make(Scope))

        referenced := make([]bool, len(a.nodes))

        mark_child :: proc(index: parser.NodeIndex) {
                if index != parser.INVALID_NODE {
                        referenced[index] = true
                }
        }

        for node_index, node in a.nodes {
                #partial switch node.kind {
                case .Block:
                        block_stmt := parser.decode_data(a.node_data, node.data, parser.BlockStmt)

                        for stmt in block_stmt.statements {
                                mark_child(stmt)
                        }
                case .Variable:
                        var_decl := parser.decode_data(a.node_data, node.data, parser.VarDecl)

                        mark_child(var_decl.type)
                        mark_child(var_decl.value)
                case .Parameter:
                        param_decl := parser.decode_data(a.node_data, node.data, parser.ParamDecl)

                        mark_child(param_decl.type)
                        mark_child(param_decl.value)
                case .Procedure:
                        proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)

                        mark_child(proc_decl.return_type)
                        mark_child(proc_decl.body)

                        for param in proc_decl.parameters {
                                mark_child(param)
                        }
                case .If:
                        if_expr := parser.decode_data(a.node_data, node.data, parser.IfExpr)

                        mark_child(if_expr.condition)
                        mark_child(if_expr.then_body)
                        mark_child(if_expr.else_body)
                case .For:
                        for_stmt := parser.decode_data(a.node_data, node.data, parser.ForStmt)

                        mark_child(for_stmt.initial)
                        mark_child(for_stmt.condition)
                        mark_child(for_stmt.update)
                        mark_child(for_stmt.body)
                case .Return:
                        return_stmt := parser.decode_data(a.node_data, node.data, parser.ReturnStmt)

                        mark_child(return_stmt.value)
                case .ExprStmt:
                        expr_stmt := parser.decode_data(a.node_data, node.data, parser.ExprStmt)

                        mark_child(expr_stmt.expression)
                case .Call:
                        call_expr := parser.decode_data(a.node_data, node.data, parser.CallExpr)

                        mark_child(call_expr.callee)

                        for arg in call_expr.arguments {
                                mark_child(arg)
                        }
                case .Assignment:
                        assign_expr := parser.decode_data(a.node_data, node.data, parser.AssignExpr)

                        mark_child(assign_expr.left)
                        mark_child(assign_expr.right)
                case .Addition:
                        add_expr := parser.decode_data(a.node_data, node.data, parser.AddExpr)

                        mark_child(add_expr.left)
                        mark_child(add_expr.right)
                case .Multiplication:
                        mul_expr := parser.decode_data(a.node_data, node.data, parser.MulExpr)

                        mark_child(mul_expr.left)
                        mark_child(mul_expr.right)
                case .Equal:
                        equal_expr := parser.decode_data(a.node_data, node.data, parser.EqualExpr)

                        mark_child(equal_expr.left)
                        mark_child(equal_expr.right)
                case .Less:
                        less_expr := parser.decode_data(a.node_data, node.data, parser.LessExpr)

                        mark_child(less_expr.left)
                        mark_child(less_expr.right)
                case:
                        // literals and identifiers have no children
                }
        }

        top_level_nodes := make([dynamic]parser.NodeIndex)
        for node_index in 0 ..< len(a.nodes) {
                        if !referenced[node_index] {
                                append(&top_level_nodes, parser.NodeIndex(node_index))
                        }
        }

        for top_node in top_level_nodes {
                node := a.nodes[top_node]

                if node.kind == .Procedure {
                        collect(a, top_node)
                }
        }

        for top_node in top_level_nodes {
                node := a.nodes[top_node]

                if node.kind == .Variable {
                        collect(a, top_node)
                }
        }

        for top_node in top_level_nodes {
                process(a, top_node)
        }
}
