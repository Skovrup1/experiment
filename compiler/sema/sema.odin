package sema

import "core:fmt"
import "core:slice"
import "core:strconv"
import "core:strings"

import "../lexer"
import "../parser"

BaseType :: enum u8 {
	Null = 0,
	B32,
	U32,
	S32,
	F32,
}

base_type_strings := [?]string{"Null", "B32", "U32", "S32", "F32"}

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

SymbolKind :: enum u8 {
	Variable,
	Parameter,
	Procedure,
}

Symbol :: struct {
	kind:  SymbolKind,
	name:  StringIndex,
	type:  TypeIndex,
	value: InstID,
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
	strings:           [dynamic]string,
	string_map:        map[string]StringIndex,
	symbols:           [dynamic]Symbol,
	scopes:            [dynamic]Scope,
	errors:            [dynamic]string,
	functions:         [dynamic]Function,
	current_func:      ^Function,
	current_block:     ^Block,
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

	errors := make([dynamic]string)

	functions := make([dynamic]Function)

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
		functions,
		nil,
		nil,
	}

	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.Null}}})
	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.B32}}})
	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.U32}}})
	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.S32}}})
	get_or_add_type(&a, {.Primitive, {primitive = {BaseType.F32}}})

	return a
}

add_error :: proc(a: ^Analyzer, message: string) {
	append(&a.errors, message)
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
		add_error(a, fmt.tprintf("redeclaration of %v", symbol.name))
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
		return_type = TypeIndex(BaseType.Null)
	} else {
		return_type = lookup_type(a^, proc_decl.return_type)
	}

	proc_type := get_or_add_type(
		a,
		{.Procedure, {procedure = {return_type, param_names[:], param_types[:]}}},
	)

	return Symbol{.Procedure, get_or_add_string(a, proc_name), proc_type, INVALID_INST}
}

enter_procedure :: proc(a: ^Analyzer, node: parser.Node) {
	proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)
	proc_token := a.tokens[node.token]
	proc_name := a.source[proc_token.start:proc_token.end]

	if symbol_index, ok := lookup_symbol(a, proc_name); ok {
		symbol := a.symbols[symbol_index]
		proc_type := a.types[symbol.type]
		assert(proc_type.kind == .Procedure)

		prev_func := a.current_func
		prev_block := a.current_block

		append(&a.scopes, make(Scope))
		append(&a.return_type_stack, proc_type.procedure.return_type)

		append(&a.functions, Function{name = proc_name})
		a.current_func = &a.functions[len(a.functions) - 1]

		append(&a.current_func.blocks, Block{name = proc_name})
		a.current_block = &a.current_func.blocks[len(a.current_func.blocks) - 1]
		a.current_block.insts = make([dynamic]InstID)

		for param_name, i in proc_type.procedure.param_names {
			param_type := proc_type.procedure.param_types[i]

			symbol := Symbol{.Parameter, param_name, param_type, INVALID_INST}

			symbol_index := add_symbol_to_current_scope(a, symbol)

			a.symbols[symbol_index].value = emit_inst(a, Inst{kind = .Param, value = i64(i)})
		}

		check_empty(a, proc_decl.body)

		a.current_block = prev_block
		a.current_func = prev_func

		pop(&a.scopes)
		pop(&a.return_type_stack)
	} else {
		panic("failed to enter procedure")
	}
}

declare_globals :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	node := a.nodes[node_index]
	token := a.tokens[node.token]

	#partial switch node.kind {
	case .Variable:
		var_name := a.source[token.start:token.end]

		symbol := Symbol{.Variable, get_or_add_string(a, var_name), INVALID_TYPE, INVALID_INST}

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

	node := a.nodes[node_index]

	if node.kind == .Procedure {
		proc_decl := parser.decode_data(a.node_data, node.data, parser.ProcDecl)
		symbol := build_procedure_symbol(a, proc_decl)

		add_symbol_to_current_scope(a, symbol)
	}
}

infer_globals :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	node := a.nodes[node_index]
	token := a.tokens[node.token]

	if node.kind == .Variable {
		var_name := a.source[token.start:token.end]
		var_decl := parser.decode_data(a.node_data, node.data, parser.VarDecl)

		if symbol_index, ok := lookup_symbol(a, var_name); ok {
			symbol := &a.symbols[symbol_index]

			_, type := check_value(a, var_decl.value)

			symbol.type = type
		}
	}
}

check_value :: proc(
	a: ^Analyzer,
	node_index: parser.NodeIndex,
) -> (
	address: InstID,
	type: TypeIndex,
) {
	node := a.nodes[node_index]
	token := a.tokens[node.token]
	address = INVALID_INST
	type = INVALID_TYPE

	#partial switch node.kind {
	case .True:
		type = TypeIndex(BaseType.B32)
		value := i64(0)
		address = emit_inst(a, Inst{kind = .Imm, type = type, value = 1})
	case .False:
		type = TypeIndex(BaseType.B32)
		address = emit_inst(a, Inst{kind = .Imm, type = type, value = 0})
	case .Integer:
		name := a.source[token.start:token.end]

		type = TypeIndex(BaseType.S32)

		value, ok := strconv.parse_i64_of_base(name, 10)
		ensure(ok)

		address = emit_inst(a, Inst{kind = .Imm, type = type, value = value})
	case .Float:
		name := a.source[token.start:token.end]
		type = TypeIndex(BaseType.F32)
		value, ok := strconv.parse_f64(name)
		ensure(ok)
		address = emit_inst(a, Inst{kind = .Imm, type = type, value = i64(value)})
	case .Identifier:
		name := a.source[token.start:token.end]

		symbol_index, ok := lookup_symbol(a, name)
		if !ok {
			add_error(a, fmt.tprintf("undeclared indentifier, %v", name))
			return
		}

		symbol := a.symbols[symbol_index]

		type = symbol.type

		load_args := make([]InstID, 1)
		load_args[0] = symbol.value

		address = emit_inst(
			a,
			Inst{kind = .Load, args = load_args, type = type, value = i64(symbol_index)},
		)
	case .Call:
		call_expr := parser.decode_data(a.node_data, node.data, parser.CallExpr)
		callee_addr, callee_type_index := check_value(a, call_expr.callee)
		if callee_type_index == INVALID_TYPE {
			return
		}

		callee_type := a.types[callee_type_index]
		if callee_type.kind != .Procedure {
			add_error(a, "calling non-procedure")
			return
		}

		if len(call_expr.arguments) != len(callee_type.procedure.param_types) {
			add_error(a, "argument count mismatch")
		}

		arg_addrs := make([dynamic]InstID, 0, len(call_expr.arguments))
		for arg_node in call_expr.arguments {
			arg_addr, arg_type := check_value(a, arg_node)
			append(&arg_addrs, arg_addr)
			// todo: proper type checking per argument
			_ = arg_type
		}

		type = callee_type.procedure.return_type

		call_args := make([]InstID, 1 + len(arg_addrs))
		call_args[0] = callee_addr
		for i in 0 ..< len(arg_addrs) {
			call_args[i + 1] = arg_addrs[i]
		}

		address = emit_inst(
			a,
			Inst {
				kind = .Call,
				args = call_args,
				type = type,
				value = i64(len(call_expr.arguments)),
			},
		)
	case .Assignment:
		assign_expr := parser.decode_data(a.node_data, node.data, parser.AssignExpr)

		left_node := a.nodes[assign_expr.left]
		if left_node.kind != .Identifier {
			add_error(a, "left-hand side of assignment must be identifier")
			return
		}

		left_token := a.tokens[left_node.token]
		left_name := a.source[left_token.start:left_token.end]

		left_symbol_index, ok := lookup_symbol(a, left_name)
		if !ok {
			add_error(a, fmt.tprintf("undeclared indentifier, %v", left_name))
			return
		}

		left_symbol := &a.symbols[left_symbol_index]
		left := left_symbol.type

		addr_r, right := check_value(a, assign_expr.right)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		type = left

		left_symbol.value = addr_r

		store_args := make([]InstID, 1)
		store_args[0] = addr_r

		emit_inst(
			a,
			Inst {
				kind = .Store,
				args = store_args,
				type = left_symbol.type,
				value = i64(left_symbol_index),
			},
		)
		address = addr_r
	case .Addition:
		add_expr := parser.decode_data(a.node_data, node.data, parser.AddExpr)

		addr_l, left := check_value(a, add_expr.left)
		addr_r, right := check_value(a, add_expr.right)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		type = left

		add_args := make([]InstID, 2)
		add_args[0] = addr_l
		add_args[1] = addr_r
		address = emit_inst(a, Inst{kind = .Add, args = add_args, type = type})
	case .Multiplication:
		mul_expr := parser.decode_data(a.node_data, node.data, parser.MulExpr)

		addr_l, left := check_value(a, mul_expr.left)
		addr_r, right := check_value(a, mul_expr.right)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		type = left

		mul_args := make([]InstID, 2)
		mul_args[0] = addr_l
		mul_args[1] = addr_r
		address = emit_inst(a, Inst{kind = .Mul, args = mul_args, type = type})
	case .Equal:
		assign_expr := parser.decode_data(a.node_data, node.data, parser.EqualExpr)

		addr_l, left := check_value(a, assign_expr.left)
		addr_r, right := check_value(a, assign_expr.right)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		type = TypeIndex(BaseType.B32)
		eq_args := make([]InstID, 2)
		eq_args[0] = addr_l
		eq_args[1] = addr_r
		address = emit_inst(a, Inst{kind = .Equal, args = eq_args, type = type})
	case .Less:
		less_expr := parser.decode_data(a.node_data, node.data, parser.LessExpr)

		addr_l, left := check_value(a, less_expr.left)
		addr_r, right := check_value(a, less_expr.right)

		if left != right {
			add_error(a, "assignment mismatch")
		}

		type = TypeIndex(BaseType.B32)
		lt_args := make([]InstID, 2)
		lt_args[0] = addr_l
		lt_args[1] = addr_r
		address = emit_inst(a, Inst{kind = .Less, args = lt_args, type = type})
	case:
		panic("unhandled")
	}

	return
}

check_empty :: proc(a: ^Analyzer, node_index: parser.NodeIndex) -> (address: InstID) {
	node := a.nodes[node_index]
	token := a.tokens[node.token]
	address = INVALID_INST

	#partial switch node.kind {
	case .Variable:
		var_name := a.source[token.start:token.end]
		var_decl := parser.decode_data(a.node_data, node.data, parser.VarDecl)

		var_type := INVALID_TYPE
		addr := INVALID_INST
		if var_decl.value != parser.INVALID_NODE {
			addr, var_type = check_value(a, var_decl.value)
		}

		symbol := Symbol {
			kind  = .Variable,
			name  = get_or_add_string(a, var_name),
			type  = var_type,
			value = INVALID_INST,
		}

		symbol_index := add_symbol_to_current_scope(a, symbol)
		a.symbols[symbol_index].value = addr
	case .Procedure:
		enter_procedure(a, node)
	case .Block:
		append(&a.scopes, make(Scope))

		block_stmt := parser.decode_data(a.node_data, node.data, parser.BlockStmt)

		for stmt in block_stmt.statements {
			declare_procedures(a, stmt)
		}

		for stmt in block_stmt.statements {
			check_empty(a, stmt)
		}

		pop(&a.scopes)
	case .Return:
		if len(a.return_type_stack) == 0 {
			add_error(a, "return outside procedure")
			return
		}

		expected_type := a.return_type_stack[len(a.return_type_stack) - 1]

		return_stmt := parser.decode_data(a.node_data, node.data, parser.ReturnStmt)

		if return_stmt.value == parser.INVALID_NODE && expected_type != TypeIndex(BaseType.Null) {
			add_error(a, "missing return value")
		}

		if expected_type == TypeIndex(BaseType.Null) {
			return
		}

		addr, expr_type := check_value(a, return_stmt.value)
		if expected_type != expr_type {
			add_error(a, fmt.tprintf("mismatch type %v != %v", expected_type, expr_type))
		}

		ret_args := make([]InstID, 1)
		ret_args[0] = addr
		return emit_inst(a, Inst{kind = .Return, args = ret_args, type = expected_type})

	case .For:
		for_stmt := parser.decode_data(a.node_data, node.data, parser.ForStmt)

		append(&a.scopes, make(Scope))

		check_empty(a, for_stmt.initial)

		_, condition := check_value(a, for_stmt.condition)
		if condition != TypeIndex(BaseType.B32) {
			add_error(a, "condition must be boolean")
		}

		check_empty(a, for_stmt.update)
		check_empty(a, for_stmt.body)

		pop(&a.scopes)
	case .If:
		if_expr := parser.decode_data(a.node_data, node.data, parser.IfExpr)

		cond_addr, condition := check_value(a, if_expr.condition)
		if condition != TypeIndex(BaseType.B32) {
			add_error(a, "condition must be boolean")
		}

		then_index := BlockIndex(len(a.current_func.blocks))
		append(
			&a.current_func.blocks,
			Block{name = strings.concatenate({a.current_func.name, ".then"})},
		)
		a.current_func.blocks[then_index].insts = make([dynamic]InstID)

		else_index := BlockIndex(len(a.current_func.blocks))
		append(
			&a.current_func.blocks,
			Block{name = strings.concatenate({a.current_func.name, ".else"})},
		)
		a.current_func.blocks[else_index].insts = make([dynamic]InstID)

		merge_index := BlockIndex(len(a.current_func.blocks))
		append(
			&a.current_func.blocks,
			Block{name = strings.concatenate({a.current_func.name, ".merge"})},
		)
		a.current_func.blocks[merge_index].insts = make([dynamic]InstID)

		branch_args := make([]InstID, 1)
		branch_args[0] = cond_addr
		branch_blocks := make([]BlockIndex, 2)
		branch_blocks[0] = then_index
		branch_blocks[1] = else_index
		emit_inst(
			a,
			Inst{kind = .Branch, args = branch_args, type = condition, blocks = branch_blocks},
		)

		a.current_block = &a.current_func.blocks[then_index]
		check_empty(a, if_expr.then_body)

		if block_needs_terminator(a, a.current_block^) {
			jump_blocks := make([]BlockIndex, 1)
			jump_blocks[0] = merge_index
			emit_inst(a, Inst{kind = .Jump, blocks = jump_blocks})
		}

		a.current_block = &a.current_func.blocks[else_index]
		if if_expr.else_body != parser.INVALID_NODE {
			check_empty(a, if_expr.else_body)
		}

		if block_needs_terminator(a, a.current_block^) {
			jump_blocks := make([]BlockIndex, 1)
			jump_blocks[0] = merge_index
			emit_inst(a, Inst{kind = .Jump, blocks = jump_blocks})
		}

		a.current_block = &a.current_func.blocks[merge_index]
	case .ExprStmt:
		expr_stmt := parser.decode_data(a.node_data, node.data, parser.ExprStmt)
		check_value(a, expr_stmt.inner)
	case:
		panic(fmt.tprintf("unhandled, %v", node.kind))
	}

	return
}

analyze :: proc(a: ^Analyzer) {
	root_index := parser.NodeIndex(len(a.nodes) - 1)
	root := a.nodes[root_index]
	module_decl := parser.decode_data(a.node_data, root.data, parser.ModuleDecl)

	append(&a.scopes, make(Scope))

	for stmt in module_decl.nodes {
		declare_globals(a, stmt)
	}

	for stmt in module_decl.nodes {
		infer_globals(a, stmt)
	}

	for stmt in module_decl.nodes {
		node := a.nodes[stmt]
		if node.kind == .Procedure {
			enter_procedure(a, node)
		}
	}

	pop(&a.scopes)
}

InstID :: distinct u32
INVALID_INST :: max(InstID)

BlockIndex :: distinct u32
INVALID_BLOCK :: max(BlockIndex)

InstKind :: enum u8 {
	Imm,
	Param,
	Load,
	Store,
	Call,
	Add,
	Mul,
	Equal,
	Less,
	Jump,
	Branch,
	Return,
}

Inst :: struct {
	kind:   InstKind,
	args:   []InstID,
	type:   TypeIndex,
	value:  i64,
	blocks: []BlockIndex,
}

Block :: struct {
	name:  string,
	insts: [dynamic]InstID,
}

Function :: struct {
	name:   string,
	insts:  [dynamic]Inst,
	blocks: [dynamic]Block,
}

inst_label :: proc(index: InstID) -> string {
	if index == INVALID_INST {
		return "<invalid>"
	}

	return fmt.tprintf("%%%v", index)
}

format_inst :: proc(a: ^Analyzer, func: Function, index: InstID, inst: Inst) -> string {
	dst := inst_label(index)
	type_name := "<invalid>"
	if inst.type != INVALID_TYPE && u32(inst.type) < u32(len(a.types)) {
		t := a.types[inst.type]
		if t.kind == .Primitive {
			type_name = base_type_strings[t.primitive.inner]
		} else {
			type_name = "<proc>"
		}
	}

	#partial switch inst.kind {
	case .Imm:
		return fmt.tprintf("imm %s,  %v     ; %s", dst, inst.value, type_name)
	case .Param:
		return fmt.tprintf("param %s, %v   ; %s", dst, inst.value, type_name)
	case .Load:
		sym := a.symbols[SymbolIndex(inst.value)]
		return fmt.tprintf("load %s, %s ; %s", dst, a.strings[sym.name], type_name)
	case .Store:
		sym := a.symbols[SymbolIndex(inst.value)]
		src := inst_label(inst.args[0])
		return fmt.tprintf("store %s, %s ; %s", a.strings[sym.name], src, type_name)
	case .Call:
		callee := inst_label(inst.args[0])
		if len(inst.args) > 1 {
			return fmt.tprintf(
				"call %s, %s, %s ; argc=%v ; %s",
				dst,
				callee,
				inst_label(inst.args[1]),
				inst.value,
				type_name,
			)
		}
		return fmt.tprintf("call %s, %s ; argc=%v ; %s", dst, callee, inst.value, type_name)
	case .Add:
		return fmt.tprintf(
			"add  %s, %s, %s ; %s",
			dst,
			inst_label(inst.args[0]),
			inst_label(inst.args[1]),
			type_name,
		)
	case .Mul:
		return fmt.tprintf(
			"mul  %s, %s, %s ; %s",
			dst,
			inst_label(inst.args[0]),
			inst_label(inst.args[1]),
			type_name,
		)
	case .Equal:
		return fmt.tprintf(
			"eq  %s, %s, %s ; %s",
			dst,
			inst_label(inst.args[0]),
			inst_label(inst.args[1]),
			type_name,
		)
	case .Less:
		return fmt.tprintf(
			"less  %s, %s, %s ; %s",
			dst,
			inst_label(inst.args[0]),
			inst_label(inst.args[1]),
			type_name,
		)
	case .Jump:
		target := "<invalid>"
		if len(inst.blocks) > 0 && u32(inst.blocks[0]) < u32(len(func.blocks)) {
			target = func.blocks[int(inst.blocks[0])].name
		}
		return fmt.tprintf("jmp  %s", target)
	case .Branch:
		target_true := "<invalid>"
		target_false := "<invalid>"
		if len(inst.blocks) > 0 && u32(inst.blocks[0]) < u32(len(func.blocks)) {
			target_true = func.blocks[int(inst.blocks[0])].name
		}
		if len(inst.blocks) > 1 && u32(inst.blocks[1]) < u32(len(func.blocks)) {
			target_false = func.blocks[int(inst.blocks[1])].name
		}
		return fmt.tprintf(
			"br  %s, %s, %s ; %s",
			inst_label(inst.args[0]),
			target_true,
			target_false,
			type_name,
		)
	case .Return:
		if len(inst.args) == 0 do return "ret"

		return fmt.tprintf("ret %s         ; %s", inst_label(inst.args[0]), type_name)
	}

	return fmt.tprintf("<%v> %s", inst.kind, dst)
}

print_block :: proc(a: ^Analyzer, func: Function, block: Block) {
	fmt.println(fmt.tprintf("%s:", block.name))
	for inst_index in block.insts {
		inst := func.insts[int(inst_index)]
		fmt.println(fmt.tprintf("  %s", format_inst(a, func, inst_index, inst)))
	}
}

emit_inst :: proc(a: ^Analyzer, inst: Inst) -> InstID {
	id := InstID(len(a.current_func.insts))
	append(&a.current_func.insts, inst)
	append(&a.current_block.insts, id)

	return id
}

block_needs_terminator :: proc(a: ^Analyzer, block: Block) -> bool {
	if len(block.insts) == 0 {
		return true
	}

	last_inst := a.current_func.insts[int(block.insts[len(block.insts) - 1])]
	return last_inst.kind != .Return && last_inst.kind != .Jump && last_inst.kind != .Branch
}
