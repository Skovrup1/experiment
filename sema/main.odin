package sema

import "../parser"
import "../scanner"

import "core:fmt"

BaseType :: enum {
	Void,
	String,
	F32,
	I32,
	Bool,
}

Primitive :: struct {
	inner: BaseType,
}

Procedure :: struct {
	return_type: TypeIndex,
	params:      []TypeIndex,
}

Type :: union {
	Primitive,
	Procedure,
}

TypeIndex :: distinct u32
INVALID_TYPE :: max(TypeIndex)

Scope :: map[string]SymbolIndex

SymbolKind :: enum u8 {
	Var,
	Param,
	Proc,
	Type,
}

Symbol :: struct {
	kind:     SymbolKind,
	ident:    string,
	type:     TypeIndex,
	is_const: bool,
}

SymbolIndex :: distinct u32
INVALID_SYMBOL :: max(SymbolIndex)

Analyzer :: struct {
	p:                 ^parser.Parser,
	symbols:           [dynamic]Symbol,
	scopes:            [dynamic]Scope,
	types:             [dynamic]Type,
	return_type_stack: [dynamic]TypeIndex,
	errors:            [dynamic]string,
}

make_analyzer :: proc(p: ^parser.Parser) -> Analyzer {
	analyzer := Analyzer {
		p,
		make([dynamic]Symbol),
		make([dynamic]Scope),
		make([dynamic]Type),
		make([dynamic]TypeIndex),
		make([dynamic]string),
	}

	append(&analyzer.types, Primitive{BaseType.Void})
	append(&analyzer.types, Primitive{BaseType.String})
	append(&analyzer.types, Primitive{BaseType.F32})
	append(&analyzer.types, Primitive{BaseType.I32})
	append(&analyzer.types, Primitive{BaseType.Bool})

	return analyzer
}

enter_scope :: proc(a: ^Analyzer) {
	append(&a.scopes, make(Scope))
}

exit_scope :: proc(a: ^Analyzer) {
	pop(&a.scopes)
}

add_symbol_to_current_scope :: proc(a: ^Analyzer, name: string, symbol: Symbol) {
	symbol_index := SymbolIndex(len(a.symbols))
	append(&a.symbols, symbol)

	current_scope := &a.scopes[len(a.scopes) - 1]
	current_scope[name] = symbol_index
}

lookup_symbol :: proc(a: ^Analyzer, ident: string) -> (SymbolIndex, bool) {
	// from local to global
	for i := len(a.scopes) - 1; i >= 0; i -= 1 {
		if symbol_index, found := a.scopes[i][ident]; found {
			return symbol_index, true
		}
	}
	return INVALID_SYMBOL, false
}

lookup_type :: proc(a: ^Analyzer, node_index: parser.NodeIndex) -> (TypeIndex, bool) {
	if node_index == parser.INVALID_NODE {
		return 0, false
	}

	node := a.p.nodes[node_index]

	if type_node, ok := node.(parser.TypeSpec); ok {
		name := parser.token_to_string(a.p, type_node.token)

		switch name {
		case "Void":
			return TypeIndex(BaseType.Void), true
		case "String":
			return TypeIndex(BaseType.String), true
		case "F32":
			return TypeIndex(BaseType.F32), true
		case "I32":
			return TypeIndex(BaseType.I32), true
		case "Bool":
			return TypeIndex(BaseType.Bool), true
		}
	}

	panic("failed to lookup type")
}

hoisting :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	node := a.p.nodes[node_index]
	#partial switch v in node {
	case parser.Module:
		enter_scope(a)
		for decl in v.nodes {
			hoisting(a, decl)
		}
	case parser.VarDecl:
		ident := parser.token_to_string(a.p, v.token)

		declared_type, has_type := lookup_type(a, v.type)

		if !has_type {
			declared_type = infer(a, v.expr)
		}

		symbol := Symbol {
			kind  = .Var,
			ident = ident,
			type  = declared_type,
		}
		add_symbol_to_current_scope(a, ident, symbol)
	case parser.ConstDecl:
		ident := parser.token_to_string(a.p, v.token)

		declared_type, has_type := lookup_type(a, v.type)

		if !has_type {
			declared_type = infer(a, v.expr)
		}

		symbol := Symbol {
			kind     = .Var,
			ident    = ident,
			type     = declared_type,
			is_const = true,
		}
		add_symbol_to_current_scope(a, ident, symbol)
	case parser.ParamDecl:
		ident := parser.token_to_string(a.p, v.token)

		declared_type, _ := lookup_type(a, v.type)

		symbol := Symbol {
			kind  = .Proc,
			ident = ident,
			type  = declared_type,
		}
		add_symbol_to_current_scope(a, ident, symbol)
	case parser.MemberDecl:
	case parser.ProcDecl:
		ident := parser.token_to_string(a.p, v.token)

		param_types := make([dynamic]TypeIndex, len(v.params))
		for param, i in v.params {
			param_node := a.p.nodes[param].(parser.ParamDecl)
			param_types[i], _ = lookup_type(a, param_node.type)
		}
		return_type, _ := lookup_type(a, v.return_type)

		append(&a.types, Procedure{return_type, param_types[:]})
		symbol := Symbol {
			kind  = .Proc,
			ident = ident,
			type  = TypeIndex(len(a.types) - 1),
		}
		add_symbol_to_current_scope(a, ident, symbol)

        // note: recheck
		append(&a.return_type_stack, return_type)
	case parser.StructDecl:
	}
}

infer :: proc(a: ^Analyzer, node_index: parser.NodeIndex) -> TypeIndex {
	node := a.p.nodes[node_index]
	#partial switch v in node {
	case parser.StringLit:
		return TypeIndex(BaseType.String)
	case parser.RealLit:
		return TypeIndex(BaseType.F32)
	case parser.IntLit:
		return TypeIndex(BaseType.I32)
	case parser.BoolLit:
		return TypeIndex(BaseType.Bool)
	case parser.IdentLit:
		ident := parser.token_to_string(a.p, v.token)
		if symbol_idx, found := lookup_symbol(a, ident); found {
			return a.symbols[symbol_idx].type
		}
		panic(fmt.tprintf("undeclared identifier: %v\n", ident))
	case parser.CallExpr:
		callee_index := infer(a, v.callee)
		callee_type := a.types[callee_index]
		proc_type, is_proc := callee_type.(Procedure)

		if !is_proc {
			panic(fmt.tprintf("invalid callee type %v", callee_type))
		}

		for arg, i in v.args {
			param_type := proc_type.params[i]
			check(a, arg, param_type)
		}

		return proc_type.return_type

	case parser.BinaryExpr:
		left := infer(a, v.left)
		right := infer(a, v.right)
		op := a.p.tokens[v.token].kind
		if left != right {
			panic(fmt.tprintf("error: %v, type mismatch with %v, %v", op, left, right))
		}
		#partial switch op {
		case .Assign,
		     .Equal,
		     .NotEqual,
		     .Greater,
		     .GreaterEqual,
		     .Less,
		     .LessEqual,
		     .Plus,
		     .PlusEqual,
		     .Minus,
		     .MinusEqual,
		     .Mul,
		     .MulEqual,
		     .Div,
		     .DivEqual,
		     .Mod,
		     .ModEqual,
		     .Power,
		     .PowerEqual,
		     .TildeEqual,
		     .Ampersand,
		     .AmpersandEqual,
		     .Pipe,
		     .PipeEqual,
		     .Hat,
		     .HatEqual,
		     .LShift,
		     .LShiftEqual,
		     .RShift,
		     .RShiftEqual:
			return left
		case .Or, .And:
			if left != TypeIndex(BaseType.Bool) {
				panic(fmt.tprintf("error: not a bool"))
			}
			if right != TypeIndex(BaseType.Bool) {
				panic(fmt.tprintf("error: not a bool"))
			}
			return TypeIndex(BaseType.Bool)
		case:
			panic(fmt.tprintf("error: unimplemented BinaryExpr %v", op))
		}
	case parser.UnaryExpr:
		inner := infer(a, v.expr)
		op := a.p.tokens[v.token].kind
		#partial switch op {
		case .Minus, .Tilde, .Ampersand, .Deref:
			return inner
		case .Not:
			if inner != TypeIndex(BaseType.Bool) {
				panic(fmt.tprintf("error: not a bool"))
			}
			return TypeIndex(BaseType.Bool)
		case:
			panic(fmt.tprintf("error: unimplemented UnaryExpr %v", op))
		}
	case parser.MemberExpr:
		inner := infer(a, v.expr)
		return inner
	case parser.IndexExpr:
		base := infer(a, v.base)
		offset := infer(a, v.offset)

		if base != offset {
			panic("invalid index")
		}

		return base
	case:
		panic(fmt.tprintf("cannot infer type of node %v\n", v))
	}
}

check :: proc(
	a: ^Analyzer,
	node_index: parser.NodeIndex,
	expected_type: TypeIndex,
	loc := #caller_location,
) {
	node := a.p.nodes[node_index]

	inferred_type := infer(a, node_index)
	if inferred_type != expected_type {
		expected := a.types[expected_type]
		inferred := a.types[inferred_type]
		panic(
			fmt.tprintf(
				"type mismatch: expected type '%v', but got '%v' :: %v\n",
				expected,
				inferred,
				loc,
			),
		)
	}
}

type_check :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {
	node := a.p.nodes[node_index]
	#partial switch v in node {
	case parser.Module:
		for child in v.nodes {
			type_check(a, child)
		}
	case parser.VarDecl:
		ident := parser.token_to_string(a.p, v.token)

		declared_type, has_type := lookup_type(a, v.type)

		if !has_type {
			declared_type = infer(a, v.expr)
		}

		if v.expr != parser.INVALID_NODE {
			check(a, v.expr, declared_type)
		}

		if _, found := lookup_symbol(a, ident); found {
			panic(fmt.tprintf("'%v' already declared\n", ident))
		}

		symbol := Symbol {
			kind  = .Var,
			ident = ident,
			type  = declared_type,
		}
		add_symbol_to_current_scope(a, ident, symbol)

	case parser.ConstDecl:
		ident := parser.token_to_string(a.p, v.token)

		declared_type, has_type := lookup_type(a, v.type)

		if !has_type {
			declared_type = infer(a, v.expr)
		}

		if v.expr != parser.INVALID_NODE {
			check(a, v.expr, declared_type)
		}

		if _, found := lookup_symbol(a, ident); found {
			panic(fmt.tprintf("'%v' already declared\n", ident))
		}

		symbol := Symbol {
			kind     = .Var,
			ident    = ident,
			type     = declared_type,
			is_const = true,
		}
		add_symbol_to_current_scope(a, ident, symbol)

	case parser.ParamDecl:
		ident := parser.token_to_string(a.p, v.token)

		declared_type, has_type := lookup_type(a, v.type)

		if !has_type {
			declared_type = infer(a, v.expr)
		}

		if v.expr != parser.INVALID_NODE {
			check(a, v.expr, declared_type)
		}

		if _, found := lookup_symbol(a, ident); found {
			panic(fmt.tprintf("'%v' already declared\n", ident))
		}

		symbol := Symbol {
			kind  = .Param,
			ident = ident,
			type  = declared_type,
		}
		add_symbol_to_current_scope(a, ident, symbol)

	case parser.ProcDecl:
		proc_symbol, _ := lookup_symbol(a, parser.token_to_string(a.p, v.token))
		symbol_type_index := a.symbols[proc_symbol].type
		proc_type := a.types[symbol_type_index]
		type, ok := proc_type.(Procedure)

		if !ok {
			panic("invalid type")
		}

		append(&a.return_type_stack, type.return_type)
		enter_scope(a)
		for param in v.params {
			type_check(a, param)
		}
		type_check(a, v.body)
		exit_scope(a)
		pop(&a.return_type_stack)

	case parser.ExprStmt:
		infer(a, v.expr)

	case parser.BlockStmt:
		enter_scope(a)
		for stmt in v.stmts {
			type_check(a, stmt)
		}
		exit_scope(a)

	case parser.ReturnStmt:
		expected_type := pop(&a.return_type_stack)

		if v.expr != parser.INVALID_NODE {

			if expected_type == TypeIndex(BaseType.Void) {
				panic("error: cannot return a value from a void procedure")
			}

			check(a, v.expr, expected_type)
		}

	case:
	}
}

flow_check :: proc(a: ^Analyzer, node_index: parser.NodeIndex) {

}

analyze :: proc(a: ^Analyzer) {
	ast := a.p.nodes[:]
	if len(ast) == 0 {
		return
	}
	root_node := parser.NodeIndex(len(ast) - 1)

	hoisting(a, root_node)
	type_check(a, root_node)
	flow_check(a, root_node)
}
