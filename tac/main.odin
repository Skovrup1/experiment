package sema

import "../parser"

import "core:fmt"

// tac

// plan
// - typeless conversion to three adress code
// - scope analysis
// - type analysis

Scope :: map[string]InstIndex

enter_scope :: proc(a: ^Analyzer) {
	append(&a.scopes, make(Scope))
}

leave_scope :: proc(a: ^Analyzer) {
	pop(&a.scopes)
}

scope_lookup :: proc(a: ^Analyzer, ident: string) -> (InstIndex, bool) {
	// search scopes from top to bottom (local to global)
	for i := len(a.scopes) - 1; i >= 0; i -= 1 {
		scope := a.scopes[i]
		if inst_index, found := scope[ident]; found {
			return inst_index, true
		}
	}

	// identifier not found
	return INVALID_INST, false
}

Analyzer :: struct {
	parser:      ^parser.Parser,
	insts:       [dynamic]Inst,
	strings:     [dynamic]string,
	itable:      map[string]StringIndex,
	label_count: int,
	scopes:      [dynamic]Scope,
}

StringIndex :: distinct u32

make_analyzer :: proc(p: ^parser.Parser) -> Analyzer {
	strings := make([dynamic]string) // string interning for any string information that is needed later
	itable := make(map[string]StringIndex)
	insts := make([dynamic]Inst)
	scopes := make([dynamic]Scope)
	return Analyzer{p, insts, strings, itable, 0, scopes}
}

Label :: struct {
	name: StringIndex,
}

Alloc :: struct {
	ident: string,
}

Load :: struct {
	address: InstIndex,
}

Store :: struct {
	address: InstIndex,
	value:   InstIndex,
}

ConstInt :: struct {
	value: int,
}

Add :: struct {
	left:  InstIndex,
	right: InstIndex,
}

Sub :: struct {
	left:  InstIndex,
	right: InstIndex,
}

Mul :: struct {
	left:  InstIndex,
	right: InstIndex,
}

Div :: struct {
	left:  InstIndex,
	right: InstIndex,
}

// equal
Eq :: struct {
	left:  InstIndex,
	right: InstIndex,
}

// not equal
Ne :: struct {
	left:  InstIndex,
	right: InstIndex,
}

// less than
Lt :: struct {
	left:  InstIndex,
	right: InstIndex,
}

Le :: struct {
	left:  InstIndex,
	right: InstIndex,
}

Gt :: struct {
	left:  InstIndex,
	right: InstIndex,
}

Ge :: struct {
	left:  InstIndex,
	right: InstIndex,
}


And :: struct {
	left:  InstIndex,
	right: InstIndex,
}

Or :: struct {
	left:  InstIndex,
	right: InstIndex,
}

Neg :: struct {
	expr: InstIndex,
}

Not :: struct {
	expr: InstIndex,
}

Param :: struct {
	value: InstIndex,
}

Call :: struct {
	callee: InstIndex,
}

Return :: struct {
	value: InstIndex,
}

Jump :: struct {
	target: InstIndex,
}

Branch :: struct {
	cond:       InstIndex,
	then_label: InstIndex,
	else_label: InstIndex,
}

Inst :: union {
	// memory & metadata
	Label,
	Alloc,
	Load,
	Store,
	// constants
	ConstInt,
	// arithmetic ops
	Add,
	Sub,
	Mul,
	Div,
	// comparison ops
	Eq,
	Ne,
	Lt,
	Le,
	Gt,
	Ge,
	// logical ops
	And,
	Or,
	// unary ops
	Neg,
	Not,
	// functions & calls
	Param,
	Call,
	Return,
	// control flow
	Jump,
	Branch,
}

InstIndex :: distinct u32
INVALID_INST :: max(InstIndex)

add_inst :: proc(a: ^Analyzer, inst: Inst) -> InstIndex {
	append(&a.insts, inst)
	return InstIndex(len(a.insts) - 1)
}

add_string :: proc(a: ^Analyzer, str: string) -> StringIndex {
	append(&a.strings, str)
	index := StringIndex(len(a.strings) - 1)
	a.itable[str] = index
	return index
}

get_string :: proc(a: ^Analyzer, index: StringIndex) -> string {
	return a.strings[index]
}

new_label :: proc(a: ^Analyzer) -> StringIndex {
	str := fmt.aprintf("L%v", a.label_count)
	a.label_count += 1
	index := add_string(a, str)
	return index
}

Address :: struct {
	index: InstIndex,
}

Value :: struct {
	index: InstIndex,
}

Const :: struct {
	number: int,
}

SemaResult :: union {
	Address,
	Value,
	Const,
}

ensure_value :: proc(a: ^Analyzer, res: SemaResult) -> Value {
	switch v in res {
	case Address:
		load_index := add_inst(a, Load{address = v.index})
		return Value{index = load_index}
	case Value:
		return v
	case Const:
		// we create a ConstInt instruction to make it a runtime value
		const_index := add_inst(a, ConstInt{value = v.number})
		return Value{const_index}
	case:
		panic("AHH")
	}
}

analyze :: proc(a: ^Analyzer) -> []Inst {
	analyze_node :: proc(a: ^Analyzer, index: parser.NodeIndex) -> SemaResult {
		if index == parser.INVALID_NODE {
			return {}
		}

		node := a.parser.nodes[index]

		switch v in node {
		case parser.Module:
			enter_scope(a)
			for value in v.nodes {
				analyze_node(a, value)
			}
			leave_scope(a)

		case parser.IdentLit:
			ident := parser.token_to_string(a.parser, v.token)
			if index, ok := scope_lookup(a, ident); ok {
				return Address{index}
			} else {
				panic(fmt.tprintf("error: undeclared identifier '%v'\n", ident))
			}

		case parser.StringLit:

		case parser.RealLit:

		case parser.IntLit:
			number := parser.token_to_int(a.parser, v.token)
			return Const{number}

		case parser.BoolLit:

		case parser.StructLit:

		case parser.ArrayLit:

		case parser.VarDecl:
			ident := parser.token_to_string(a.parser, v.token)

			if _, ok := scope_lookup(a, ident); ok {
				panic(fmt.tprintf("error: shadowing of identifier '%v' is not allowed.\n", ident))
			}

			alloc := add_inst(a, Alloc{ident})

			current_scope := &a.scopes[len(a.scopes) - 1]
			current_scope[ident] = alloc

			if v.expr != parser.INVALID_NODE {
				expr := analyze_node(a, v.expr)
				value := ensure_value(a, expr)
				add_inst(a, Store{alloc, value.index})
			}
		case parser.ConstDecl:
			ident := parser.token_to_string(a.parser, v.token)
			if _, ok := scope_lookup(a, ident); ok {
				panic(fmt.tprintf("error: shadowing of identifier '%v' is not allowed.\n", ident))
			}
		// hmm

		case parser.ParamDecl:

		case parser.ProcDecl:
			token_str := parser.token_to_string(a.parser, v.token)

			ident := add_string(a, token_str)
			label := add_inst(a, Label{ident})

			current_scope := &a.scopes[len(a.scopes) - 1]
			current_scope[token_str] = label

			enter_scope(a)
			for value in v.params {
				analyze_node(a, value)
			}
			analyze_node(a, v.body)
			leave_scope(a)

		case parser.StructDecl:

		case parser.ExprStmt:
			analyze_node(a, v.expr)

		case parser.BlockStmt:
			enter_scope(a)
			for value in v.stmts {
				analyze_node(a, value)
			}
			leave_scope(a)

		case parser.ReturnStmt:
			result := analyze_node(a, v.expr)
			return_val := ensure_value(a, result)
			add_inst(a, Return{return_val.index})

		case parser.IfStmt:
			result := analyze_node(a, v.cond)
			value := ensure_value(a, result)
			br_index := add_inst(a, Branch{value.index, INVALID_INST, INVALID_INST})

			then_label := add_inst(a, Label{new_label(a)})
			analyze_node(a, v.then)

			else_label := add_inst(a, Label{new_label(a)})
			analyze_node(a, v.else_)

			br := &((&a.insts[br_index]).(Branch))
			br.then_label = then_label
			br.else_label = else_label

		case parser.LoopStmt:
			panic("todo")

		case parser.BreakStmt:
			panic("todo")

		case parser.ContinueStmt:
			panic("todo")

		case parser.CallExpr:
			token := parser.token_to_string(a.parser, v.token)
			for arg in v.args {
				analyze_node(a, arg)
			}

			callee := analyze_node(a, v.callee)
			callee_index := callee.(Address).index
			/*
			callee_index, ok := scope_lookup(a, callee)
			if !ok {
				panic(fmt.tprintf("unknown callee: %v\n", callee))
			}*/

			return Value{add_inst(a, Call{callee_index})}
		case parser.MemberExpr:
			panic("todo")

        case parser.IndexExpr:
			panic("todo")

		case parser.UnaryExpr:
			panic("todo")

		case parser.BinaryExpr:
			op_str := parser.token_to_string(a.parser, v.token)
			switch op_str {
			case "=":
				left := analyze_node(a, v.left)
				left_addr, ok := left.(Address)
				if !ok {
					panic("error: left side of assignment must be a variable or memory location")
				}

				right := analyze_node(a, v.right)
				right_value := ensure_value(a, right)

				add_inst(a, Store{left_addr.index, right_value.index})

				return right_value
			case "==":
				left := analyze_node(a, v.left)
				right := analyze_node(a, v.right)

				left_value := ensure_value(a, left)
				right_value := ensure_value(a, right)

				eq := add_inst(a, Eq{left_value.index, right_value.index})
				return Value{eq}

			case "+":
				left := analyze_node(a, v.left)
				right := analyze_node(a, v.right)

				left_value := ensure_value(a, left)
				right_value := ensure_value(a, right)

				add := add_inst(a, Add{left_value.index, right_value.index})
				return Value{add}
			case:
				panic(fmt.tprintf("invalid binary op: %v\n", op_str))
			}
		}

		return {}
	}

	root := parser.NodeIndex(len(a.parser.nodes)) - 1
	analyze_node(a, root)

	return a.insts[:]
}

print_insts :: proc(a: ^Analyzer) {
	print_indent :: proc(indent: int) {
		for _ in 0 ..< indent {
			fmt.print("  ")
		}
	}

	for inst, i in a.insts {
		switch v in inst {
		case Label:
			name := get_string(a, v.name)
			fmt.printf("%s:\n", name)
		case ConstInt:
			print_indent(2)
			fmt.printf("const %%%v, %v\n", i, v.value)
		case Alloc:
			print_indent(2)
			type_str := "I32"
			fmt.printf("alloc %%%v, %s\n", i, type_str)
		case Load:
			print_indent(2)
			fmt.printf("load  %%%v, %%%v\n", i, v.address)
		case Store:
			print_indent(2)
			fmt.printf("store %%%v, %%%v\n", v.address, v.value)
		case Add:
		case Sub:
		case Mul:
		case Div:
		case Eq:
		case Ne:
		case Lt:
		case Le:
		case Gt:
		case Ge:
		case And:
		case Or:
		case Neg:
		case Not:
		case Param:
		case Call:
			print_indent(2)
			fmt.printf("call %%%v, %v\n", i, get_string(a, StringIndex(0)))
		case Return:
			print_indent(2)
			fmt.printf("return %%%v\n", v.value)
		case Jump:
		case Branch:
			print_indent(2)
			then_label := a.insts[v.then_label].(Label)
			else_label := a.insts[v.else_label].(Label)
			then_name := get_string(a, then_label.name)
			else_name := get_string(a, else_label.name)
			fmt.printf("branch t%v, %v, %v\n", v.cond, then_name, else_name)
		}
	}
}
