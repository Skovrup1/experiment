package test

import "../compiler/parser"

import "core:fmt"

compare_nodes :: proc(
	p1: ^parser.Parser,
	n1: parser.NodeIndex,
	p2: ^parser.Parser,
	n2: parser.NodeIndex,
) -> bool {
	if n1 == parser.INVALID_NODE && n2 == parser.INVALID_NODE {
		return true
	}
	if n1 == parser.INVALID_NODE || n2 == parser.INVALID_NODE {
		fmt.printf("one node is INVALID: n1=%v, n2=%v\n", n1, n2)
		return false
	}

	node1 := p1.nodes[n1]
	node2 := p2.nodes[n2]

	if node1.kind != node2.kind {
		fmt.printf("node kind mismatch: %v != %v\n", node1.kind, node2.kind)
		return false
	}

	#partial switch node1.kind {
	case .Identifier, .True, .False, .Integer, .Float:
		t1 := p1.tokens[node1.token]
		t2 := p2.tokens[node2.token]
		s1 := p1.source[t1.start:t1.end]
		s2 := p2.source[t2.start:t2.end]
		if s1 != s2 {
			fmt.printf("Token text mismatch for %v: '%s' vs '%s'\n", node1.kind, s1, s2)
			return false
		}
		return true

	case .Addition:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.AddExpr)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.AddExpr)
		return compare_nodes(p1, d1.left, p2, d2.left) && compare_nodes(p1, d1.right, p2, d2.right)

	case .Multiplication:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.MulExpr)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.MulExpr)
		return compare_nodes(p1, d1.left, p2, d2.left) && compare_nodes(p1, d1.right, p2, d2.right)

	case .Less:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.LessExpr)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.LessExpr)
		return compare_nodes(p1, d1.left, p2, d2.left) && compare_nodes(p1, d1.right, p2, d2.right)

	case .Equal:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.EqualExpr)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.EqualExpr)
		return compare_nodes(p1, d1.left, p2, d2.left) && compare_nodes(p1, d1.right, p2, d2.right)

	case .Assignment:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.AssignExpr)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.AssignExpr)
		return compare_nodes(p1, d1.left, p2, d2.left) && compare_nodes(p1, d1.right, p2, d2.right)

	case .Variable:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.VarDecl)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.VarDecl)
		t1 := p1.tokens[node1.token]
		t2 := p2.tokens[node2.token]
		s1 := p1.source[t1.start:t1.end]
		s2 := p2.source[t2.start:t2.end]
		if s1 != s2 {
			fmt.printf("Variable name mismatch: '%s' vs '%s'\n", s1, s2)
			return false
		}
		return compare_nodes(p1, d1.value, p2, d2.value)

	case .Return:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.ReturnStmt)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.ReturnStmt)
		return compare_nodes(p1, d1.value, p2, d2.value)

	case .ExprStmt:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.ExprStmt)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.ExprStmt)
		return compare_nodes(p1, d1.inner, p2, d2.inner)

	case .Block:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.BlockStmt)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.BlockStmt)
		if len(d1.statements) != len(d2.statements) {
			fmt.printf(
				"Block statement count mismatch: %d vs %d\n",
				len(d1.statements),
				len(d2.statements),
			)
			return false
		}
		for stmt1, i in d1.statements {
			if !compare_nodes(p1, stmt1, p2, d2.statements[i]) {
				fmt.printf("Block statement %d mismatch\n", i)
				return false
			}
		}
		return true

	case .Procedure:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.ProcDecl)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.ProcDecl)

		t1 := p1.tokens[d1.name]
		t2 := p2.tokens[d2.name]
		s1 := p1.source[t1.start:t1.end]
		s2 := p2.source[t2.start:t2.end]
		if s1 != s2 {
			fmt.printf("Procedure name mismatch: '%s' vs '%s'\n", s1, s2)
			return false
		}

		if len(d1.parameters) != len(d2.parameters) {
			fmt.printf(
				"Parameter count mismatch: %d vs %d\n",
				len(d1.parameters),
				len(d2.parameters),
			)
			return false
		}

		for p1_idx, i in d1.parameters {
			if !compare_nodes(p1, p1_idx, p2, d2.parameters[i]) {
				fmt.printf("Parameter %d mismatch\n", i)
				return false
			}
		}

		if !compare_nodes(p1, d1.return_type, p2, d2.return_type) {
			fmt.println("Return type mismatch")
			return false
		}

		if !compare_nodes(p1, d1.body, p2, d2.body) {
			fmt.println("Procedure body mismatch")
			return false
		}

		return true

	case .Parameter:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.ParamDecl)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.ParamDecl)
		t1 := p1.tokens[node1.token]
		t2 := p2.tokens[node2.token]
		s1 := p1.source[t1.start:t1.end]
		s2 := p2.source[t2.start:t2.end]
		if s1 != s2 {
			fmt.printf("Parameter name mismatch: '%s' vs '%s'\n", s1, s2)
			return false
		}
		return compare_nodes(p1, d1.type, p2, d2.type)

	case .For:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.ForStmt)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.ForStmt)
		if !compare_nodes(p1, d1.initial, p2, d2.initial) {
			fmt.println("For initial mismatch")
			return false
		}
		if !compare_nodes(p1, d1.condition, p2, d2.condition) {
			fmt.println("For condition mismatch")
			return false
		}
		if !compare_nodes(p1, d1.update, p2, d2.update) {
			fmt.println("For update mismatch")
			return false
		}
		if !compare_nodes(p1, d1.body, p2, d2.body) {
			fmt.println("For body mismatch")
			return false
		}
		return true

	case .If:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.IfExpr)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.IfExpr)
		if !compare_nodes(p1, d1.condition, p2, d2.condition) {
			fmt.println("If condition mismatch")
			return false
		}
		if !compare_nodes(p1, d1.then_body, p2, d2.then_body) {
			fmt.println("If then-body mismatch")
			return false
		}
		if !compare_nodes(p1, d1.else_body, p2, d2.else_body) {
			fmt.println("If else-body mismatch")
			return false
		}
		return true

	case .Call:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.CallExpr)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.CallExpr)
		if !compare_nodes(p1, d1.callee, p2, d2.callee) {
			fmt.println("Call callee mismatch")
			return false
		}
		if len(d1.arguments) != len(d2.arguments) {
			fmt.printf(
				"Call argument count mismatch: %d vs %d\n",
				len(d1.arguments),
				len(d2.arguments),
			)
			return false
		}
		for arg1, i in d1.arguments {
			if !compare_nodes(p1, arg1, p2, d2.arguments[i]) {
				fmt.printf("Call argument %d mismatch\n", i)
				return false
			}
		}
		return true

	case .Module:
		d1 := parser.decode_data(p1.data[:], node1.data, parser.ModuleDecl)
		d2 := parser.decode_data(p2.data[:], node2.data, parser.ModuleDecl)
		if len(d1.statements) != len(d2.statements) {
			fmt.printf(
				"Module statement count mismatch: %d vs %d\n",
				len(d1.statements),
				len(d2.statements),
			)
			return false
		}
		for stmt1, i in d1.statements {
			if !compare_nodes(p1, stmt1, p2, d2.statements[i]) {
				fmt.printf("Module statement %d mismatch\n", i)
				return false
			}
		}
		return true
	}

	fmt.printf("Unknown node kind: %v\n", node1.kind)
	return false
}
