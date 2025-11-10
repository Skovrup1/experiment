package parser

import "../lexer"

import "core:fmt"

NodeKind :: enum u8 {
	Invalid = 0,
	Primitive,
	Identifier,
	Integer,
	Block,
	Variable,
	Parameter,
	Procedure,
	For,
	Return,
	ExprStmt,
	Call,
	Assignment,
	Addition,
	Multiplication,
	Less,
}

NodeData :: struct {
	int_lit:         struct {
		integer: i64,
	},
	var_stmt:        struct {
		type:  NodeIndex,
		value: NodeIndex,
	},
	param_stmt:      struct {
		type:  NodeIndex,
		value: NodeIndex,
	},
	block_statement: struct {
		statements: []NodeIndex,
	},
	proc_stmt:       struct {
		identifier:  lexer.TokenIndex,
		return_type: NodeIndex,
		body:        NodeIndex,
		parameters:  []NodeIndex,
	},
	for_stmt:        struct {
		initial:   NodeIndex,
		condition: NodeIndex,
		update:    NodeIndex,
		body:      NodeIndex,
	},
	return_stmt:     struct {
		value: NodeIndex,
	},
	expr_stmt:       struct {
		expression: NodeIndex,
	},
	call_expr:       struct {
		callee:    NodeIndex,
		arguments: []NodeIndex,
	},
	assign_expr:     struct {
		left:  NodeIndex,
		right: NodeIndex,
	},
	mul_expr:        struct {
		left:  NodeIndex,
		right: NodeIndex,
	},
	add_expr:        struct {
		left:  NodeIndex,
		right: NodeIndex,
	},
	less_expr:       struct {
		left:  NodeIndex,
		right: NodeIndex,
	},
}

DataIndex :: distinct u32
INVALID_DATA :: max(DataIndex)

Node :: struct {
	kind:  NodeKind,
	data:  DataIndex,
	token: lexer.TokenIndex,
}

NodeIndex :: distinct u32
INVALID_NODE :: max(NodeIndex)

Parser :: struct {
	source:    string,
	tokens:    []lexer.Token,
	nodes:     [dynamic]Node,
	data:      [dynamic]NodeData,
	cursor:    lexer.TokenIndex,
	lookahead: lexer.TokenIndex,
}

make_parser :: proc(source: string, tokens: []lexer.Token) -> Parser {
	nodes := make([dynamic]Node)
	data := make([dynamic]NodeData)
	cursor :: 0
	lookahead :: 1
	return Parser{source, tokens, nodes, data, cursor, lookahead}
}

next :: proc(p: ^Parser) -> lexer.TokenIndex {
	p.cursor = p.lookahead
	p.lookahead += 1

	return p.cursor
}

peek :: proc(p: ^Parser) -> lexer.TokenKind {
	return p.tokens[p.cursor].kind
}

peek_next :: proc(p: ^Parser) -> lexer.TokenKind {
	return p.tokens[p.lookahead].kind
}

expect :: proc(p: ^Parser, kind: lexer.TokenKind, loc := #caller_location) {
	if peek(p) == kind {
		next(p)
	} else {
		panic(fmt.tprintf("expected %v, got %v, at %v", kind, peek(p), loc))
	}
}

allow :: proc(p: ^Parser, kind: lexer.TokenKind) -> bool {
	if peek(p) == kind {
		next(p)
		return true
	} else {
		return false
	}
}

add_node :: proc(p: ^Parser, node: Node) -> NodeIndex {
	append(&p.nodes, node)
	return NodeIndex(len(p.nodes) - 1)
}

add_data :: proc(p: ^Parser, data: NodeData) -> DataIndex {
	append(&p.data, data)
	return DataIndex(len(p.data) - 1)
}

print_ast :: proc(p: ^Parser) {
	print_node :: proc(p: ^Parser, index: NodeIndex, indent := 0) {
		print_indent :: proc(indent: int) {
			for _ in 0 ..< indent {
				fmt.print(' ')
			}
		}

		if index == INVALID_NODE {
			print_indent(indent)
			fmt.println("invalid node")
			return
		}

		next_indent := indent + 2

		node := p.nodes[index]
		#partial switch node.kind {
		case .Primitive:
			print_indent(indent)
			fmt.println("PrimitiveType")
		case .Identifier:
			print_indent(indent)
			fmt.println("IdentifierLiteral")
		case .Integer:
			print_indent(indent)
			fmt.println("IntegerLiteral")
		case .Variable:
			print_indent(indent)
			fmt.println("VariableStatement")
		case .Block:
			print_indent(indent)
			fmt.println("BlockStatement")
			block_data := p.data[node.data].block_statement
			for stmt in block_data.statements {
				print_node(p, stmt, next_indent)
			}
		case .Parameter:
			print_indent(indent)
			fmt.println("Parameter")
		case .Procedure:
			print_indent(indent)
			fmt.println("ProcedureStatement")
			proc_data := p.data[node.data].proc_stmt
			for param in proc_data.parameters {
				print_node(p, param, next_indent)
			}
			print_node(p, proc_data.return_type, next_indent)
			print_node(p, proc_data.body, next_indent)
		case .For:
			print_indent(indent)
			fmt.println("ForStatement")
		case .Return:
			print_indent(indent)
			fmt.println("ReturnStatement")
		case .ExprStmt:
			print_indent(indent)
			fmt.println("ExpressionStatement")
		case .Call:
			print_indent(indent)
			fmt.println("CallExpression")
		case .Assignment:
			print_indent(indent)
			fmt.println("AssignmentExpression")
		case .Addition:
			print_indent(indent)
			fmt.println("AdditionExpression")
		case .Multiplication:
			print_indent(indent)
			fmt.println("MultiplicationExpression")
		case .Less:
			print_indent(indent)
			fmt.println("LessExpression")
		case:
			print_indent(indent)
			fmt.println("unknown node kind: %v", node.kind)
		}
	}

	print_node(p, NodeIndex(len(p.nodes) - 1))
}

print_program :: proc(p: ^Parser) {
	print_node :: proc(p: ^Parser, index: NodeIndex, indent := 0) {
		print_indent :: proc(indent: int) {
			for _ in 0 ..< indent {
				fmt.print(' ')
			}
		}

		if index == INVALID_NODE {
			print_indent(indent)
			fmt.println("invalid node")
			return
		}

		next_indent := indent + 4

		node := p.nodes[index]
		#partial switch node.kind {
		case .Primitive:
			token := p.tokens[node.token]
			fmt.print(p.source[token.start:token.end])
		case .Identifier:
			token := p.tokens[node.token]
			fmt.print(p.source[token.start:token.end])
		case .Integer:
			token := p.tokens[node.token]
			fmt.print(p.source[token.start:token.end])
		case .Variable:
			var_data := p.data[node.data].var_stmt
			token := p.tokens[node.token]
			if indent > 0 {
				print_indent(indent)
			}
			fmt.print(p.source[token.start:token.end])
			fmt.print(" := ")
			print_node(p, var_data.value)
			if indent > 0 {
				fmt.println(";")
			}
		case .Block:
			fmt.println(" {")
			block_data := p.data[node.data].block_statement
			for stmt in block_data.statements {
				print_node(p, stmt, next_indent)
			}
			print_indent(indent)
			fmt.println("}")
		case .Parameter:
			param_data := p.data[node.data].param_stmt
			token := p.tokens[node.token]
			fmt.print(p.source[token.start:token.end])
			fmt.print(": ")
			print_node(p, param_data.type)
		case .Procedure:
			proc_data := p.data[node.data].proc_stmt
			print_indent(indent)
			fmt.print("def ")
			id_token := p.tokens[proc_data.identifier]
			fmt.print(p.source[id_token.start:id_token.end])
			fmt.print("(")
			for param, i in proc_data.parameters {
				if i > 0 do fmt.print(", ")
				print_node(p, param)
			}
			fmt.print(") -> ")
			print_node(p, proc_data.return_type)
			print_node(p, proc_data.body, indent)
		case .For:
			for_data := p.data[node.data].for_stmt
			print_indent(indent)
			fmt.print("for ")
			print_node(p, for_data.initial)
			fmt.print("; ")
			print_node(p, for_data.condition)
			fmt.print("; ")
			print_node(p, for_data.update)
			print_node(p, for_data.body, indent)
		case .Return:
			return_data := p.data[node.data].return_stmt
			print_indent(indent)
			fmt.print("return ")
			print_node(p, return_data.value)
			fmt.println(";")
		case .ExprStmt:
			expr_data := p.data[node.data].expr_stmt
			print_indent(indent)
			print_node(p, expr_data.expression)
			fmt.println(";")
		case .Call:
			call_data := p.data[node.data].call_expr
			print_node(p, call_data.callee)
			fmt.print("(")
			for arg, i in call_data.arguments {
				if i > 0 do fmt.print(", ")
				print_node(p, arg)
			}
			fmt.print(")")
		case .Assignment:
			assign_data := p.data[node.data].assign_expr
			print_node(p, assign_data.left)
			fmt.print(" = ")
			print_node(p, assign_data.right)
		case .Addition:
			add_data := p.data[node.data].add_expr
			print_node(p, add_data.left)
			fmt.print(" + ")
			print_node(p, add_data.right)
		case .Multiplication:
			mul_data := p.data[node.data].mul_expr
			print_node(p, mul_data.left)
			fmt.print(" * ")
			print_node(p, mul_data.right)
		case .Less:
			less_data := p.data[node.data].less_expr
			print_node(p, less_data.left)
			fmt.print(" < ")
			print_node(p, less_data.right)
		}
	}

	print_node(p, NodeIndex(len(p.nodes) - 1))
}

parse_atom :: proc(p: ^Parser) -> NodeIndex {
	#partial switch peek(p) {
	case .Identifier:
		token := p.cursor
		expect(p, .Identifier)
		return add_node(p, Node{.Identifier, INVALID_DATA, token})
	case .Number:
		token := p.cursor
		expect(p, .Number)
		return add_node(p, Node{.Integer, INVALID_DATA, token})
	case .True, .False:
		panic("todo")
	case .LeftParen:
		panic("todo")
	case:
		panic(fmt.tprintf("unexpected token: %v", peek(p)))
	}
}

postfix_prec :: proc(kind: lexer.TokenKind) -> int {
	#partial switch kind {
	case .LeftParen:
		return 7
	case:
		return 0
	}
}

is_binary_op :: proc(kind: lexer.TokenKind) -> bool {
	#partial switch kind {
	case .Equal, .Plus, .Asterisk, .Less:
		return true
	case:
		return false
	}
}

infix_prec :: proc(kind: lexer.TokenKind) -> int {
	#partial switch kind {
	case .Equal:
		return 1
	case .Less:
		return 2
	case .Plus:
		return 3
	case .Asterisk:
		return 4
	case:
		return 0
	}
}

is_right_associative :: proc(kind: lexer.TokenKind) -> bool {
	#partial switch kind {
	case .Equal:
		return true
	case .Plus, .Asterisk, .Less:
		return false
	case:
		return false
	}
}

parse_expr :: proc(p: ^Parser, min_prec := 0) -> NodeIndex {
	left := parse_atom(p)

	for postfix_prec(peek(p)) > min_prec {
		#partial switch peek(p) {
		case .LeftParen:
			token := p.cursor
			next(p)
			args := make([dynamic]NodeIndex)
			if !allow(p, .RightParen) {
				append(&args, parse_expr(p))
				for allow(p, .Comma) {
					append(&args, parse_expr(p))
				}
				expect(p, .RightParen)
			}
			data := NodeData{}
			data.call_expr.callee = left
			data.call_expr.arguments = args[:]
			left = add_node(p, Node{.Call, add_data(p, data), token})
		case:
			panic("unreachable")
		}
	}

	// is this is_binary_op call necessary?
	for is_binary_op(peek(p)) && infix_prec(peek(p)) >= min_prec {
		token := p.cursor
		op_kind := peek(p)
		prec := infix_prec(op_kind)

		next_min_prec: int
		if is_right_associative(op_kind) {
			next_min_prec = prec
		} else {
			next_min_prec = prec + 1
		}

		next(p)
		right := parse_expr(p, next_min_prec)

		#partial switch op_kind {
		case .Equal:
			data := NodeData{}
			data.assign_expr.left = left
			data.assign_expr.right = right
			left = add_node(p, Node{.Assignment, add_data(p, data), token})
		case .Plus:
			data := NodeData{}
			data.add_expr.left = left
			data.add_expr.right = right
			left = add_node(p, Node{.Addition, add_data(p, data), token})
		case .Asterisk:
			data := NodeData{}
			data.mul_expr.left = left
			data.mul_expr.right = right
			left = add_node(p, Node{.Multiplication, add_data(p, data), token})
		case .Less:
			data := NodeData{}
			data.less_expr.left = left
			data.less_expr.right = right
			left = add_node(p, Node{.Less, add_data(p, data), token})
		case:
			for node in p.nodes {
				fmt.println("NODE: %v", node)
			}
			panic(fmt.tprintf("unexpected binary operator: %v", op_kind))
		}
	}

	return left
}

parse_type :: proc(p: ^Parser) -> NodeIndex {
	#partial switch peek(p) {
	case .Identifier:
		token := p.cursor
		next(p)
		return add_node(p, Node{.Primitive, INVALID_DATA, token})
	case:
		panic(fmt.tprintf("unexpected type token: %v", peek(p)))
	}
}

parse_param :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor
	expect(p, .Identifier)
	expect(p, .Colon)
	type := parse_type(p)

	data := NodeData{}
	data.param_stmt.type = type
	data.param_stmt.value = INVALID_NODE

	return add_node(p, Node{.Parameter, add_data(p, data), token})
}

parse_stmt :: proc(p: ^Parser) -> NodeIndex {
	token := p.cursor

	#partial switch peek(p) {
	case .Identifier:
		#partial switch peek_next(p) {
		case .Var:
			expect(p, .Identifier)
			expect(p, .Var)
			value := parse_expr(p)
			expect(p, .Semicolon)

			data := NodeData{}
			data.var_stmt.type = INVALID_NODE
			data.var_stmt.value = value

			return add_node(p, Node{.Variable, add_data(p, data), token})
		case:
			expression := parse_expr(p)
			expect(p, .Semicolon)

			data := NodeData{}
			data.expr_stmt.expression = expression

			return add_node(p, Node{.ExprStmt, add_data(p, data), token})
		}
	case .Def:
		next(p)

		identifier := p.cursor
		expect(p, .Identifier)

		parameters := make([dynamic]NodeIndex)
		if allow(p, .LeftParen) {
			if peek(p) != .RightParen {
				append(&parameters, parse_param(p))
				for allow(p, .Comma) {
					append(&parameters, parse_param(p))
				}
			}
			expect(p, .RightParen)
		}

		expect(p, .Arrow)
		return_type := parse_type(p)

		body := parse_stmt(p)

		data := NodeData{}
		data.proc_stmt.identifier = identifier
		data.proc_stmt.return_type = return_type
		data.proc_stmt.body = body
		data.proc_stmt.parameters = parameters[:]

		return add_node(p, Node{.Procedure, add_data(p, data), token})
	case .For:
		next(p)
		initial := parse_stmt(p)
		condition := parse_expr(p)
		expect(p, .Semicolon)
		update := parse_expr(p)
		body := parse_stmt(p)

		data := NodeData{}
		data.for_stmt.initial = initial
		data.for_stmt.condition = condition
		data.for_stmt.update = update
		data.for_stmt.body = body

		return add_node(p, Node{.For, add_data(p, data), token})
	case .Return:
		next(p)
		value := parse_expr(p)
		expect(p, .Semicolon)

		data := NodeData{}
		data.return_stmt.value = value

		return add_node(p, Node{.Return, add_data(p, data), token})
	case .LeftBrace:
		next(p)
		stmts := make([dynamic]NodeIndex)
		for peek(p) != .RightBrace && peek(p) != .Eof {
			append(&stmts, parse_stmt(p))
		}
		expect(p, .RightBrace)

		data := NodeData{}
		data.block_statement.statements = stmts[:]

		return add_node(p, Node{.Block, add_data(p, data), token})
	case:
		expression := parse_expr(p)
		expect(p, .Semicolon)

		data := NodeData{}
		data.expr_stmt.expression = expression

		return add_node(p, Node{.ExprStmt, add_data(p, data), token})
	}

	return INVALID_NODE
}

parse :: proc(p: ^Parser) -> [dynamic]Node {
	stmts := make([dynamic]NodeIndex)

	for peek(p) != .Eof {
		append(&stmts, parse_stmt(p))
	}

	return p.nodes
}
