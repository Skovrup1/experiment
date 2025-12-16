package tac

import "core:fmt"

import "../parser"
import "../sema"

InstIndex :: distinct u32
INVALID_INST :: max(InstIndex)

BlockIndex :: distinct u32
INVALID_BLOCK :: max(BlockIndex)

Proccedure :: struct {
	entry:  BlockIndex,
	blocks: []Block,
}

Block :: struct {
	sequence: [dynamic]InstIndex,
}

InstData :: struct #raw_union {
	using binary:    struct {
		left:  InstIndex,
		right: InstIndex,
	},
	using immediate: struct {
		value: i64,
	},
}

InstKind :: enum u8 {
	Add,
	Const,
}

Inst :: struct {
	kind:       InstKind,
	result:     InstIndex,
	using data: InstData,
}

Builder :: struct {
	nodes:         []parser.Node,
	node_data:     []u32,
	node_types:    []sema.TypeIndex,
	node_symbols:  map[parser.NodeIndex]sema.SymbolIndex,
	symbols:       []sema.Symbol,
	symbol_values: []InstIndex,
	instructions:  [dynamic]Inst,
	blocks:        [dynamic]Block,
}

make_builder :: proc(
	nodes: []parser.Node,
	node_data: []u32,
	node_types: []sema.TypeIndex,
	node_symbols: map[parser.NodeIndex]sema.SymbolIndex,
	symbols: []sema.Symbol,
) -> Builder {
	symbol_values := make([]InstIndex, len(symbols))

	instructions := make([dynamic]Inst)
	blocks := make([dynamic]Block)

	return {
		nodes,
		node_data,
		node_types,
		node_symbols,
		symbols,
		symbol_values,
		instructions,
		blocks,
	}
}

new_block :: proc(b: ^Builder) -> BlockIndex {
	append(&b.blocks, Block{})
	return BlockIndex(len(b.blocks) - 1)
}

emit_inst :: proc(b: ^Builder, inst: Inst) -> InstIndex {
	append(&b.instructions, inst)
	index := InstIndex(len(b.instructions) - 1)

	current_block := len(b.blocks) - 1
	append(&b.blocks[current_block].sequence, index)

	return index
}

lower_expr :: proc(b: ^Builder, node_index: parser.NodeIndex) -> InstIndex {
	node := b.nodes[node_index]

	#partial switch node.kind {
	case .Integer:
		int_lit := parser.decode_data(b.node_data, node.data, parser.IntLit)

		inst := Inst {
			kind   = .Const,
			result = INVALID_INST,
			value  = int_lit.value,
		}

		result := emit_inst(b, inst)
		b.instructions[result].result = result
		return result
	case .Identifier:
		if symbol_index, ok := b.node_symbols[node_index]; ok {
			return b.symbol_values[symbol_index]
		} else {
			panic("undefined symbol")
		}
	case .Addition:
		add_expr := parser.decode_data(b.node_data, node.data, parser.AddExpr)

		left := lower_expr(b, add_expr.left)
		right := lower_expr(b, add_expr.right)

		inst := Inst {
			kind   = .Add,
			result = INVALID_INST,
			left   = left,
			right  = right,
		}

		result := emit_inst(b, inst)
		b.instructions[result].result = result
		return result
	case:
		panic(fmt.tprintf("unhandled lower_expr, %v", node.kind))
	}

	return INVALID_INST
}

lower_stmt :: proc(b: ^Builder, node_index: parser.NodeIndex) -> InstIndex {
	node := b.nodes[node_index]

	#partial switch node.kind {
	case .Block:
	case .Variable:
	case .Parameter:
	case .Return:
	case .Procedure:
	case:
		panic(fmt.tprintf("unhandled lower_stmt, %v", node.kind))
	}

	return INVALID_INST
}

do_stuff :: proc(b: ^Builder) {
	entry := new_block(b)
	result := lower_expr(b, 7)

	fmt.println(result)
	fmt.println()

	for block, i in b.blocks {
		fmt.println(i, block)
	}

	for inst, i in b.instructions {
		fmt.println(i, inst)
	}
}
