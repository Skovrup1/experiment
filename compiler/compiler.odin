package compiler

import "lexer"
import "parser"
import "sema"

import "core:fmt"
import "core:os"

import "core:mem"
import vmem "core:mem/virtual"

main :: proc() {
	arena: vmem.Arena
	arena_err := vmem.arena_init_growing(&arena, reserved = mem.Gigabyte)
	ensure(arena_err == nil)
	context.allocator = vmem.arena_allocator(&arena)
	defer {
		fmt.printf("arena allocated: %dKB\n", arena.total_used / mem.Kilobyte)

		free_all(context.allocator)
		free_all(context.temp_allocator)
		vmem.arena_destroy(&arena)
	}

	handle, open_err := os.open("examples/for.lang")
	defer os.close(handle)

	ensure(open_err == os.ERROR_NONE)

	source_buffer, read_ok := os.read_entire_file(handle)
	ensure(read_ok == true)

	source := string(source_buffer)
	fmt.println(source)

	s := lexer.make_scanner(source)
	tokens := lexer.scan_tokens(&s)

	if len(s.errors) > 0 {
		for err, i in s.errors {
			fmt.println(i, err)
		}
		fmt.println()
	} else {
		for token, i in tokens {
			fmt.println(i, token)
		}
		fmt.println()
	}

	p := parser.make_parser(source, tokens[:])
	parser.parse(&p)

	if len(p.errors) > 0 {
		for err, i in p.errors {
			fmt.println(i, err)
		}
		fmt.println()
	} else {
		for node, i in p.nodes {
			fmt.println(i, node)
		}
		fmt.println()
	}

	a := sema.make_analyzer(source, tokens[:], p.nodes[:], p.data[:])
	sema.analyze(&a)

	if len(a.errors) > 0 {
		for err, i in a.errors {
			fmt.println(i, err)
		}
		fmt.println()
	} else {
		for symbol, i in a.symbols {
			fmt.println(i, symbol)
		}
		fmt.println()

		for func_i in 0..<len(a.functions) {
			func := a.functions[func_i]
			for block in func.blocks {
				sema.print_block(&a, func, block)
				fmt.println()
			}
		}
	}

	fmt.println("size_of(Node) =", size_of(parser.Node))
	fmt.println("total nodes =", len(p.nodes))

	fmt.println("space taken by nodes =", size_of(parser.Node) * len(p.nodes))
	fmt.println("space taken by node data =", size_of(u32) * len(p.data))
	fmt.println(
		"space taken by AST =",
		size_of(parser.Node) * len(p.nodes) + size_of(u32) * len(p.data),
	)
}
