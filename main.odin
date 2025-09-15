package main

import "parser"
import "scanner"
import "sema"

import "core:fmt"
import "core:os"

import "core:mem"
import vmem "core:mem/virtual"

// stages of compilation
// scanner -> parser -> sema -> tac -> pal

main :: proc() {
	arena: vmem.Arena
	arena_err := vmem.arena_init_growing(&arena, reserved = mem.Gigabyte)
	ensure(arena_err == nil)
	context.allocator = vmem.arena_allocator(&arena)
	defer free_all(context.allocator)
	defer free_all(context.temp_allocator)

	handle, open_err := os.open("examples/call.lang")
	defer os.close(handle)

	if open_err != os.ERROR_NONE {
		fmt.eprintf("failed to open file: %v\n", open_err)
		os.exit(1)
	}

	source, read_ok := os.read_entire_file(handle)
	defer delete(source)

	if !read_ok {
		fmt.eprintf("failed to read file: %v\n", open_err)
		os.exit(1)
	}

	fmt.println(string(source))

	p := parser.make_parser(source)
	ast := parser.parse(&p)

	for node, i in ast {
		fmt.println(i, node)
	}
	fmt.println()

	parser.print_tree(&p)
	fmt.println()

	fmt.println("size_of(parser.Node) =", size_of(parser.Node))
	fmt.println("size of AST in bytes =", size_of(parser.Node) * len(ast))
	fmt.println()

	a := sema.make_analyzer(&p)
	sema.analyze(&a)

	for type in a.types {
		fmt.println(type)
	}
	fmt.println()

	for symbol in a.symbols {
		fmt.println(symbol)
	}
	fmt.println()
}
