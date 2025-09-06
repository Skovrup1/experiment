package main

import "parser"
import "scanner"

import "core:fmt"
import "core:os"

import "core:mem"
import vmem "core:mem/virtual"

main :: proc() {
	arena: vmem.Arena
	arena_err := vmem.arena_init_growing(&arena, reserved = mem.Gigabyte)
	ensure(arena_err == nil)
	context.allocator = vmem.arena_allocator(&arena)
	defer free_all(context.allocator)
	defer free_all(context.temp_allocator)

	handle, open_err := os.open("examples/operators.lang")
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
	parse_tree := parser.parse(&p)

	for node, i in parse_tree {
		fmt.println(i, node)
	}
	fmt.println()

	parser.print_tree(&p)
	fmt.println()

	fmt.println("size_of(parser.Node) =", size_of(parser.Node))
	fmt.println()
}
