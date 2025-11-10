package compiler

import "lexer"
import "parser"

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

	handle, open_err := os.open("examples/for.lang")
	defer os.close(handle)

	ensure(open_err == os.ERROR_NONE)

	source_buffer, read_ok := os.read_entire_file(handle)
	ensure(read_ok == true)

    source := string(source_buffer)
	fmt.println(source)

	l := lexer.make_scanner(source)

	tokens := lexer.scan_tokens(&l)
	for token, i in tokens {
		fmt.println(i, token)
	}

	p := parser.make_parser(source, tokens[:])
	ast := parser.parse(&p)

	for node, i in p.nodes {
		fmt.println(i, node)
	}

	parser.print_ast(&p)

    parser.print_program(&p)

    fmt.println(size_of(parser.NodeData))
    fmt.println(size_of(parser.NodeData) * len(p.data))
}
