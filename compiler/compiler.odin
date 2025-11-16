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
	defer vmem.arena_destroy(&arena)

	handle, open_err := os.open("examples/test.lang")
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
	ast := parser.parse(&p)

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

	fmt.println(parser.ast_to_string(&p))

	fmt.println(size_of(parser.Node))
	fmt.println(len(p.nodes))

	fmt.println(size_of(parser.Node) * len(p.nodes))
	fmt.println(size_of(u32) * len(p.data))
}
