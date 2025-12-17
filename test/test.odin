package test

import "../compiler/lexer"
import "../compiler/parser"

import "core:fmt"
import "core:math/rand"
import "core:mem"

run_property_test :: proc(iterations: int, failures_only := true) {
	track: mem.Tracking_Allocator
	mem.tracking_allocator_init(&track, context.allocator)
	context.allocator = mem.tracking_allocator(&track)
	defer free_all(context.allocator)
	defer mem.tracking_allocator_destroy(&track)

	passes := 0
	failures := 0

	fmt.printf("running %d property tests\n", iterations)

	for test_number in 0 ..< iterations {
		free_all(context.allocator)

		r := rand.create(u64(test_number + 1))

		p1 := parser.make_parser("", {})
		p1_tokens := make([dynamic]lexer.Token)

		proc_node := gen_module(&p1, &p1_tokens)

		// note: ugly fix
		p1.tokens = p1_tokens[:]

		program := parser.program_to_string(&p1)

		s := lexer.make_scanner(program)
		tokens := lexer.scan_tokens(&s)
		p2 := parser.make_parser(program, tokens[:])
		parser.parse(&p2)

		root_node_idx := parser.NodeIndex(len(p2.nodes) - 1)
		root_node := p2.nodes[root_node_idx]

		ensure(len(p2.nodes) > 0)

		if compare_nodes(&p1, proc_node, &p2, root_node_idx) {
			passes += 1
			if !failures_only {
				fmt.printf("pass test %d\n", test_number + 1)
				fmt.println("original program:")
				fmt.println(program)
				fmt.println("reparsed program:")
				fmt.println(parser.program_to_string(&p2))
			}
		} else {
			fmt.printf("fail test %d: ast mismatch\n", test_number + 1)
			fmt.println("original AST:")
			fmt.println(parser.program_to_string(&p1))
			fmt.println("reparsed AST:")
			fmt.println(parser.program_to_string(&p2))
			fmt.println()
			failures += 1
		}
	}

	fmt.println("=== results ===")
	fmt.printf("passes: %d\n", passes)
	fmt.printf("failures: %d\n", failures)
	fmt.printf("success rate: %.1f%%\n", f64(passes) / f64(iterations) * 100)
}

main :: proc() {
	run_property_test(1000, true)
}
