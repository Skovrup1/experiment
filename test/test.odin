package test

import "../parser"
import "../scanner"
import "../sema"

import "core:c/libc"
import "core:fmt"
import "core:os"
import "core:testing"

@(test)
procedure_test :: proc(t: ^testing.T) {
	compile("examples/tests/procedure.lang")
}

@(test)
if_test :: proc(t: ^testing.T) {
	compile("examples/tests/if.lang")
}

@(test)
loop_test :: proc(t: ^testing.T) {
	compile("examples/tests/loop.lang")
}

@(test)
pointer_test :: proc(t: ^testing.T) {
	compile("examples/tests/pointer.lang")
}

@(test)
array_test :: proc(t: ^testing.T) {
	compile("examples/tests/array.lang")
}

@(test)
struct_test :: proc(t: ^testing.T) {
	compile("examples/tests/struct.lang")
}

@(test)
tuple_test :: proc(t: ^testing.T) {
	compile("examples/tests/tuple.lang")
}

@(test)
union_test :: proc(t: ^testing.T) {
	compile("examples/tests/union.lang")
}

@(test)
enum_test :: proc(t: ^testing.T) {
	compile("examples/tests/enum.lang")
}

@(test)
module_test :: proc(t: ^testing.T) {
	compile("examples/tests/module.lang")
}

@(test)
typedef_test :: proc(t: ^testing.T) {
	compile("examples/tests/typedef.lang")
}

// need to improve support for failing tests
/*
@(test)
type_mismatch_test :: proc(t: ^testing.T) {
	compile("examples/tests/type-mismatch.lang")
}
*/

// this should not fail
/*
@(test)
type_coercion_test :: proc(t: ^testing.T) {
	compile("examples/tests/type-coercion.lang")
}
*/

compile :: proc(path: string) {
	defer free_all(context.allocator)
	defer free_all(context.temp_allocator)

	handle, open_err := os.open(path)
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

	s := scanner.make_scanner(source)
	scanner.consume_all(&s)

	p := parser.make_parser(source, s.tokens[:], path)
	ast := parser.parse(&p)

	a := sema.make_analyzer(&p)
	sema.analyze(&a)
}
