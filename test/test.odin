package test

import "../parser"
import "../sema"

import "core:fmt"
import "core:os"
import "core:testing"

array_literal :: proc(t: ^testing.T) {
	compile("examples/tests/array_literal.lang")
}

@(test)
struct_declaration :: proc(t: ^testing.T) {
	compile("examples/tests/struct_declaration.lang")
}

struct_literal :: proc(t: ^testing.T) {
	compile("examples/tests/struct_literal.lang")
}

@(test)
procedure_call :: proc(t: ^testing.T) {
	compile("examples/tests/procedure_call.lang")
}

@(test)
variable_declaration :: proc(t: ^testing.T) {
	compile("examples/tests/variable_declaration.lang")
}

@(test)
if_statement :: proc(t: ^testing.T) {
	compile("examples/tests/if_statement.lang")
}

@(test)
pointer_operations :: proc(t: ^testing.T) {
	compile("examples/tests/pointer_operations.lang")
}

@(test)
arithmetic_operators :: proc(t: ^testing.T) {
	compile("examples/tests/arithmetic_operators.lang")
}

@(test)
logical_operators :: proc(t: ^testing.T) {
	compile("examples/tests/logical_operators.lang")
}

@(test)
type_alias :: proc(t: ^testing.T) {
	compile("examples/tests/type_alias.lang")
}

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

	p := parser.make_parser(source)
	ast := parser.parse(&p)

	a := sema.make_analyzer(&p)
	sema.analyze(&a)
}
