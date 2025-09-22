package test

import "../scanner"
import "../parser"
import "../sema"

import "core:fmt"
import "core:os"
import "core:testing"

@(test)
struct_declaration :: proc(t: ^testing.T) {
	compile("examples/tests/struct_declaration.lang")
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

@(test)
procedure_with_params :: proc(t: ^testing.T) {
	compile("examples/tests/procedure_with_params.lang")
}

@(test)
loop_with_increment :: proc(t: ^testing.T) {
	compile("examples/tests/loop_with_increment.lang")
}

@(test)
while_loop :: proc(t: ^testing.T) {
	compile("examples/tests/while_loop.lang")
}

@(test)
bitwise_operators :: proc(t: ^testing.T) {
	compile("examples/tests/bitwise_operators.lang")
}

@(test)
assignment_operators :: proc(t: ^testing.T) {
	compile("examples/tests/assignment_operators.lang")
}

@(test)
comparison_operators :: proc(t: ^testing.T) {
	compile("examples/tests/comparison_operators.lang")
}

@(test)
compound_assignment :: proc(t: ^testing.T) {
	compile("examples/tests/compound_assignment.lang")
}

@(test)
multiple_declarations :: proc(t: ^testing.T) {
	compile("examples/tests/multiple_declarations.lang")
}

@(test)
nested_expressions :: proc(t: ^testing.T) {
	compile("examples/tests/nested_expressions.lang")
}

@(test)
if_else_nested :: proc(t: ^testing.T) {
	compile("examples/tests/if_else_nested.lang")
}

@(test)
array_literal :: proc(t: ^testing.T) {
	compile("examples/tests/array_literal.lang")
}

@(test)
struct_literal :: proc(t: ^testing.T) {
	compile("examples/tests/struct_literal.lang")
}

@(test)
nested_structs :: proc(t: ^testing.T) {
	compile("examples/tests/nested_structs.lang")
}

@(test)
procedure_with_default_params :: proc(t: ^testing.T) {
	compile("examples/tests/procedure_with_default_params.lang")
}

@(test)
union_declaration :: proc(t: ^testing.T) {
	compile("examples/tests/union_declaration.lang")
}

@(test)
union_usage :: proc(t: ^testing.T) {
	compile("examples/tests/union_usage.lang")
}

@(test)
tuple_declaration :: proc(t: ^testing.T) {
	compile("examples/tests/tuple_declaration.lang")
}

@(test)
tuple_usage :: proc(t: ^testing.T) {
	compile("examples/tests/tuple_usage.lang")
}

@(test)
enum_declaration :: proc(t: ^testing.T) {
	compile("examples/tests/enum_declaration.lang")
}

@(test)
enum_usage :: proc(t: ^testing.T) {
	compile("examples/tests/enum_usage.lang")
}

@(test)
enum_with_type :: proc(t: ^testing.T) {
	compile("examples/tests/enum_with_type.lang")
}

@(test)
break_statement :: proc(t: ^testing.T) {
	compile("examples/tests/break_statement.lang")
}

@(test)
continue_statement :: proc(t: ^testing.T) {
	compile("examples/tests/continue_statement.lang")
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

	s := scanner.make_scanner(source)
	scanner.consume_all(&s)

	p := parser.make_parser(source, s.tokens[:], path)
	ast := parser.parse(&p)

	a := sema.make_analyzer(&p)
	sema.analyze(&a)
}
