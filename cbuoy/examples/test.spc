def var_name: i32;
def array_name: [5]i32;
def int_test: u16 = 3;
def ptr_test: *u16 = 6;
def void_test: *void = 0;
def void_er: void;

def func_ptr: ^ret_type(type_1, type_2, type_3) = 3049;

struct type_name
{
    var1: type1;
    var2: type2;
}

asmfn asm_func(a, b, c): ret_type
{
    asm
    asm
    asm
}

/* asmfn asm_func(a, b, c): ret_type
{
    asm1
    asm1
    asm1
}
*/

fn func_name_ptr(a, b): ret_type = 3943;

fn func_name(a, b, c): ret_type
{
    def var_name: var_type;
    var_name = expression;

    if (a = 3) {
    }

    if (b = 3 && c == 3) {
        return 5;
    }

    if (b=4&&c==6) {
        return 5;
    }

    return 7;
}

// Base Types -> i16, u16, d16, void, *
// Case Conversions -> as<u16>(expr)
// Functions are just normal variables with an address, as long as stack matches, call will work
// Types are checked at compile time
// Conversions are provided -> no automatic conversions

fn main(): void {

}
