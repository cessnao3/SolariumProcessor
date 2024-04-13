def var_name: i32;
def array_name: [5]i32;
def int_test: u16 = 3u16;
def ptr_test: *u16 = 6u32;
def void_test: *void = 0u32;
def void_er: void;

def func_ptr: ^(*u8, *u16, *u32): u32 = 3049;

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

/* asmfn asm_func(a, b, c) ret_type
{
    asm1
    asm1
    asm1
}
*/

fn func_name_ptr(a, b) ret_type = 3943;

fn func_name(a: u16, b: u32, c: u32) ret_type
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

fn main() {

}
