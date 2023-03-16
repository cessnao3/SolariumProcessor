fn ret_type func_name(a, b, c)
{
    def var_type var_name;
    var_name = expression;

    if (a = 3) {
    }

    if (b = 3 && c == 3) {
        return 5;
    }

    return 7;
}

// Base Types -> i16, u16, d16, void, *
// Case Conversions -> as<u16>(expr)
// Functions are just normal variables with an address, as long as stack matches, call will work
// Types are checked at compile time
// Conversions are provided -> no automatic conversions

fn void main() {

}
