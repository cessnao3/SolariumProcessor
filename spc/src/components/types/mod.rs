trait Type {
    fn get_name() -> String;

    fn size_in_words() -> usize;
}

trait BaseType: Type {
    fn pre_math_instructions(&self) -> Vec<String>;

    fn post_math_instructions(&self) -> Vec<String>;
}

struct WordType {

}

impl Type for WordType {

}

struct UnsignedWordType {

}

struct DecimalWordType {

}
