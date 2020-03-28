pub trait TypeInfo {
    fn type_info(&self) -> &'static str;
}

impl<'a> TypeInfo for String {
    fn type_info(&self) -> &'static str {
        "String"
    }
}
