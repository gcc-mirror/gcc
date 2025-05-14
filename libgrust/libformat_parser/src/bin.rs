use libformat_parser::rust;

fn main() {
    dbg!(rust::collect_pieces(
        std::env::args().nth(1).unwrap().as_str(),
        None,
        None,
        false,
        libformat_parser::ffi::ParseMode::Format,
    ));
}
