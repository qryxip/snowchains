macro_rules! print_and_flush {
    ($format: tt$(, $x: expr)*) => {
        {
            use std::io::{self, Write};

            print!($format$(, $x)*);
            io::stdout().flush().unwrap();
        }
    }
}

macro_rules! print_decorated {
    ($attr:expr, $color: expr, $format: tt$(, $x: expr)*) => (
        __write_decorated!(stdout, write, $attr, $color, $format$(, $x)*)
    );
}


macro_rules! println_decorated {
    ($attr:expr, $color: expr, $format: tt$(, $x: expr)*) => (
        __write_decorated!(stdout, writeln, $attr, $color, $format$(, $x)*)
    );
}


macro_rules! write_error_decorated {
    ($attr:expr, $color: expr, $format: tt) => (
        __write_decorated!(stderr, write, $attr, $color, $format)
    );
}


macro_rules! writeln_error_decorated {
    ($attr:expr, $color: expr, $format: tt$(, $x: expr)*) => (
        __write_decorated!(stderr, writeln, $attr, $color, $format$(, $x)*)
    );
}


macro_rules! __write_decorated {
    ($out: ident, $write: ident, $attr: expr, $color: expr, $format: tt$(, $x: expr)*) => {
        {
            use std::io::{self, Write};
            use term;

            if let Some(mut term) = term::$out() {
                if let Ok(_) = term.attr($attr) {
                    if let Some(color) = $color {
                        let _ = term.fg(color);
                    }
                    $write!(term, $format$(, $x)*).unwrap();
                    term.reset().unwrap();
                } else {
                    $write!(io::$out(), $format$(, $x)*).unwrap();
                }
            } else {
                $write!(io::$out(), $format$(, $x)*).unwrap();
            }
            io::$out().flush().unwrap();
        }
    };
}
