macro_rules! quick_main_colored {
    ($main: expr) => {
        fn main() {
            if let Err(e) = $main() {
                use self::error::PrintChainColored;
                use std::process;
                eprint!("\n");
                e.print_chain_colored();
                process::exit(1);
            }
        }
    }
}


macro_rules! try_opt {
    ($option: expr) => {
        match $option {
            Some(x) => x,
            None => return None,
        }
    }
}


macro_rules! return_none_unless {
    ($x: expr) => {
        if !$x {
            return None;
        }
    }
}


macro_rules! print_and_flush {
    ($format: tt$(, $x: expr)*) => {
        {
            use std::io::{self, Write};
            print!($format$(, $x)*);
            io::stdout().flush().unwrap();
        }
    }
}


macro_rules! eprint_and_flush {
    ($format: tt$(, $x: expr)*) => {
        {
            use std::io::{self, Write};
            eprint!($format$(, $x)*);
            io::stderr().flush().unwrap();
        }
    }
}


macro_rules! print_decorated {
    ($attr:expr, $color: expr, $format: tt$(, $x: expr)*) => (
        _write_decorated!(stdout, write, $attr, $color, $format$(, $x)*)
    );
}


macro_rules! println_decorated {
    ($attr:expr, $color: expr, $format: tt$(, $x: expr)*) => (
        _write_decorated!(stdout, writeln, $attr, $color, $format$(, $x)*)
    );
}


macro_rules! eprint_decorated {
    ($attr:expr, $color: expr, $format: tt$(, $x: expr)*) => (
        _write_decorated!(stderr, write, $attr, $color, $format$(, $x)*)
    );
}


macro_rules! eprintln_decorated {
    ($attr:expr, $color: expr, $format: tt$(, $x: expr)*) => (
        _write_decorated!(stderr, writeln, $attr, $color, $format$(, $x)*)
    );
}


macro_rules! _write_decorated {
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
