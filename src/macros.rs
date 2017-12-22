macro_rules! quick_main_colored {
    ($main: expr) => {
        fn main() {
            if let Err(e) = $main() {
                use term::color;
                use std::process;
                eprint_bold!(Some(color::RED), "\nError: ");
                eprintln!("{}", e);
                for e_kind in e.iter().skip(1) {
                    eprint_bold!(None, "Caused by: ");
                    eprintln!("{}", e_kind);
                }
                if let Some(backtrace) = e.backtrace() {
                    eprintln!("{:?}", backtrace);
                }
                process::exit(1);
            }
        }
    }
}

macro_rules! return_none_if {
    ($x: expr) => {
        if $x { return None; }
    }
}

macro_rules! return_none_unless {
    ($x: expr) => {
        if !$x { return None; }
    }
}

macro_rules! print_and_flush {
    ($format: tt $(, $x: expr)*) => {
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

macro_rules! print_bold {
    ($color: expr, $format: tt $(, $x: expr)*) => {
        _write_decorated!(stdout, write, term::Attr::Bold, $color, $format$(, $x)*)
    }
}

macro_rules! println_bold {
    ($color: expr, $format: tt $(, $x: expr)*) => {
        _write_decorated!(stdout, writeln, term::Attr::Bold, $color, $format$(, $x)*)
    }
}

macro_rules! eprint_bold {
    ($color: expr, $format: tt $(, $x: expr)*) => {
        _write_decorated!(stderr, write, term::Attr::Bold, $color, $format$(, $x)*)
    }
}

macro_rules! eprintln_bold {
    ($color: expr, $format: tt $(, $x: expr)*) => {
        _write_decorated!(stderr, writeln, term::Attr::Bold, $color, $format$(, $x)*)
    }
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
    }
}
