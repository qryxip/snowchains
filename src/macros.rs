macro_rules! quick_main_colored {
    ($main: expr) => {
        fn main() {
            use terminal::Color;

            use std::process;

            if let Err(e) = $main() {
                eprint_bold!(Color::Fatal, "\nError: ");
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
    };
}

macro_rules! return_none_if {
    ($x: expr) => {
        if $x {
            return None;
        }
    };
}

macro_rules! return_none_unless {
    ($x: expr) => {
        if !$x {
            return None;
        }
    };
}

macro_rules! eprint_and_flush {
    ($($arg: tt)*) => {
        {
            use std::io::{self, Write};
            io::stderr().write_fmt(format_args!($($arg)*)).unwrap();
            io::stderr().flush().unwrap();
        }
    }
}

macro_rules! print_bold {
    ($color: expr, $($arg: tt)*) => {
        ::terminal::print_bold($color, format_args!($($arg)*))
    }
}

macro_rules! println_bold {
    ($color: expr, $($arg: tt)*) => {
        ::terminal::println_bold($color, format_args!($($arg)*))
    }
}

macro_rules! eprint_bold {
    ($color: expr, $($arg: tt)*) => {
        ::terminal::eprint_bold($color, format_args!($($arg)*))
    }
}

macro_rules! eprintln_bold {
    ($color: expr, $($arg: tt)*) => {
        ::terminal::eprintln_bold($color, format_args!($($arg)*))
    }
}
