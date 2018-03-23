macro_rules! quick_main_colored {
    ($main: expr) => {
        fn main() {
            ::errors::quick_main_colored($main)
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

macro_rules! println_plural {
    ($format: tt, $n: expr, $singular: expr, $plural: expr) => {
        println!(
            $format,
            format_args!("{} {}", $n, if $n > 1 { $plural } else { $singular })
        )
    };
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
