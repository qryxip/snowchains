macro_rules! ensure_opt {
    ($x:expr) => {
        if !$x {
            return None;
        }
    };
}

macro_rules! println_plural {
    ($format:tt, $n:expr, $singular:expr, $plural:expr) => {
        println!(
            $format,
            format_args!("{} {}", $n, if $n > 1 { $plural } else { $singular })
        )
    };
}

macro_rules! print_bold {
    ($color: expr, $($arg: tt)*) => {
        $crate::terminal::print_bold($color, format_args!($($arg)*))
    }
}

macro_rules! println_bold {
    ($color: expr, $($arg: tt)*) => {
        $crate::terminal::println_bold($color, format_args!($($arg)*))
    }
}

#[macro_export]
macro_rules! eprint_bold {
    ($color: expr, $($arg: tt)*) => {
        $crate::terminal::eprint_bold($color, format_args!($($arg)*))
    }
}

#[macro_export]
macro_rules! eprintln_bold {
    ($color: expr, $($arg: tt)*) => {
        $crate::terminal::eprintln_bold($color, format_args!($($arg)*))
    }
}
