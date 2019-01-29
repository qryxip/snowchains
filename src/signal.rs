use futures::{Future, Stream};
use tokio::timer::Interval;

use std::io;
use std::sync::atomic::{self, AtomicBool};
use std::time::Duration;

static CTRL_C: AtomicBool = AtomicBool::new(false);

pub fn start_handling_ctrl_c() -> std::result::Result<(), ctrlc::Error> {
    ctrlc::set_handler(|| CTRL_C.store(true, atomic::Ordering::SeqCst))
}

pub(crate) fn ctrl_c<T, E: From<io::Error> + From<tokio::timer::Error>>(
) -> impl Future<Item = T, Error = E> {
    const DURATION: Duration = Duration::from_millis(20);
    Interval::new_interval(DURATION)
        .filter(|_| CTRL_C.load(atomic::Ordering::SeqCst))
        .take(1)
        .into_future()
        .map_err(|(e, _)| E::from(e))
        .and_then::<_, std::result::Result<T, E>>(|_| {
            Err(E::from(io::Error::new(
                io::ErrorKind::Interrupted,
                "Interrupted",
            )))
        })
}
