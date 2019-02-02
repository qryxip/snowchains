use futures::{Future, Stream};
use tokio::timer::Interval;

use std::io;
use std::sync::atomic::{self, AtomicBool};
use std::time::Duration;

static CTRL_C: AtomicBool = AtomicBool::new(false);

const DURATION: Duration = Duration::from_millis(20);

pub fn start_catching_ctrl_c() -> std::result::Result<(), ctrlc::Error> {
    ctrlc::set_handler(|| CTRL_C.store(true, atomic::Ordering::SeqCst))
}

#[inline]
pub(crate) fn ctrl_c<T, E: From<io::Error> + From<tokio::timer::Error>>(
) -> impl Future<Item = T, Error = E> {
    check_ctrl_c(&CTRL_C)
}

fn check_ctrl_c<T, E: From<io::Error> + From<tokio::timer::Error>>(
    atomic: &'static AtomicBool,
) -> impl Future<Item = T, Error = E> {
    Interval::new_interval(DURATION)
        .filter(move |_| atomic.load(atomic::Ordering::SeqCst))
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

#[cfg(test)]
mod tests {
    use failure::Fallible;
    use futures::Future;
    use if_chain::if_chain;
    use tokio::runtime::Runtime;
    use tokio::timer::Delay;
    use tokio::util::FutureExt;

    use std::io;
    use std::sync::atomic::{self, AtomicBool};
    use std::time::{Duration, Instant};

    #[test]
    fn test_check_ctrl_c() -> Fallible<()> {
        static CTRL_C: AtomicBool = AtomicBool::new(false);

        let mut rt = Runtime::new()?;
        let send = Delay::new(Instant::now() + super::DURATION + Duration::from_millis(10))
            .inspect(|&()| CTRL_C.store(true, atomic::Ordering::SeqCst))
            .map_err(failure::Error::from);
        let recv = super::check_ctrl_c::<(), failure::Error>(&CTRL_C);
        if_chain! {
            let err = rt
                .block_on(send.join(recv).timeout(Duration::from_millis(100)))
                .unwrap_err()
                .into_inner()
                .unwrap();
            if let Some(io_err) = err.downcast_ref::<io::Error>();
            if io_err.kind() ==  io::ErrorKind::Interrupted && io_err.to_string() == "Interrupted";
            then {} else { return Err(err) }
        }
        let _ = rt.shutdown_now().wait();
        assert!(CTRL_C.load(atomic::Ordering::SeqCst));
        Ok(())
    }
}
