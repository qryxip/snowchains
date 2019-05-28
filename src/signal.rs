use futures::{Async, Poll, Stream};

use std::io;
use std::sync::atomic::{self, AtomicBool};

static CTRL_C: AtomicBool = AtomicBool::new(false);

pub fn start_catching_ctrl_c() -> std::result::Result<(), ctrlc::Error> {
    ctrlc::set_handler(|| CTRL_C.store(true, atomic::Ordering::SeqCst))
}

#[inline]
pub(crate) fn check_ctrl_c() -> io::Result<()> {
    if CTRL_C.load(atomic::Ordering::SeqCst) {
        Err(io::Error::new(io::ErrorKind::Interrupted, "Interrupted"))
    } else {
        Ok(())
    }
}

#[cfg(unix)]
#[derive(Debug)]
pub(crate) struct Sigwinch(signal_hook::iterator::Async);

#[cfg(not(unix))]
#[derive(Debug)]
pub(crate) struct Sigwinch(());

impl Sigwinch {
    #[cfg(unix)]
    pub(crate) fn try_new() -> io::Result<Self> {
        use signal_hook::iterator::Signals;

        let inner = Signals::new(&[signal_hook::SIGWINCH])?.into_async()?;
        Ok(Self(inner))
    }

    #[cfg(not(unix))]
    pub(crate) fn try_new() -> io::Result<Self> {
        Ok(Self(()))
    }
}

impl Stream for Sigwinch {
    type Item = ();
    type Error = io::Error;

    #[cfg(unix)]
    #[inline]
    fn poll(&mut self) -> Poll<Option<()>, io::Error> {
        use futures::try_ready;

        try_ready!(self.0.poll());
        Ok(Async::Ready(Some(())))
    }

    #[cfg(not(unix))]
    #[inline]
    fn poll(&mut self) -> Poll<Option<()>, io::Error> {
        Ok(Async::NotReady)
    }
}
