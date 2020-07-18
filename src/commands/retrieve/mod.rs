pub(crate) mod testcases;

use crate::commands::retrieve::testcases::OptRetrieveTestcases;
use std::io::BufRead;
use structopt::StructOpt;
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
pub enum OptRetrieve {
    /// Retrieves test cases
    #[structopt(author, visible_alias("t"))]
    Testcases(OptRetrieveTestcases),
}

pub(crate) fn run(
    opt: OptRetrieve,
    ctx: crate::Context<impl BufRead, impl WriteColor, impl WriteColor>,
) -> anyhow::Result<()> {
    match opt {
        OptRetrieve::Testcases(opt) => testcases::run(opt, ctx),
    }
}
