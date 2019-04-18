use ego_tree::NodeRef;
use scraper::{ElementRef, Node, Selector};

pub(crate) fn or(selector1: &Selector, selector2: &Selector) -> Selector {
    let mut ret = selector1.clone();
    ret.selectors.extend(selector2.selectors.clone());
    ret
}

type ChildrenText<'a> = std::iter::FlatMap<
    ego_tree::iter::Children<'a, Node>,
    Option<&'a str>,
    fn(NodeRef<'a, Node>) -> Option<&'a str>,
>;

pub(crate) trait ElementRefExt {
    type ChildrenText;
    fn children_text(&self) -> Self::ChildrenText;
    fn collect_text(&self) -> String;
}

impl<'a> ElementRefExt for ElementRef<'a> {
    type ChildrenText = ChildrenText<'a>;

    fn children_text(&self) -> Self::ChildrenText {
        self.children().flat_map(|node| match node.value() {
            Node::Text(t) => Some(t.as_ref()),
            _ => None,
        })
    }

    fn collect_text(&self) -> String {
        self.text().fold("".to_owned(), |mut r, s| {
            r.push_str(s);
            r
        })
    }
}
