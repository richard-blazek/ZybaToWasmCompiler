use std::collections::HashMap;
use std::hash::Hash;

pub fn sorted<A, T: IntoIterator<Item=A>, K: Ord, F: Fn(&A) -> &K>(xs: T, f: F) -> Vec<A> {
    let mut v = Vec::from_iter(xs);
    v.sort_by(|x1, x2| K::cmp(f(x1), f(x2)));
    v
}

pub fn order_of<A: Ord + Hash>(mut xs: Vec<A>) -> HashMap<A, usize> {
    xs.sort();

    let mut indices = HashMap::new();
    indices.reserve(xs.len());

    let mut i = 0;
    for x in xs {
        indices.insert(x, i);
        i += 1;
    }
    indices
}
