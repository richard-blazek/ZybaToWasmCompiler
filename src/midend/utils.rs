pub fn sorted<A, T: IntoIterator<Item=A>, K: Ord, F: Fn(&A) -> &K>(xs: T, f: F) -> Vec<A> {
    let mut v = Vec::from_iter(xs);
    v.sort_by(|x1, x2| K::cmp(f(x1), f(x2)));
    v
}
