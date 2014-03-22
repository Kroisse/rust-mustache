pub struct IteratorChain<T, U> {
    priv iterators: T,
    priv current: Option<U>,
}

impl<T: Iterator<U>, U: Iterator<V>, V> IteratorChain<T, U> {
    pub fn new(iterators: T) -> IteratorChain<T, U> {
        IteratorChain { iterators: iterators, current: None }
    }
}

impl<T: Iterator<U>, U: Iterator<V>, V> Iterator<V> for IteratorChain<T, U> {
    fn next(&mut self) -> Option<V> {
        loop {
            if self.current.is_some() {
                let value = self.current.get_mut_ref().next();
                if value.is_some() {
                    return value;
                } else {
                    self.current = None;
                }
            } else {
                match self.iterators.next() {
                    Some(it) => { self.current = Some(it); }
                    None => { return None; }
                }
            }
        }
    }
}
