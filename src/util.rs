pub fn union<T:Eq>(v1: ~[T], v2: ~[T]) -> ~[T] {
    let mut v1 = v1;
    for t in v2.move_iter() {
        if !v1.contains(&t) {
            v1.push(t);
        }
    }
    v1
}

