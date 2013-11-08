use cx;

#[deriving(Eq,Clone,IterBytes)]
pub struct Id {
    repr: uint // index into context's identifiers list
}

pub fn Id(u: uint) -> Id {
    Id { repr: u }
}

pub struct Interner {
    identifiers: ~[~str],
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            identifiers: ~[]
        }
    }

    pub fn id(&mut self, s: &str) -> Id {
        match self.identifiers.iter().enumerate().find(|&(_,p)| p.slice(0, p.len()) == s) {
            Some((i,_)) => Id(i),
            None => {
                self.identifiers.push(s.to_owned());
                Id(self.identifiers.len() - 1)
            }
        }
    }

    pub fn int_identifier(&mut self, i: uint) -> Id {
        self.id(format!("{}", i))
    }
}

impl cx::Describe for Id {
    fn describe(&self, cx: &cx::Context, out: &mut ~str) {
        out.push_str(cx.interner.identifiers[self.repr]);
    }
}

