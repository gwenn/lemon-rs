use std::ops::{Index, IndexMut};

pub struct Stack<T> {
    pub yyidx: usize, /* Index to top element of the stack */
    pub vec: Vec<T>,  /* The parser's stack */
}
use std::cmp::Ordering;
use std::ops::Neg as _;
impl<T> Stack<T> {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            yyidx: 0,
            vec: Vec::with_capacity(capacity),
        }
    }
    fn shift(&self, shift: i8) -> usize {
        assert!(shift <= 1);
        match shift.cmp(&0) {
            Ordering::Equal => self.yyidx,
            Ordering::Greater => self.yyidx + shift as usize,
            Ordering::Less => self.yyidx.checked_sub(shift.neg() as usize).unwrap(),
        }
    }

    pub fn yyidx_shift(&mut self, shift: i8) {
        match shift.cmp(&0) {
            Ordering::Greater => self.yyidx += shift as usize,
            Ordering::Less => self.yyidx = self.yyidx.checked_sub(shift.neg() as usize).unwrap(),
            Ordering::Equal => {}
        }
    }

    pub fn yy_move(&mut self, shift: i8) -> T
    where
        T: Default,
    {
        use std::mem::take;
        let idx = self.shift(shift);
        take(&mut self.vec[idx])
    }

    pub fn push(&mut self, entry: T) {
        if self.yyidx == self.vec.len() {
            self.vec.push(entry);
        } else {
            self.vec[self.yyidx] = entry;
        }
    }
    pub fn grow(&mut self)
    where
        T: Default,
    {
        if self.yyidx == self.vec.len() || self.yyidx + 1 == self.vec.len() {
            self.vec.push(T::default());
        }
    }
    pub fn pop(&mut self) -> T
    where
        T: Default,
    {
        use std::mem::take;
        let yytos = take(&mut self.vec[self.yyidx]);
        self.yyidx = self.yyidx.checked_sub(1).unwrap();
        //assert_eq!(self.yyidx+1, self.yystack.len());
        yytos
    }
}

impl<T> Index<i8> for Stack<T> {
    type Output = T;

    fn index(&self, shift: i8) -> &T {
        let idx = self.shift(shift);
        &self.vec[idx]
    }
}
impl<T> IndexMut<i8> for Stack<T> {
    fn index_mut(&mut self, shift: i8) -> &mut T {
        let idx = self.shift(shift);
        &mut self.vec[idx]
    }
}
