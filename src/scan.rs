use std::io::{self, BufRead, Read};

use token::Token;

// Like a `BufReader` but with a growable buffer.
pub struct Scanner<R> {
    inner: R,
    buf: Vec<u8>,
    pos: usize,
    cap: usize,
    eof: bool,
}

impl<R: Read> Scanner<R> {
    pub fn new(inner: R) -> Scanner<R> {
        Self::with_capacity(inner, 4096)
    }
    fn with_capacity(inner: R, capacity: usize) -> Scanner<R> {
        let mut buf = Vec::with_capacity(capacity);
        unsafe {
            buf.set_len(capacity);
            inner.initializer().initialize(&mut buf);
        }
        Scanner {
            inner: inner,
            buf: buf,
            pos: 0,
            cap: 0,
            eof: false,
        }
    }

    fn scan_unshifted<'input>(data: &'input [u8], eof: bool) -> (Option<Token<'input>>, usize) {
        (None, 0)
    }

    pub fn scan<'s, 'input: 's>(&'s mut self) -> io::Result<Option<Token<'input>>> /*Option<io::Result<Token>>*/ {
        // Loop until we have a token.
        loop {
            // See if we can get a token with what we already have.
            if self.cap > self.pos || self.eof {
                match Self::scan_unshifted(&self.buf[self.pos..self.cap], self.eof) {
                    (None, 0) => {}
                    (None, amt) => {
                        self.consume(amt);
                        continue;
                    }
                    (tok, amt) => {
                        self.consume(amt);
                        return Ok(tok);
                    }
                }
            }
            // We cannot generate a token with what we are holding.
            // If we've already hit EOF, we are done.
            if self.eof {
                self.pos = 0;
                self.cap = 0;
                return Ok(None);
            }
            self.fill_buf()?;
        }
    }
}

impl<R: Read> Read for Scanner<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let nread = {
            let mut rem = self.fill_buf()?;
            rem.read(buf)?
        };
        self.consume(nread);
        Ok(nread)
    }
}

impl<R: Read> BufRead for Scanner<R> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        // First, shift data to beginning of buffer if there's lots of empty space
        // or space is needed.
        if self.pos > 0 && (self.cap == self.buf.len() || self.pos > self.buf.len() / 2) {
            unsafe {
                use std::ptr;
                ptr::copy(
                    self.buf.as_mut_ptr().offset(self.pos as isize),
                    self.buf.as_mut_ptr(),
                    self.cap - self.pos,
                );
            }
            self.cap -= self.pos;
            self.pos = 0
        }
        // Is the buffer full? If so, resize.
        if self.cap == self.buf.len() {
            let additional = self.buf.capacity();
            self.buf.reserve(additional);
            let cap = self.buf.capacity();
            unsafe {
                self.buf.set_len(cap);
                self.inner
                    .initializer()
                    .initialize(&mut self.buf[self.cap..])
            }
            self.cap -= self.pos;
            self.pos = 0;
        }
        // Finally we can read some input.
        loop {
            match self.inner.read(&mut self.buf[self.cap..]) {
                Ok(0) => {
                    self.eof = true;
                    break;
                }
                Ok(n) => {
                    self.cap += n;
                    break;
                }
                Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(&self.buf[self.pos..self.cap])
    }

    fn consume(&mut self, amt: usize) {
        assert!(self.pos + amt <= self.cap);
        self.pos += amt;
    }
}

#[cfg(test)]
mod test {
    use super::Scanner;
    use std::io::{BufRead, Read};

    #[test]
    fn test_copy() {
        let mut buf: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        unsafe {
            use std::ptr;
            ptr::copy(buf.as_mut_ptr().offset(4), buf.as_mut_ptr(), 6);
        }
        assert_eq!(vec![4, 5, 6, 7, 8, 9, 6, 7, 8, 9], buf);
    }

    #[test]
    fn test_read() {
        let inner: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let mut s = Scanner::new(inner.as_slice());
        let mut buf = [0; 16];
        assert_eq!(10, s.read(&mut buf).unwrap());
        assert_eq!(&buf[0..10], inner.as_slice());
        assert_eq!(0, s.read(&mut buf).unwrap());
        assert!(s.eof);
    }

    #[test]
    fn test_fill_buf() {
        let inner: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let mut s = Scanner::with_capacity(inner.as_slice(), 2);
        assert_eq!(0, s.pos);
        assert_eq!(0, s.cap);
        // first 2 bytes consumed
        let length = {
            let res = s.fill_buf();
            let buf = res.unwrap();
            assert_eq!([0, 1], buf);
            buf.len()
        };
        assert_eq!([0, 1], s.buf.as_ref());
        assert_eq!(0, s.pos);
        assert_eq!(2, s.cap);
        s.consume(length);
        assert_eq!([0, 1], s.buf.as_ref());
        assert_eq!(2, s.pos);
        assert_eq!(2, s.cap);
        // next 2 bytes
        {
            let res = s.fill_buf();
            let buf = res.unwrap();
            assert_eq!([2, 3], buf);
        };
        assert_eq!([2, 3], s.buf.as_ref());
        assert_eq!(0, s.pos);
        assert_eq!(2, s.cap);
        // 4 bytes
        {
            let res = s.fill_buf();
            let buf = res.unwrap();
            assert_eq!([2, 3, 4, 5], buf);
        };
        assert_eq!([2, 3, 4, 5], s.buf.as_ref());
        assert_eq!(0, s.pos);
        assert_eq!(4, s.cap);
        // 6 bytes
        {
            let res = s.fill_buf();
            let buf = res.unwrap();
            assert_eq!([2, 3, 4, 5, 6, 7], buf);
        };
        assert_eq!([2, 3, 4, 5, 6, 7, 0, 0], s.buf.as_ref());
        assert_eq!(0, s.pos);
        assert_eq!(6, s.cap);
        // EOF
        {
            let res = s.fill_buf();
            let buf = res.unwrap();
            assert_eq!([2, 3, 4, 5, 6, 7], buf);
        }
        assert!(s.eof);
    }
}
