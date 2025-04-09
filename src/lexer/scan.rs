//! Adaptation/port of [Go scanner](http://tip.golang.org/pkg/bufio/#Scanner).

use log::debug;

use std::error::Error;
use std::fmt;
use std::io;

/// Position
#[derive(Debug)]
pub struct Pos {
    /// line number
    pub line: usize,
    /// column number (byte offset, not char offset)
    pub column: usize,
}

impl Pos {
    pub fn from(input: &[u8], offset: usize) -> Self {
        let (mut line, mut column) = (1, 1);
        for byte in &input[..offset] {
            if *byte == b'\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        Self { line, column }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line: {}, column: {}", self.line, self.column)
    }
}

/// Error with position
pub trait ScanError: Error + From<io::Error> + Sized {
    /// Update the position where the error occurs
    fn position(&mut self, p: Pos);
}

/// The `(&[u8], TokenType)` is the token.
/// And the `usize` is the amount of bytes to consume.
type SplitResult<'input, TokenType, Error> =
    Result<(Option<(&'input [u8], TokenType)>, usize), Error>;

/// Split function used to tokenize the input
pub trait Splitter: Sized {
    /// Potential error raised
    type Error: ScanError;
    //type Item: ?Sized;
    /// Token generated
    type TokenType: std::fmt::Debug;

    /// The arguments are an initial substring of the remaining unprocessed
    /// data.
    ///
    /// If the returned error is non-nil, scanning stops and the error
    /// is returned to the client.
    ///
    /// The function is never called with an empty data slice.
    fn split<'input>(
        &mut self,
        data: &'input [u8],
    ) -> SplitResult<'input, Self::TokenType, Self::Error>;
}

/// Like a `BufReader` but with a growable buffer.
/// Successive calls to the `scan` method will step through the 'tokens'
/// of a file, skipping the bytes between the tokens.
///
/// Scanning stops unrecoverably at EOF, the first I/O error, or a token too
/// large to fit in the buffer. When a scan stops, the reader may have
/// advanced arbitrarily far past the last token.
pub struct Scanner<S: Splitter> {
    /// offset in `input`
    offset: usize,
    /// mark
    mark: usize,
    /// The function to tokenize the input.
    splitter: S,
}

impl<S: Splitter> Scanner<S> {
    /// Constructor
    pub fn new(splitter: S) -> Self {
        Self {
            offset: 0,
            mark: 0,
            splitter,
        }
    }

    /// Current position
    pub fn position(&self, input: &[u8]) -> Pos {
        Pos::from(input, self.offset)
    }

    /// Associated splitter
    pub fn splitter(&self) -> &S {
        &self.splitter
    }
    /// Mark current position
    pub fn mark(&mut self) {
        self.mark = self.offset;
    }
    /// Reset to mark
    pub fn reset_to_mark(&mut self) {
        self.offset = self.mark;
    }

    /// Reset the scanner such that it behaves as if it had never been used.
    pub fn reset(&mut self) {
        self.offset = 0;
    }
}

type ScanResult<'input, TokenType, Error> =
    Result<(usize, Option<(&'input [u8], TokenType)>, usize), Error>;

impl<S: Splitter> Scanner<S> {
    /// Advance the Scanner to next token.
    /// Return the token as a byte slice.
    /// Return `None` when the end of the input is reached.
    /// Return any error that occurs while reading the input.
    pub fn scan<'input>(
        &mut self,
        input: &'input [u8],
    ) -> ScanResult<'input, S::TokenType, S::Error> {
        debug!(target: "scanner", "scan({})", Pos::from(input, self.offset));
        // Loop until we have a token.
        loop {
            // See if we can get a token with what we already have.
            if self.offset < input.len() {
                let data = &input[self.offset..];
                match self.splitter.split(data) {
                    Err(mut e) => {
                        e.position(Pos::from(input, self.offset));
                        return Err(e);
                    }
                    Ok((None, 0)) => {
                        // Done
                    }
                    Ok((None, amt)) => {
                        // Ignore/skip this data
                        self.consume(data, amt);
                        continue;
                    }
                    Ok((tok, amt)) => {
                        let start = self.offset;
                        self.consume(data, amt);
                        debug!(target: "scanner", "scan(start: {}, tok: {:?}, offset: {})", start, tok, self.offset);
                        return Ok((start, tok, self.offset));
                    }
                }
            }
            // We cannot generate a token with what we are holding.
            // we are done.
            return Ok((self.offset, None, self.offset));
        }
    }

    /// Consume `amt` bytes of the buffer.
    fn consume(&mut self, data: &[u8], amt: usize) {
        debug!(target: "scanner", "consume({})", amt);
        debug_assert!(amt <= data.len());
        self.offset += amt;
    }
}

impl<S: Splitter> fmt::Debug for Scanner<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scanner")
            .field("offset", &self.offset)
            .field("mark", &self.mark)
            .finish()
    }
}
