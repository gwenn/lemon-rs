#![feature(read_initializer)]
#![feature(plugin)]
#![plugin(phf_macros)]

extern crate phf;

pub mod error;
pub mod scan;
pub mod token;
