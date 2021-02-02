use std::env;
use std::io::Result;
use std::path::Path;
use std::process::Command;

use cc::Build;

fn main() -> Result<()> {
    let out_dir = env::var("OUT_DIR").unwrap();
    let out_path = Path::new(&out_dir);
    let rlemon = out_path.join("rlemon");

    let lemon_src_dir = Path::new("third_party").join("lemon");
    let rlemon_src = lemon_src_dir.join("lemon.c");

    // compile rlemon:
    {
        assert!(Build::new()
            .get_compiler()
            .to_command()
            .arg("-o")
            .arg(rlemon.clone())
            .arg(rlemon_src)
            .status()?
            .success());
    }

    let sql_parser = Path::new("src").join("parser").join("parse.y");
    // run rlemon / generate parser:
    {
        // TODO -d<string>   Output directory.  Default '.'
        assert!(Command::new(rlemon)
            .arg("-DSQLITE_ENABLE_UPDATE_DELETE_LIMIT")
            .arg("-Tthird_party/lemon/lempar.rs")
            .arg(format!("-d{}", out_dir))
            .arg(sql_parser)
            .status()?
            .success());
        // TODO ./rlemon -m -Tthird_party/lemon/lempar.rs examples/simple.y
    }

    println!("cargo:rerun-if-changed=third_party/lemon/lemon.c");
    println!("cargo:rerun-if-changed=third_party/lemon/lempar.rs");
    println!("cargo:rerun-if-changed=src/parser/parse.y");
    // TODO examples/simple.y if test
    Ok(())
}
