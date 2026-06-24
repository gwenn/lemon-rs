use std::env;
use std::io::Result;
use std::path::Path;
use std::process::Command;

use cc::Build;

fn main() -> Result<()> {
    let out_dir = env::var("OUT_DIR").unwrap();
    let out_path = Path::new(&out_dir);
    let rlemon = out_path.join("rlemon");

    // compile rlemon:
    {
        assert!(Build::new()
            .target(&env::var("HOST").unwrap())
            .get_compiler()
            .to_command()
            .arg("-o")
            .arg(rlemon.clone())
            .arg(Path::new("third_party").join("lemon").join("lemon.c"))
            .status()?
            .success());
    }

    let sql_parser = "src/parser/parse.y";
    // run rlemon / generate parser:
    {
        let mut cmd = Command::new(rlemon);
        cmd.arg("-DSQLITE_ENABLE_UPDATE_DELETE_LIMIT");
        #[cfg(feature = "SQLITE_ENABLE_ORDERED_SET_AGGREGATES")]
        {
            cmd.arg("-DSQLITE_ENABLE_ORDERED_SET_AGGREGATES");
        }
        cmd.arg("-Tthird_party/lemon/lempar.rs")
            .arg(format!("-d{out_dir}"))
            .arg(sql_parser);
        assert!(cmd.status()?.success());
        // TODO ./rlemon -m -Tthird_party/lemon/lempar.rs examples/simple.y
    }

    // compile mkkeywordhash
    let mkkeywordhash = out_path.join("mkkeywordhash");
    {
        assert!(Build::new()
            .target(&env::var("HOST").unwrap())
            //#[cfg(feature = "SQLITE_ENABLE_ORDERED_SET_AGGREGATES")]
            //.define("SQLITE_ENABLE_ORDERED_SET_AGGREGATES", None)
            .get_compiler()
            .to_command()
            .arg("-o")
            .arg(mkkeywordhash.clone())
            .arg(Path::new("third_party").join("mkkeywordhash.c"))
            .status()?
            .success());
    }
    // run mkkeywordhash
    {
        let keywords = out_path.join("keywords.rs");
        let outputs = std::fs::File::create(keywords)?;
        assert!(Command::new(mkkeywordhash)
            .stdout(outputs)
            .status()?
            .success());
    }

    println!("cargo:rerun-if-changed=third_party/lemon/lemon.c");
    println!("cargo:rerun-if-changed=third_party/lemon/lempar.rs");
    println!("cargo:rerun-if-changed=src/parser/parse.y");
    println!("cargo:rerun-if-changed=third_party/mkkeywordhash.c");
    // TODO examples/simple.y if test
    Ok(())
}
