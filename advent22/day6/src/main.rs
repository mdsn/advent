use ascii::{AsciiChar, AsciiString};
use ascii_read::AsciiBufRead;
use std::collections::HashSet;
use std::error::Error;
use std::io;

fn main() -> Result<(), Box<dyn Error>> {
    let len = 14;
    let mut line = AsciiString::new();
    let _ = io::stdin().lock().read_ascii_line(&mut line);

    for (i, w) in line.as_slice().windows(len).enumerate() {
        let set: HashSet<&AsciiChar> = HashSet::from_iter(w.iter());
        if set.len() == len {
            println!("{}", i + len);
            break;
        }
    }

    Ok(())
}
