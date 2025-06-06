fn pos_from_index(source: &str, index: usize) -> Option<(usize, usize)> {
    let mut line = 0;
    let mut col = 0;
    let mut current = 0;

    for c in source.chars() {
        if current == index {
            return Some((line, col));
        }

        if c == '\n' {
            col = 0;
            line += 1;
        } else {
            col += 1;
        }

        current += 1;
    }

    None
}

fn get_line(source: &str, line: usize) -> Option<String> {
    let lines: Vec<String> = source.lines().map(|s| s.to_string()).collect();
    lines.get(line).map(|s| s.clone())
}

pub fn print_error_at(source: &str, index: usize, error: &str) {
    if let Some((line, col)) = pos_from_index(&source, index) {
        if let Some(line_text) = get_line(&source, line) {
            println!("\n");

            // TODO: automatically determine how much context we wanna show
            if let Some(diff) = line.checked_sub(2) {
                if let Some(prev) = get_line(&source, diff) {
                    println!("\t{} |{prev}", line - 2);
                }
            }

            if let Some(diff) = line.checked_sub(1) {
                if let Some(prev) = get_line(&source, diff) {
                    println!("\t{} |{prev}", line - 1);
                }
            }

            println!("\t{} |{line_text}", line);
            println!(
                "\t{}^--- {error}",
                " ".repeat(format!("{} |", line).len() + col)
            );

            if let Some(diff) = line.checked_add(1) {
                if let Some(prev) = get_line(&source, diff) {
                    println!("\t{} |{prev}", line + 1);
                }
            }

            if let Some(diff) = line.checked_add(2) {
                if let Some(prev) = get_line(&source, diff) {
                    println!("\t{} |{prev}", line + 2);
                }
            }

            println!("");
            return;
        }

        panic!("could not find line: {line} col: {col}");
    }

    panic!("could not find index: {index}");
}
