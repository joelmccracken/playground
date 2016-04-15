use std::process::Command;

fn main() {

    let output = Command::new("/usr/local/bin/brew")
        .arg("leaves")
        .output()
        .unwrap_or_else(|e| {panic!("filed to execute process: {}", e)});


    println!("output is: {}", String::from_utf8(output.stdout).unwrap());
}
