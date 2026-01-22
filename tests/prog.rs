use razor::compiler;

fn compile_program(inp_path: &str, out_path: &str, keep_asm: bool) {
    compiler::Compiler::new().compile(inp_path, out_path, keep_asm);
}

#[test]
fn test_programs() {
    let prog_dir = "tests/programs/";
    let out_path = "tests/programs/out";
    let keep_asm = false;

    let programs = std::fs::read_dir(prog_dir).unwrap();
    for entry in programs {
        let entry = entry.unwrap();
        let mut path = entry.path();
        let mut exp_out_path = path.clone();
        exp_out_path.push("output.txt");
        path.push("prog.rz");

        compile_program(path.to_str().unwrap(), out_path, keep_asm);

        let command = std::process::Command::new(out_path)
            .output()
            .expect("Failed to execute compiled program");

        let output = String::from_utf8_lossy(&command.stdout);

        let expected_output = std::fs::read_to_string(exp_out_path).unwrap();

        assert_eq!(
            output,
            expected_output,
            "Output mismatch for program: {:?}",
            entry.path()
        );
    }
}
