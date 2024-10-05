pub struct LabelGen {
    label: &'static str,
    counter: usize,
}

impl LabelGen {
    pub fn new(label: &'static str) -> LabelGen {
        LabelGen {
            label,
            counter: 0,
        }
    }

    pub fn next(&mut self) -> String {
        let label = format!(".{}_{}", self.label, self.counter);
        self.counter += 1;
        label
    }
}
