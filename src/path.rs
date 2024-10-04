use std::fmt::Display;

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct UsePath {
    path: Vec<String>,
}

impl UsePath {
    pub fn new() -> UsePath {
        UsePath { path: Vec::new() }
    }

    pub fn from_path(path: &str) -> UsePath {
        UsePath {
            path: path
                .strip_suffix(".rz")
                .unwrap_or(path)
                .split("/")
                .map(|s| s.to_string())
                .collect(),
        }
    }

    pub fn from_vec(vec: Vec<String>) -> UsePath {
        UsePath { path: vec }
    }

    pub fn matches(&self, other: &UsePath) -> bool {
        self.path
            .iter()
            .rev()
            .zip(other.path.iter().rev())
            .all(|(a, b)| a == b)
    }

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }

    pub fn add(&mut self, path: &str) {
        self.path.push(path.to_string());
    }

    pub fn to_path(&self) -> String {
        let mut path = self.path.join("/");
        path.push_str(".rz");
        path
    }
}

impl Display for UsePath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.path.join("."))
    }
}
