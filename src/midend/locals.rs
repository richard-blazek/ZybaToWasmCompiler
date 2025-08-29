use std::collections::HashMap;

use crate::midend::ir::Type;

pub struct Locals {
    ids: HashMap<String, usize>,
    types: Vec<Type>
}

impl Locals {
    pub fn new() -> Locals {
        Locals { ids: HashMap::new(), types: vec![] }
    }

    pub fn add(&mut self, name: String, tpe: Type) -> usize {
        self.types.push(tpe);
        self.ids.insert(name, self.types.len() - 1);
        self.types.len() - 1
    }

    pub fn contains(&self, name: &str) -> bool {
        self.ids.contains_key(name)
    }

    pub fn get_id(&self, name: &str) -> usize {
        self.ids[name]
    }

    pub fn get_type_by_id(&self, id: usize) -> Type {
        self.types[id].clone()
    }

    pub fn get_type(&self, name: &str) -> Type {
        self.get_type_by_id(self.ids[name])
    }

    pub fn get_all(self) -> Vec<Type> {
        self.types
    }
}
