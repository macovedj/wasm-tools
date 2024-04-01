use super::*;
use anyhow::{bail, Result};

impl Module {
    /// Ensure that all of this Wasm module's functions will terminate when
    /// executed.
    ///
    /// This adds a new mutable, exported global to the module to keep track of
    /// how much "fuel" is left. Fuel is decremented at the head of each loop
    /// and function. When fuel reaches zero, a trap is raised.
    ///
    /// The index of the fuel global is returned, so that you may control how
    /// much fuel the module is given.
    ///
    /// # Errors
    ///
    /// Returns an error if any function body was generated with
    /// possibly-invalid bytes rather than being generated by wasm-smith. In
    /// such a situation this pass does not parse the input bytes and inject
    /// instructions, instead it returns an error.
    pub fn ensure_termination(&mut self, default_fuel: u32) -> Result<u32> {
        let fuel_global = self.globals.len() as u32;
        self.globals.push(GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        });
        self.defined_globals
            .push((fuel_global, ConstExpr::i32_const(default_fuel as i32)));

        for code in &mut self.code {
            let check_fuel = |insts: &mut Vec<Instruction>| {
                // if fuel == 0 { trap }
                insts.push(Instruction::GlobalGet(fuel_global));
                insts.push(Instruction::I32Eqz);
                insts.push(Instruction::If(BlockType::Empty));
                insts.push(Instruction::Unreachable);
                insts.push(Instruction::End);

                // fuel -= 1
                insts.push(Instruction::GlobalGet(fuel_global));
                insts.push(Instruction::I32Const(1));
                insts.push(Instruction::I32Sub);
                insts.push(Instruction::GlobalSet(fuel_global));
            };

            let instrs = match &mut code.instructions {
                Instructions::Generated(list) => list,
                Instructions::Arbitrary(_) => {
                    bail!(
                        "failed to ensure that a function generated due to it \
                         containing arbitrary instructions"
                    )
                }
            };
            let mut new_insts = Vec::with_capacity(instrs.len() * 2);

            // Check fuel at the start of functions to deal with infinite
            // recursion.
            check_fuel(&mut new_insts);

            for inst in mem::replace(instrs, vec![]) {
                let is_loop = matches!(&inst, Instruction::Loop(_));
                new_insts.push(inst);

                // Check fuel at loop heads to deal with infinite loops.
                if is_loop {
                    check_fuel(&mut new_insts);
                }
            }

            *instrs = new_insts;
        }

        Ok(fuel_global)
    }
}
