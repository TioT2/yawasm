
// pub struct TypedFunction<Args: WasmValueType, Rets: WasmValueType> {
//     func: Box<dyn Fn(&[types::Value]) -> Vec<types::Value>>,
//     _phantom_data: PhantomData<fn(Args) -> Rets>,
// }
//
// impl<Args: WasmValueType, Rets: WasmValueType> TypedFunction<Args, Rets> {
//     pub fn call(&self, args: Args) -> Option<Rets> {
//         Rets::try_from_values(&(self.func)(&args.into_values()))
//     }
// }

fn main() {
    let module = yawasm::Module::new(yawasm::Source::WASM(include_bytes!("../test/math.wasm"))).unwrap();
    let mut instance = module.create_instance();

    let mut wasm_vec3f_norm = |x: f32, y: f32, z: f32| -> (f32, f32, f32) {
        let rs = instance.call("vec3f_norm", &[x.into(), y.into(), z.into()]).expect("No return values");

        if rs.len() != 3 {
            panic!("Unexpected number of values returned");
        }

        (
            rs[0].as_f32().expect("First return must be f32"),
            rs[1].as_f32().expect("Second return must be f32"),
            rs[2].as_f32().expect("Third return must be f32"),
        )
    };

    println!("{:?}", wasm_vec3f_norm(3.0, 4.0, 5.0));
} // fn main

// file main.rs
