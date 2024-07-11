extern crate yawasm;

fn main() {
    let instance = yawasm::Module::new(yawasm::Source::WASM(include_bytes!("../test/math.wasm")))
        .expect("Error creating module")
        .create_instance();

    let wasm_vec3f_norm = instance
        .get_function("vec3f_norm")
        .expect("Error getting function")
        .as_callable::<(f32, f32, f32), (f32, f32, f32)>()
        .expect("Error hard-typing function");

    let wasm_f_inv_sqrt = instance
        .get_function("f_inv_sqrt")
        .expect("Error getting function")
        .as_callable::<f32, f32>()
        .unwrap();

    println!("WASM f_inv_sqrt: {}", wasm_f_inv_sqrt(4.0).unwrap());
    println!("WASM vec3f_norm: {:?}", wasm_vec3f_norm((3.0, 4.0, 5.0)).unwrap());
}