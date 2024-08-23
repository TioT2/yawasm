extern crate yawasm;

fn main() {
    let instance = yawasm::Module::new(yawasm::Source::WASM(include_bytes!("../test/math.wasm")))
        .expect("Error creating module")
        .create_instance()
        .expect("Error during module instantiation occured.");

    let wasm_vec3f_norm = instance
        .get_function("vec3f_norm")
        .expect("Error getting function")
        .as_typed_callable::<(f32, f32, f32), (f32, f32, f32)>()
        .expect("Error hard-typing function");

    let wasm_f_inv_sqrt = instance
        .get_function("f_inv_sqrt")
        .expect("Error getting function")
        .as_typed_callable::<f32, f32>()
        .expect("Error hard-typing function");

    let i = wasm_f_inv_sqrt(4.0)
        .expect("Error during fast inverse square root computation occured");
    println!("WASM f_inv_sqrt: {}", i);

    let n = wasm_vec3f_norm((3.0, 4.0, 5.0))
        .expect("Error during vector normalization occured");
    println!("WASM vec3f_norm: {:?}", n);
}
