const fs = require('node:fs');
let buf = fs.readFileSync('math.wasm');
let instance = WebAssembly.instantiate(buf).then(module => {
  console.log('exports: ', module.instance.exports)
  console.log(module.instance.exports.vec3f_norm(3.0, 4.0, 5.0))
});
