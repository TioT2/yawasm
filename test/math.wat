(module
  (func $f_inv_sqrt (param $x f32) (result f32)
    (f32.div
      (f32.const 1)
      (f32.sqrt (local.get $x))
    )
    return
  )

  (func $f_sign (param $x f32) (result f32)
    (if (f32.gt (local.get $x) (f32.const 0))
      (then
        f32.const 1
        return
      )
    )
    
    f32.const -1
    return
  )

  (func $vec3f_len2 (param $x f32) (param $y f32) (param $z f32) (result f32)
    (f32.mul
      (local.get $x)
      (local.get $x)
    )

    (f32.mul
      (local.get $y)
      (local.get $y)
    )
    f32.add

    (f32.mul
      (local.get $z)
      (local.get $z)
    )
    f32.add
    return
  )

  (func $vec3f_len (param $x f32) (param $y f32) (param $z f32) (result f32)
    (call $vec3f_len2
      (local.get $x)
      (local.get $y)
      (local.get $z)
    )
    f32.sqrt
    return
  )

  (export "vec3f_len2" (func $vec3f_len2))
  (export "vec3f_len" (func $vec3f_len))
  (export "f_inv_sqrt" (func $f_inv_sqrt))
  (export "f_sign" (func $f_sign))
)
