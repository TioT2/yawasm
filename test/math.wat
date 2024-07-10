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

  (func $f_is_zero (param $x f32) (result i32)
    (if (f32.eq (local.get $x) (f32.const 0))
      (then
        i32.const 1
        return
      )
      (else
        i32.const 0
        return
      )
    )
    i32.const 0xFFFFFFFF
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

  (func $vec3f_norm (param $x f32) (param $y f32) (param $z f32) (result f32 f32 f32)
    (local $inv_len f32)
    (call $vec3f_len2
      (local.get $x)
      (local.get $y)
      (local.get $z)
    )
    call $f_inv_sqrt
    local.set $inv_len

    local.get $x
    local.get $inv_len
    f32.mul

    local.get $y
    local.get $inv_len
    f32.mul

    local.get $z
    local.get $inv_len
    f32.mul

    return
)

  (export "vec3f_len2" (func $vec3f_len2))
  (export "vec3f_len" (func $vec3f_len))
  (export "f_inv_sqrt" (func $f_inv_sqrt))
  (export "f_sign" (func $f_sign))
  (export "f_is_zero" (func $f_is_zero))
  (export "vec3f_norm" (func $vec3f_norm))
)
