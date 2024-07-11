;; Simple math library

(module
  ;; Fast inverse square root calculation function
  ;; ARGUMENTS:
  ;;   $x f32 - number to calculate fast inverse square root of
  ;; RETURNS:
  ;;   (f32) Fast inverse square root with one newtonian iteration of $x parameter
  (func $f_inv_sqrt (export "f_inv_sqrt") (param $x f32) (result f32)
    (local $v f32)
    
    i32.const 0x5F3759DF
    local.get $x
    i32.reinterpret_f32
    i32.const 1
    i32.shr_u
    i32.sub
    f32.reinterpret_i32
    local.set $v

    ;; Multiply $x by 0.5
    local.get $x
    f32.const 0.5
    f32.mul
    local.set $x

    ;; Newtonian iteration
    f32.const 1.5
    local.get $v
    local.get $v
    f32.mul
    local.get $x
    f32.mul
    f32.sub
    local.get $v
    f32.mul
    local.set $v

    local.get $v
    return
  ) ;; func $f_inv_sqrt

  ;; 3-component vector length square calculation function
  ;; ARGUMENTS:
  ;;   $x $y $z f32 - vector components
  ;; RETURNS:
  ;;   (f32) <$x, $y, $z> vector length squared length
  (func $vec3f_len2 (export "vec3f_len2") (param $x f32) (param $y f32) (param $z f32) (result f32)
    (f32.mul (local.get $x) (local.get $x))
    (f32.mul (local.get $y) (local.get $y))
    (f32.mul (local.get $z) (local.get $z))
    f32.add
    f32.add

    return
  ) ;; func $vec3f_len2

  ;; 3-component vector normalization function
  ;; ARGUMENTS:
  ;;   $x $y $z f32 - vector components
  ;; RETURNS:
  ;;   (f32 f32 f32) Vector with direction similar with <$x, $y, $z> but with unit length
  (func $vec3f_norm (export "vec3f_norm") (param $x f32) (param $y f32) (param $z f32) (result f32 f32 f32)
    (local $inv_len f32)

    (call $vec3f_len2
      (local.get $x)
      (local.get $y)
      (local.get $z)
    )
    call $f_inv_sqrt
    local.set $inv_len

    (f32.mul (local.get $x) (local.get $inv_len))
    (f32.mul (local.get $y) (local.get $inv_len))
    (f32.mul (local.get $z) (local.get $inv_len))

    return
  ) ;; func $vec3f_norm)
) ;; module

;; file math.wat
