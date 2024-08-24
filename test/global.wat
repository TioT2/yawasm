;; Global variable test

(module
  (global $counter (mut i32)
    i32.const 0
  ) ;; global $counter

  ;; counter value getting function
  ;; ARGUMENTS: None
  ;; RETURNS:
  ;;   (i32) Counter current value
  (func $get_counter (export "get_counter") (result i32)
    global.get $counter
    return
  ) ;; func $get_c4ounter

  ;; counter value increment function
  ;; ARGUMENTS: None
  ;; RETURNS: None
  (func $inc_counter (export "inc_counter")
    global.get $counter
    i32.const 1
    i32.add
    global.set $counter
    return
  ) ;; func $inc_counter
) ;; module

;; file global.wat