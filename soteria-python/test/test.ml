open Soteria_python_lib
open Pytecode

let phir src =
  match Loader.load_string ~filename:"<test>" src with
  | Ok code -> Phir.of_code code
  | Error e -> failwith ("ERROR: " ^ Error.to_string e)

let%expect_test "test_run" =
  Interp.run @@ phir "x = 1";
  [%expect
    {|
    [DEBUG] exec_instr on {Assign(name:x, 1)}
    [DEBUG] exec_instr on {Return(None)}
    Program executed with result:
      Ok: None_
    |}]

let%expect_test "test_print" =
  Interp.run @@ phir "print(1)";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "Interp.ReprAndStr.py_repr ToDo")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Soteria_python_lib__Interp.map_m in file "soteria-python/lib/interp.ml", line 42, characters 20-26
  Called from Soteria_python_lib__Interp.FrameExecution.call_builtin in file "soteria-python/lib/interp.ml", line 970, characters 20-51
  Called from Soteria_python_lib__Interp.FrameExecution.exec_instr in file "soteria-python/lib/interp.ml", line 834, characters 26-53
  Called from Iter.fold in file "src/Iter.ml", line 77, characters 2-32
  Called from Iter.to_list in file "src/Iter.ml", line 812, characters 27-60
  Called from Stdlib__Fun.protect in file "fun.ml", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file "fun.ml", line 39, characters 6-52
  Called from Soteria__Stats.As_ctx.add_time_of_to in file "soteria/lib/stats/stats.ml", line 224, characters 14-18
  Called from Soteria_python_lib__Interp.run in file "soteria-python/lib/interp.ml", line 1043, characters 16-52
  Called from Test_soteria_python__Test.(fun) in file "soteria-python/test/test.ml", line 20, characters 2-31
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28

  Trailing output
  ---------------
  [DEBUG] exec_instr on {Call(name:print, null, [1])}
  |}]

let%expect_test "test_randint_1" =
  Interp.run @@ phir {|
x = 1 / Soteria_randint()
|};
  [%expect
    {|
    [DEBUG] exec_instr on {Push(1)}
    [DEBUG] exec_instr on {Call(name:Soteria_randint, null, [])}
    [DEBUG] exec_instr on {Binary_op(/, stack, stack)}
    [DEBUG] exec_instr on {Assign(name:x, stack)}
    [DEBUG] exec_instr on {Return(None)}
    Program executed with result:
      Error: ZeroException
        (0 == V|1|)
      Ok: None_
        (0 != V|1|)
    |}]

let%expect_test "test_randint_2" =
  Interp.run @@ phir {|
x = Soteria_randint()
y = 0
y = 1/x
z = 1/(x+1)
|};
  [%expect
    {|
    [DEBUG] exec_instr on {Call(name:Soteria_randint, null, [])}
    [DEBUG] exec_instr on {Assign(name:x, stack)}
    [DEBUG] exec_instr on {Assign(name:y, 0)}
    [DEBUG] exec_instr on {Binary_op(/, 1, name:x)}
    [DEBUG] exec_instr on {Assign(name:y, stack)}
    [DEBUG] exec_instr on {Push(1)}
    [DEBUG] exec_instr on {Binary_op(+, name:x, 1)}
    [DEBUG] exec_instr on {Binary_op(/, stack, stack)}
    [DEBUG] exec_instr on {Assign(name:z, stack)}
    [DEBUG] exec_instr on {Return(None)}
    Program executed with result:
      Error: ZeroException
        (0 == V|1|)
      Error: ZeroException
        (0 != V|1|)
        (V|1| == -1)
      Ok: None_
        (0 != V|1|)
        (V|1| != -1)
    |}]

let%expect_test "test_randint_3" =
  Interp.run
  @@ phir
       {|
v1 = Soteria_randint()
v2 = Soteria_randint()
if Soteria_randint():
  x = 1/v1
else:
  x = 2/v2
|};
  [%expect
    {|
    [DEBUG] exec_instr on {Call(name:Soteria_randint, null, [])}
    [DEBUG] exec_instr on {Assign(name:v1, stack)}
    [DEBUG] exec_instr on {Call(name:Soteria_randint, null, [])}
    [DEBUG] exec_instr on {Assign(name:v2, stack)}
    [DEBUG] exec_instr on {Call(name:Soteria_randint, null, [])}
    [DEBUG] exec_instr on {Unary(to_bool, stack)}
    [DEBUG] Unary (to_bool, V|3|): (V|3| != 0)
    [DEBUG] exec_instr on {Cond_jump(if_false, stack, 10)}
    [DEBUG] Evaluated guard (V|3| != 0) -> !((V|3| != 0))
    [DEBUG] exec_instr on {Binary_op(/, 2, name:v2)}
    [DEBUG] exec_instr on {Assign(name:x, stack)}
    [DEBUG] exec_instr on {Return(None)}
    [DEBUG] exec_instr on {Binary_op(/, 1, name:v1)}
    [DEBUG] exec_instr on {Assign(name:x, stack)}
    [DEBUG] exec_instr on {Return(None)}
    Program executed with result:
      Error: ZeroException
        !((V|3| != 0))
        (0 == V|2|)
      Ok: None_
        !((V|3| != 0))
        (0 != V|2|)
      Error: ZeroException
        (V|3| != 0)
        (0 == V|1|)
      Ok: None_
        (V|3| != 0)
        (0 != V|1|)
    |}]

let%expect_test "test_abs_val_with_error" =
  Interp.run
  @@ phir {|
x = Soteria_randint()
if x < 0:
  x = -x
assert (x > 0)
|};
  [%expect
    {|
    [DEBUG] exec_instr on {Call(name:Soteria_randint, null, [])}
    [DEBUG] exec_instr on {Assign(name:x, stack)}
    [DEBUG] exec_instr on {Compare(< as bool, name:x, 0)}
    [DEBUG] exec_instr on {Cond_jump(if_false, stack, 6)}
    [DEBUG] Evaluated guard (V|1| < 0) -> (0 <= V|1|)
    [DEBUG] exec_instr on {Compare(> as bool, name:x, 0)}
    [DEBUG] exec_instr on {Cond_jump(if_true, stack, 10)}
    [DEBUG] Evaluated guard (0 < V|1|) -> (0 < V|1|)
    [DEBUG] exec_instr on {Return(None)}
    [DEBUG] exec_instr on {Load_assertion_error}
    [DEBUG] exec_instr on {Raise(stack)}
    [DEBUG] exec_instr on {Unary(neg, name:x)}
    [DEBUG] Unary (neg, V|1|): (0 - V|1|)
    [DEBUG] exec_instr on {Assign(name:x, stack)}
    [DEBUG] exec_instr on {Compare(> as bool, name:x, 0)}
    [DEBUG] exec_instr on {Cond_jump(if_true, stack, 10)}
    [DEBUG] Evaluated guard (V|1| < 0) -> (V|1| < 0)
    [DEBUG] exec_instr on {Return(None)}
    Program executed with result:
      Ok: None_
        (0 <= V|1|)
        (0 < V|1|)
      Error: Ref(189)
        (0 <= V|1|)
        (V|1| <= 0)
      Ok: None_
        (V|1| < 0)
    |}]

let%expect_test "test_branch" =
  Interp.run @@ phir {|
x = Soteria_randint()
y = x if x > 0 else 1/x
z = 1/y
|};
  [%expect
    {|
    [DEBUG] exec_instr on {Call(name:Soteria_randint, null, [])}
    [DEBUG] exec_instr on {Assign(name:x, stack)}
    [DEBUG] exec_instr on {Compare(> as bool, name:x, 0)}
    [DEBUG] exec_instr on {Cond_jump(if_false, stack, 6)}
    [DEBUG] Evaluated guard (0 < V|1|) -> (V|1| <= 0)
    [DEBUG] exec_instr on {Binary_op(/, 1, name:x)}
    [DEBUG] exec_instr on {Assign(name:y, stack)}
    [DEBUG] exec_instr on {Binary_op(/, 1, name:y)}
    [DEBUG] exec_instr on {Assign(name:z, stack)}
    [DEBUG] exec_instr on {Return(None)}
    [DEBUG] exec_instr on {Push(name:x)}
    [DEBUG] exec_instr on {Jump(7)}
    [DEBUG] exec_instr on {Assign(name:y, stack)}
    [DEBUG] exec_instr on {Binary_op(/, 1, name:y)}
    [DEBUG] exec_instr on {Assign(name:z, stack)}
    [DEBUG] exec_instr on {Return(None)}
    Program executed with result:
      Error: ZeroException
        (V|1| <= 0)
        (0 == V|1|)
      Error: ZeroException
        (V|1| <= 0)
        (0 != V|1|)
        (0 == (1 / V|1|))
      Ok: None_
        (V|1| <= 0)
        (0 != V|1|)
        (0 != (1 / V|1|))
      Ok: None_
        (0 < V|1|)
        (0 != V|1|)
    |}]
