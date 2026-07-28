open Soteria_python_lib
open Pytecode

let phir src =
  match Loader.load_string ~filename:"<test>" src with
  | Ok code -> Phir.of_code code
  | Error e -> failwith ("ERROR: " ^ Error.to_string e)

let%expect_test "test_run" =
  Interp.run @@ phir "x = 1";
  [%expect {|
    Program executed with result:
      Ok: None_
    |}]

let%expect_test "test_call" =
  Interp.run @@ phir "print(1)";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "ToDo Interp.FrameExecution.call_builtin for: print")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Soteria_python_lib__Interp.FrameExecution.exec_instr in file "soteria-python/lib/interp.ml", line 764, characters 26-53
  Called from Iter.fold in file "src/Iter.ml", line 77, characters 2-32
  Called from Iter.to_list in file "src/Iter.ml", line 812, characters 27-60
  Called from Stdlib__Fun.protect in file "fun.ml", line 34, characters 8-15
  Re-raised at Stdlib__Fun.protect in file "fun.ml", line 39, characters 6-52
  Called from Soteria__Stats.As_ctx.add_time_of_to in file "soteria/lib/stats/stats.ml", line 224, characters 14-18
  Called from Soteria_python_lib__Interp.run in file "soteria-python/lib/interp.ml", line 937, characters 16-52
  Called from Test_soteria_python__Test.(fun) in file "soteria-python/test/test.ml", line 17, characters 2-31
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

let%expect_test "test_randint" =
  Interp.run @@ phir {|
x = 1 / Soteria_randint()
|};
  [%expect
    {|
    Program executed with result:
      Error: ZeroException
      (0 == V|1|)
      Ok: None_

      (0 != V|1|)
    |}]
let%expect_test "test_randint" =
  Interp.run @@ phir {|
x = Soteria_randint()
y = 0
y = 1/x
z = 1/(x+1)
|};
  [%expect
    {|
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
let%expect_test "test_randint" =
  Interp.run @@ phir {|
v1 = Soteria_randint()
v2 = Soteria_randint()
if Soteria_randint():
  x = 1
else:
  x = 2/v2
|};
  [%expect {|
    Program executed with result:
      Error: ZeroException
      (0 == V|1|)
      Ok: None_

      (0 != V|1|)
    |}]

let%expect_test "test_randint" =
  Interp.run
  @@ phir
       {|
x = Soteria_randint()
y = 0
if x > 0:
  y = x
else:
  y = -x
assert (y >= 0)
|};
  [%expect
    {|
    Program executed with result:
      Ok: None_
    |}]
let%expect_test "test_branch" =
  Interp.run
  @@ phir
       {|
x = Soteria_randint()
y = x if x > 0 else 1/x
assert (y > 0)
|};
  [%expect
    {|
    Program executed with result:
      Ok: None_
    |}]
