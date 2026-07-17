open Soteria_python_lib

open Pytecode

let phir src =
  match Loader.load_string ~filename:"<test>" src with
  | Ok code -> Phir.of_code code
  | Error e -> failwith ("ERROR: " ^ Error.to_string e)


let%expect_test "test_run" =
  Interp.run @@ phir "x = 1";
  [%expect {|
    Test Program executed with result:
      Ok: None_
    |}]
;;

let%expect_test "test_call" =
  Interp.run @@ phir "print(1)";
  [%expect {|
    Test Program executed with result:
      Ok: None_
    |}]
;;
