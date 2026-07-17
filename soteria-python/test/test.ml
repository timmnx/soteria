open Soteria_python_lib

open Pytecode

let phir src =
  match Loader.load_string ~filename:"<test>" src with
  | Ok code -> Phir.of_code code
  | Error e -> failwith ("ERROR: " ^ Error.to_string e)

let prog1 = "x = 1"

let%expect_test "show1" =
  Interp.run @@ phir prog1;
  [%expect {| (fun x -> fun y -> x) (1) |}]
;;
