(** Module used to easyly test all the widgets in bin/simple_term.ml *)

module Notty_crops_example = Notty_crops_example

module Table = struct
  open Table
  let s = (2, 2)
  let f (w, h as s) = function
    | _ -> Some s

  let imgf (ow, oh) (w, h) =
    let data = [
      ["test1";"test22";"test333"];
      ["test4444";"test55555";"test6666666"];
      ["test777777";"test88888888";"test999999999"];
    ] in
    create data
end
