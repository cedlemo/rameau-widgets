module Notty_crops_example = struct
  open Notty_crops_example

end

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
