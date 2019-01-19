(** Module used to easyly test all the widgets in bin/simple_term.ml *)

module Notty_crops_example = Notty_crops_example

module Table = struct
  open Table
  let s = (2, 2)
  let f (w, h as s) = function
    | _ -> Some s

  let imgf (w, h) =
    let data = [
      Utils.list_of_random_strings 5 15;
      Utils.list_of_random_strings 5 15;
      Utils.list_of_random_strings 5 15;
      Utils.list_of_random_strings 5 15;
      Utils.list_of_random_strings 5 15;
      Utils.list_of_random_strings 5 15;
      Utils.list_of_random_strings 5 15;
    ]
    in
    create ~align:`Right ~size:(w, h) data
end
