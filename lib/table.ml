open Notty

let maxby f xs = List.(fold_left max 0 (map f xs))

(* column : align:[`Left|`Middle|`Right] -> image list -> image *)
let column ~align images =
  let width = maxby I.width images in
  List.map (I.hsnap ~align width) images |> I.vcat

let hdistribute ?align w imgs =
  let n = List.length imgs in
  I.(List.map (hsnap ?align (w / n)) imgs |> hcat)

(** Generate a table from a list of list of strings
 * @elements string list list
 * *)
let create elements =
  (* calcul width for each column for each row and get the max for each column *)
  let a = A.(fg lightmagenta) in
  let imgs = List.map (fun x -> List.map I.(string a) x) elements in
  let max_col_widths = Array.make (List.length imgs) 0 in
  let () = List.iter (fun row ->
      List.iteri (fun i col ->
          let prev = Array.get max_col_widths i in
          Array.set max_col_widths i (max prev I.(width col))
        ) row
    ) imgs (** Compute max col width *)
  in
  List.map (fun row ->
      List.mapi (fun i col ->
          I.hsnap (Array.get max_col_widths i) col
        ) row |> I.hcat
    ) imgs |> I.vcat

let s = (2, 2)
let f (w, h as s) = function
    `Key (`Arrow `Left, _) -> Some (w - 1, h)
  | `Key (`Arrow `Right, _) -> Some (w + 1, h)
  | `Key (`Arrow `Up, _) -> Some (w, h - 1)
  | `Key (`Arrow `Down, _) -> Some (w, h + 1)
  | `Key (`ASCII '0', _) -> Some (0, 0)
  | _ -> Some s

let imgf (ow, oh) (w, h) =
  let data = [
    ["test1";"test22";"test333"];
    ["test4444";"test55555";"test6666666"];
    ["test777777";"test88888888";"test999999999"];
  ] in
  create data
