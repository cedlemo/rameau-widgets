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
