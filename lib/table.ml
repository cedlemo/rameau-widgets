open Notty

(** Generate a table from a list of list of strings
 * @elements string list list
 * *)

(** Calcul the set of bigger width for each column of a Notty.image list list *)
let compute_max_widths imgs =
  let n = List.hd imgs |> List.length in
  let max_col_widths = Array.make n 0 in
  let () = List.iter (fun row ->
      List.iteri (fun i col ->
          let prev = Array.get max_col_widths i in
          Array.set max_col_widths i (max prev I.(width col))
        ) row
    ) imgs (** Compute max col width *)
  in max_col_widths

let create ?size elements =
  let a = A.(fg lightmagenta) in
  let imgs = List.map (fun x -> List.map I.(string a) x) elements in
  let col_widths = begin match size with
    | None ->
      compute_max_widths imgs (** Compute max col width *)
    | Some (w, h) ->
      let n = List.hd imgs |> List.length in
      let w' = w / n in
      if n <= w then Array.make n w'
      else compute_max_widths imgs
  end
  in
  List.map (fun row ->
      List.mapi (fun i col ->
          I.hsnap (Array.get col_widths i) col
        ) row |> I.hcat
    ) imgs |> I.vcat
