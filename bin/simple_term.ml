open Notty
module Term = Notty_unix.Term

let simpleterm ~imgf ~f =
  let term = Term.create () in
  let imgf (w, h) =
    I.(string A.(fg lightblack) (Printf.sprintf "(%d,%d)" w h) <-> (
    I.(string A.(fg lightblack) "[ESC quits.]" <-> imgf (w, h - 1)))) in
  let rec go () =
    Term.image term (imgf (Term.size term));
    match Term.event term with
    | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) -> ()
    | `Resize _ -> go ()
    | #Unescape.event as e ->
      match f (Term.size term) e with Some s -> go () | _ -> ()
  in go ()

open RameauWidgets.Test.Table
(* open RameauWidgets.Test.Notty_crops_example *)

let () =
  simpleterm ~imgf ~f
