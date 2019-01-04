open Notty
module Term = Notty_unix.Term

let simpleterm ~imgf ~f ~s =
  let term = Term.create () in
  let imgf (w, h) s =
    I.(string A.(fg lightblack) "[ESC quits.]" <-> imgf (w, h - 1) s) in
  let rec go s =
    Term.image term (imgf (Term.size term) s);
    match Term.event term with
    | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) -> ()
    | `Resize _ -> go s
    | #Unescape.event as e ->
      match f s e with Some s -> go s | _ -> ()
  in go s

open RameauWidgets.Table

let () =
  simpleterm ~imgf ~f ~s
