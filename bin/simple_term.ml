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

let s = (2, 2)
let f (w, h as s) = function
    `Key (`Arrow `Left, _) -> Some (w - 1, h)
  | `Key (`Arrow `Right, _) -> Some (w + 1, h)
  | `Key (`Arrow `Up, _) -> Some (w, h - 1)
  | `Key (`Arrow `Down, _) -> Some (w, h + 1)
  | `Key (`ASCII '0', _) -> Some (0, 0)
  | _ -> Some s

let hdistribute ?align w imgs =
  let n = List.length imgs in
  I.(List.map (hsnap ?align (w / n)) imgs |> hcat)

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let outline attr i =
    let (w, h) = I.(width i, height i) in
    let chr x = I.uchar attr (Uchar.of_int x) 1 1
    and hbar  = I.uchar attr (Uchar.of_int 0x2500) w 1
    and vbar  = I.uchar attr (Uchar.of_int 0x2502) 1 h in
    let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
    grid [ [a; hbar; b]; [vbar; i; vbar]; [d; hbar; c] ]

let take w h i = I.(vsnap h i |> hsnap w)
let imgf (ow, oh) (w, h) =
  let open I in
      let (a1, a2, a3) = A.(fg lightmagenta, fg lightred, fg lightblue) in
      strf "Sizing edge behavior. Dim: (%d, %d)" w h <->
      ( hdistribute ow [
          outline a1 (uchar a1 (Uchar.of_int 0x2022) w h)
        ; outline a2 (uchar a2 (Uchar.of_int 0x2022) 300 300 |> take w h)
        ; outline a3 (void w h)
        ] |> vsnap (oh - 4) )
      <->
      hdistribute ow [string a1 "char"; string a2 "crop"; string a3 "void"]

let () =
  simpleterm ~imgf ~f ~s
