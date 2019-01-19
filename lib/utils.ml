open Notty
let maxby f xs = List.(fold_left max 0 (map f xs))

(* column : align:[`Left|`Middle|`Right] -> image list -> image *)
let column ~align images =
  let width = maxby I.width images in
  List.map (I.hsnap ~align width) images |> I.vcat

let hdistribute ?align w imgs =
  let n = List.length imgs in
  I.(List.map (hsnap ?align (w / n)) imgs |> hcat)

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let center w h i = I.(vsnap h i |> hsnap w)
(** center (pad or crop) image i in a new image with dimension w and h *)

type outline_int_decorations =
  { tl: int; (** top left *)
    tr: int; (** top right *)
    bl: int; (** bottom left *)
    br: int; (** bottom right *)
    vb: int; (** vertical border *)
    hb: int; (** horizontal border *)
  }
(** Define decoration to be used with the outline function *)

let outline attr decs img =
  let (w, h) = I.(width img, height img) in
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and hbar  = I.uchar attr (Uchar.of_int decs.hb) w 1
  and vbar  = I.uchar attr (Uchar.of_int decs.vb) 1 h in
  let (a, b, c, d) = (chr decs.tl, chr decs.tr, chr decs.br, chr decs.bl) in
  grid [ [a; hbar; b]; [vbar; img; vbar]; [d; hbar; c] ]
(** Drawn a decoration outline around an image *)

let random_string max_length =
  let length = Random.int(max_length - 1) + 1 in
  let _gen () = match Random.int(26+26+10) with
    | n when n < 26 -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n -> int_of_char '0' + n - 26 - 26
  in
  let _gen _ = String.make 1 (char_of_int(_gen())) in
  String.concat "" (Array.to_list (Array.init length _gen))

let list_of_random_strings len str_max_length =
  let rec _build acc = function
    | 0 -> acc
    | i -> let s = random_string str_max_length in
      _build (s :: acc) (i - 1)
  in
  _build [] len
