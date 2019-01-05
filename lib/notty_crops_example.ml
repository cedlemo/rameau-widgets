open Notty
open Utils

let s = (2, 2)
let f (w, h as s) = function
    `Key (`Arrow `Left, _) -> Some (w - 1, h)
  | `Key (`Arrow `Right, _) -> Some (w + 1, h)
  | `Key (`Arrow `Up, _) -> Some (w, h - 1)
  | `Key (`Arrow `Down, _) -> Some (w, h + 1)
  | `Key (`ASCII '0', _) -> Some (0, 0)
  | _ -> Some s

let decorations = {
  tl = 0x256d;
  tr = 0x256e;
  br = 0x256f;
  bl = 0x2570;
  hb = 0x2500;
  vb = 0x2502;
}

let imgf (ow, oh) (w, h) =
  let open I in
      let (a1, a2, a3) = A.(fg lightmagenta, fg lightred, fg lightblue) in
      strf "Sizing edge behavior. Dim: (%d, %d)" w h <->
      ( hdistribute ow [
          outline a1 decorations (uchar a1 (Uchar.of_int 0x2022) w h);
          outline a2 decorations (uchar a2 (Uchar.of_int 0x2022) 300 300 |> center w h);
          outline a3 decorations (void w h)
        ] |> vsnap (oh - 4) )
      <->
      hdistribute ow [string a1 "char"; string a2 "crop"; string a3 "void"]


