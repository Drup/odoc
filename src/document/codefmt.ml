open Types

type out = Source.t

type Format.stag +=
  | Elt of Inline.t

let make () =
  let open Inline in
  let out : out ref = ref [] in
  let opened_tag : Source.decoration Stack.t = Stack.create () in
  let push elt = out := (Stack.top_opt opened_tag, elt) :: !out in
  let out_functions = {Format.
    out_string = (fun s i j -> push [inline @@ Text (String.sub s i j)]);
    out_flush = (fun () -> ());
    out_newline = (fun () -> push [inline @@ Linebreak]);
    out_spaces = (fun n -> push [inline @@ Text (String.make n ' ')]);
    out_indent = (fun n -> push [inline @@ Text (String.make n ' ')])
  }
  and stag_functions =
    let print_open_stag = function
      | Elt elt -> push elt
      | Format.String_tag s -> Stack.push s opened_tag
      | _ -> ()
    and print_close_stag = function
      | Elt _ -> ()
      | Format.String_tag _ -> ignore (Stack.pop_opt opened_tag)
      | _ -> ()
    in {Format.
      mark_open_stag = (fun _ -> "");
      mark_close_stag = (fun _ -> "");
      print_open_stag; print_close_stag;
    }
  in
  let formatter = Format.formatter_of_out_functions out_functions in
  Format.pp_set_formatter_stag_functions formatter stag_functions;
  (fun () -> !out), formatter

let elt ppf e = Format.pp_open_stag ppf (Elt e); Format.pp_close_stag ppf ()

let entity e ppf = elt ppf [inline @@ Inline.Entity e]

let spf fmt =
  let flush, ppf = make () in
  Format.kfprintf (fun _ -> flush ()) ppf fmt

let pf = Format.fprintf

(** Transitory hackish API *)

let (++) f g ppf = f ppf; g ppf
let span f ppf = pf ppf "@[%t@]" f
let df = Format.dprintf
let txt s ppf = Format.pp_print_string ppf s
let noop (_ : Format.formatter) = ()

let (!) (pp : _ Fmt.t) x ppf = pp ppf x

let rec list ?sep ~f = function
  | [] -> noop
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = list ?sep ~f xs in
    match sep with
    | None -> hd ++ tl
    | Some sep -> hd ++ sep ++ tl

let render f = spf "%t" f
let code ?attr f =
  [inline ?attr @@ Inline.Source (render f)]
let documentedSrc ?(attr=[]) f =
  [DocumentedSrc.Code { attr ; code = render f }]
let codeblock ?attr f =
  [block ?attr @@ Block.Source (render f)]

let keyword keyword =
  df "@[<keyword>%s@]" keyword

module Infix = struct
  let (!) = (!)
  let (++) = (++)
end
