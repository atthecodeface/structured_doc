module Hmlm = Hmlm
open Xmlm
type dtd       = Xmlm.dtd
type name      = Xmlm.name
type attribute = Xmlm.attribute
type tag       = Xmlm.tag
type signal    = Xmlm.signal

let ns_xml   = Xmlm.ns_xml
let ns_xmlns = Xmlm.ns_xmlns

let name_string (ns,n) =
    if (String.length ns)==0 then n else Printf.sprintf "%s:%s" ns n

let attr_string (n,v) =
    Printf.sprintf "%s='%s'" (name_string n) v


let pp_dtd        = Xmlm.pp_dtd
let pp_name       = Xmlm.pp_name
let pp_attribute  = Xmlm.pp_attribute
let pp_tag        = Xmlm.pp_tag

type input = [ | `Xmlm of Xmlm.input 
               | `Hmlm of Hmlm.input
             ]

let make_xmlm t = `Xmlm t
let make_hmlm t = `Hmlm t

let input = function
  | `Xmlm t -> Xmlm.input t
  | `Hmlm t -> Hmlm.input t

let rec pp t =
    let s = input t in
    match s with
    | `El_start ((ns,name),attrs) -> (
      Format.open_tag (Printf.sprintf "%s:%s%s" ns name (List.fold_left (fun acc attr -> acc ^ " " ^ (attr_string attr)) "" attrs));
      Format.print_string "Stuff";
      pp t ;
      pp t
    )
    | `El_end ->
      Format.close_tag ();
    | _ -> ()

