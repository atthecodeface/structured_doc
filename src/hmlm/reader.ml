type t = {
    source : Xmlm.source;
    mutable i : int;
    mutable in_hand: Uchar.t;
    mutable has_been_given: bool;
    mutable line : int;
    mutable char : int;
  }

let get_char_from_source t =
 match t.source with
  | `Channel ch   -> (
    try (`Ch (input_byte ch)) with _ -> `Ch_eof
  )
  | `String (n,s) -> (
    if (n+t.i >= (String.length s)) then `Ch_eof else
    (let ch = `Ch (Char.code (String.get s (n+t.i))) in
    t.i<-t.i+1;
    ch))
  | `Fun f        -> try (`Ch (f ())) with | End_of_file -> `Ch_eof

let make source = 
    let t = {source; i=0; in_hand=`Ch 0; has_been_given=false; line=1; char=0;} in
    t.in_hand <- get_char_from_source t;
    t

let pos t = (t.line, t.char)

let get_in_hand t =
  match t.in_hand with
  | `Ch_eof -> (
    t.has_been_given <- false; ()
  )
  | _ -> (
    t.in_hand <- get_char_from_source t ;
    t.char <- t.char + 1;
    match t.in_hand with | `Ch ch when (Uchar.is_newline ch) -> (
                        t.char <- 0;
                        t.line <- t.line + 1;
                      )
                      | _ -> () ;
    t.has_been_given <- false ;
    ()
  )

let get_char t =
    if t.has_been_given then (get_in_hand t);
    t.has_been_given <- true;
    t.in_hand

let peek_char t =
    if t.has_been_given then (get_in_hand t);
    t.has_been_given <- false;
    t.in_hand

let unget t l =
    t.in_hand <- l;
    t.has_been_given <- false;
    ()

let unget_char t ch = unget t (`Ch ch)

(*f skip_whitespace *)
let rec skip_whitespace t =
    match (peek_char t) with
    | `Ch ch when (Uchar.is_whitespace ch) -> (
    ignore (get_char t);
    skip_whitespace t)
    | _ -> ()

(*f skip_at_least_one_whitespace *)
let skip_at_least_one_whitespace t =
    match (get_char t) with
    | `Ch ch when (Uchar.is_whitespace ch) -> (
        skip_whitespace t;
        true)
    | _ -> false

