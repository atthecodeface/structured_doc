type dtd       = Xmlm.dtd
type name      = Xmlm.name
type attribute = Xmlm.attribute
type tag       = Xmlm.tag
type signal    = Xmlm.signal
type pos       = Xmlm.pos

type error = Xmlm.error
exception Error = Xmlm.Error
type t_tagoc = [ | `TagOpen     of (bool * int * tag * pos)  (* bra, depth, (name, attributes)  *)
                 | `TagKet      of (int * name) (* depth, name  *)
               ]

type input = {
    reader : Reader.t;
    concat_data : string option;
    mutable tag_depth : int;
    mutable pending_tag : t_tagoc option;
    mutable pending_data : string list;
    mutable pending_end  : bool;
    mutable tag_stack : tag list;
  }

let raise_error t e = raise (Error ((Reader.pos t.reader), e))
let skip_whitespace  t = Reader.skip_whitespace t.reader
let skip_at_least_one_whitespace  t = 
    if (not (Reader.skip_at_least_one_whitespace t.reader)) then raise_error t `Malformed_char_stream
let peek_char  t = Reader.peek_char t.reader
let get_char   t = Reader.get_char t.reader
let unget      t = Reader.unget t.reader
let unget_char t = Reader.unget_char t.reader


let string_of_revlist rl : string =
    let n = List.length rl in
    let s = Bytes.make n ' ' in
    List.iteri (fun i x-> Bytes.unsafe_set s (n-1-i) (Char.chr x)) rl;
    Bytes.to_string s

let read_until_list ?(initial_letters:int list=[]) t stop_if illegal_if =
    let rec build_revstring rl =
      match get_char t with
        | `Ch ch when (stop_if rl ch) -> rl
        | `Ch ch when (not (illegal_if rl ch)) -> (
          build_revstring (ch::rl)
          )
        | `Ch ch -> raise_error t `Malformed_char_stream
        | _ -> raise_error t `Unexpected_eoi
    in
    string_of_revlist (build_revstring initial_letters)

let read_until ?(initial_letters:int list=[]) t stop_if illegal_if =
    let rec build_revstring rl =
      match get_char t with
        | `Ch ch when (stop_if ch) -> rl
        | `Ch ch when (not (illegal_if ch)) -> (
          build_revstring (ch::rl)
          )
        | `Ch ch -> raise_error t `Malformed_char_stream
        | _ -> raise_error t `Unexpected_eoi
    in
    string_of_revlist (build_revstring initial_letters)

let read_while ?(initial_letters:int list=[]) t continue_if =
    let rec build_revstring rl =
      match get_char t with
        | `Ch ch when (continue_if ch) -> (
          build_revstring (ch::rl)
          )
        | `Ch ch -> (unget_char t ch; rl)
        | _ -> rl
    in
    string_of_revlist (build_revstring initial_letters)

let str_pt t =
  match t.pending_tag with 
  | None -> "None"
  | Some `TagOpen (bra,depth,((ns,name),attrs), pos) ->
     Printf.sprintf "Open %s:%2d:%s,%s" (if bra then "t" else "f") depth ns name
  | Some `TagKet  (depth, (ns,name)) -> 
     Printf.sprintf "Close  :%2d:%s,%s" depth ns name

(*f verbose *)
let verbose t r =
  let (l,c) = Reader.pos t.reader in
  Printf.printf ">>%3d,%3d:%40s:%2d:%2d:%40s:\n" l c r t.tag_depth (List.length t.tag_stack) (str_pt t)
let verbose _ _ = ()

(*f get_tag_depth
input has a tag start character
read tag characters
return depth
 *)
let get_tag_depth t =
  verbose t "get_tag_depth";
  let rec add_tag_depth n =
    match (get_char t) with
    | `Ch ch when (Uchar.is_tag ch) -> add_tag_depth (n+1)
    | l -> (
      unget t l;
      n
    )
  in
  add_tag_depth 0

(*f get_name
input should have a name start character
read name
 *)
let get_name t =
  verbose t "get_name";
  match (get_char t) with
    | `Ch ch when (Uchar.is_name_start ch) -> (
      ("",read_while ~initial_letters:[ch] t Uchar.is_name)
                    )
    | _ -> raise_error t `Malformed_char_stream

(*f get_value
pointing at start of string delimiter
strings start with ' or " and end with the same " or '
'characters' can be &<name>; and anything not < & and the end marker
 *)
let get_value t = 
  verbose t "get_value";
  match (get_char t) with
  | `Ch ch when (Uchar.is_quote ch) -> (
    read_until t (fun x->x==ch) Uchar.is_newline
  )
  | `Ch x -> (
    unget_char t x;
    read_while t (fun x->not (Uchar.is_whitespace x))
  )
  | _ -> raise_error t `Malformed_char_stream

(*f get_attribute
pointing at start of char
 *)
let get_attribute t : attribute = 
  verbose t "get_attribute";
  let name=get_name t in
  match (get_char t) with
  | `Ch ch when (Uchar.is_equal ch) -> (
    let value=get_value t in
    (name,value)
  )
  | _ -> raise_error t `Malformed_char_stream
    
(*f get_attributes
skip whitespace
peek - if next is attribute start then get attribute
 *)
let get_attributes t : attribute list = 
  verbose t "get_attributes";
  let rec build_revattributes rl =
    skip_whitespace t;
    match (peek_char t) with
    | `Ch ch when (Uchar.is_name_start ch) -> (
      let attr = get_attribute t in
      build_revattributes (attr::rl)
    )
    | _ -> rl
  in
  let rl = build_revattributes [] in
  List.rev rl

(*f get_tag
input has a tag start character
read tag characters and get depth
read tag name
read attributes
 *)
let get_tag t =
  verbose t "get_tag";
  let pos = Reader.pos t.reader in
  let depth = get_tag_depth t in
  let name = get_name t in
  match (get_char t) with
  | `Ch ch when (ch==125) -> ( (* } *)
    `TagKet (depth, name)
  )
  | `Ch ch when (ch==123) -> ( (* { *)
    skip_at_least_one_whitespace t;
    let attributes = get_attributes t in
    `TagOpen (true, depth, (name, attributes), pos)
  )
  | l -> (
    unget t l;
    skip_at_least_one_whitespace t;
    let attributes = get_attributes t in
    `TagOpen (false, depth, (name, attributes), pos)
  )
    
(*f get_string
input has a quote start character
read quote characters (1 or 3)
read string characters (including newline) up to enclosing quote characters
 *)
let get_string t =
  verbose t "get_string";
  let end_of_triple_quote quote_ch rl ch =
    if (quote_ch!=ch) then false else
      match rl with
      | ch0::ch1::_ when ((ch0==quote_ch) && (ch1==quote_ch)) -> true
      | _ -> false
  in
  let quote_ch = match (get_char t) with | `Ch ch -> ch | _ -> raise_error t `Unexpected_eoi  in
  let ch = match (get_char t) with | `Ch ch -> ch | _ -> raise_error t `Unexpected_eoi  in
  if (ch==quote_ch) then ( (* got 2 successive quote characters - is it three? *)
    match get_char t with
    | `Ch ch when (ch==quote_ch) -> (* 3 successive quote characters *)
       let s_plus_two_quotes = (read_until_list t (end_of_triple_quote quote_ch) (fun _ _ -> false)) in
       let n = String.length s_plus_two_quotes in
       String.sub s_plus_two_quotes 0 (n-2)
    | `Ch ch -> (* just 2 quote characters - empty string *)
      ( unget_char t ch; "" )
    | _ -> (* Eoi after 2 quote characters - empty string, nothing to unget *)
      ""
  ) else ( (* 1 quote character *)
    unget_char t ch;
    read_until t (fun ch -> (ch==quote_ch)) Uchar.is_newline
  )
    
(*f get_token
skip whitespace
if it is a tag then parse the tag and any attributes
a tag starts with a tag character and finishes after last attribute

Then next character must be EOF, tag_start, or quotation for cdata
 *)
let rec get_token t =
  verbose t "get_token";
  if t.pending_end then (
    match t.pending_data with
    | [] -> (t.pending_end <- false; `El_end)
    | hd::tl -> (
      match t.concat_data with
      | None -> (t.pending_data<-tl; `Data hd)
      | Some ch -> (
        let r = String.concat ch t.pending_data in
        t.pending_data <- []; 
        `Data r
      )
    )
  ) else (
    match t.pending_tag with
    | Some (`TagKet (d,name)) -> ( (* pop top of tag stack *)
      if (t.tag_depth==0) then (
        t.tag_stack <- List.tl t.tag_stack; (* t.tag_stack should match name *)
        t.tag_depth <- d;
        t.pending_end <- true;
        get_token t
      ) else (
        t.tag_depth <- t.tag_depth - 1;
        t.tag_stack <- List.tl t.tag_stack;
        t.pending_end <- true;
        get_token t
      )
    )
    | Some (`TagOpen (bra, d, _, _)) when (d<=t.tag_depth) -> ( (* pop top of tag stack *)
      verbose t "get_token2";
      t.tag_depth <- t.tag_depth - 1;
      t.tag_stack <- List.tl t.tag_stack;
      t.pending_end <- true;
      get_token t
    )
    | Some (`TagOpen (bra, d, tag, _)) when (d==t.tag_depth+1) -> ( (* pending tag is at tag_depth+1 enter pending tag *)
      if bra then (
        t.tag_depth <- 0;
        t.tag_stack <- tag :: t.tag_stack;
        t.pending_tag <- None;
        `El_start tag
      ) else (
        t.tag_depth <- t.tag_depth + 1;
        t.tag_stack <- tag :: t.tag_stack;
        t.pending_tag <- None;
        `El_start tag
      )
    )
    | Some (`TagOpen (_, _, tag, (l,c))) -> ( (* invalid tag depth *)
      Printf.printf "Start of tag for error (%d,%d)\n" l c;
      Printf.printf "Pending tag %s\n" (str_pt t);
      Printf.printf "Tag depth %d\n" t.tag_depth;
      raise_error t `Malformed_char_stream
    )
    | None -> (
      skip_whitespace t;
      match (get_char t) with
      | `Ch ch when (Uchar.is_comment_start ch) -> (
        ignore (read_until t Uchar.is_newline (fun x->false));
        get_token t
      )
      | `Ch ch when (Uchar.is_tag_start ch) -> (
        (unget_char t ch) ;
        t.pending_tag <- Some (get_tag t);
        get_token t
      )
      | `Ch ch when (Uchar.is_quote ch) -> (
        (unget_char t ch) ;
        t.pending_data <- t.pending_data @ [get_string t];
        get_token t
      )
      | `Ch ch -> (
        raise_error t `Malformed_char_stream
      )
      | _ -> (
        match t.tag_stack with
        | hd :: tl -> (
          t.tag_depth <- t.tag_depth - 1;
          t.tag_stack <- List.tl t.tag_stack;
          t.pending_end <- true;
          get_token t
        )
        | [] ->
           raise_error t `Unexpected_eoi
      )
    )
  )

let make_input ?concat_data ?doc_tag:tag source =
    let reader = Reader.make source in
    let tag_stack = match tag with | Some x -> [x] | None -> [] in
    {reader; tag_stack; tag_depth=0; pending_tag=None; pending_data=[]; pending_end=false; concat_data}

let input t =
    get_token t

