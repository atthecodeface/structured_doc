type t = [ | `Ch_eof  | `Ch of int ]
let is_newline ch =
    match ch with | 10 | 133 -> true
                  | _ -> false

let is_whitespace ch =
    match ch with | 9 | 10 | 11 | 12 | 13 | 32 | 133 | 160 -> true
                  | _ -> false

let is_digit ch = ((ch>=48) && (ch<=57))

let is_comment_start ch =
    match ch with | 59 -> true
                  | _ -> false

let is_tag_start ch =
    match ch with | 35 -> true
                  | _ -> false

let is_tag ch =
    match ch with | 35 -> true
                  | _ -> false

let is_equal ch = (ch==61)

let is_quote ch = (ch==39) || (ch==34)

let is_name_start ch =
    match ch with | 58 | 95 -> true (* : or _ *)
                  | _ -> (
                    if ((ch>=65) && (ch<=90)) then true else    (* A-Z *)
                    if ((ch>=97) && (ch<=122)) then true else   (* a-z *)
                    if ((ch>=0xc0) && (ch<=0xd6)) then true else
                    if ((ch>=0xd8) && (ch<=0xf6)) then true else
                    if ((ch>=0xf8) && (ch<=0x2ff)) then true else
                    if ((ch>=0x370) && (ch<=0x37d)) then true else
                    if ((ch>=0x37f) && (ch<=0x1fff)) then true else
                    if ((ch>=0x200c) && (ch<=0x200d)) then true else
                    if ((ch>=0x2070) && (ch<=0x218f)) then true else
                    if ((ch>=0x2c00) && (ch<=0x2fef)) then true else
                    if ((ch>=0x3001) && (ch<=0xd7ff)) then true else
                    if ((ch>=0xf900) && (ch<=0xfdcf)) then true else
                    if ((ch>=0xfdf0) && (ch<=0xfffd)) then true else
                    if ((ch>=0x10000) && (ch<=0xeffff)) then true else
                      false )

let is_name ch = 
  (is_name_start ch) || (
    if ((ch==45) || (ch==46) || (ch==0xb7)) then true else (* - . *)
      if ((ch>=48) && (ch<=57)) then true else    (* 0-9 *)
        if ((ch>=0x399) && (ch<=0x36f)) then true else
          if ((ch>=0x203f) && (ch<=0x2040)) then true else
            false
  )

