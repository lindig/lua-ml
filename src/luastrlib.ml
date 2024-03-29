module M (I : Lua.Lib.CORE) = struct
  module V = I.V
  let ( **-> ) = V.( **-> )
  let ( **->> ) x y = x **-> V.result y
  let ( *****->> ) = V.dots_arrow
  external format_int: string -> int -> string = "caml_format_int"
external format_float: string -> float -> string = "caml_format_float"
let add_quoted_string buf s =
  let escape delim c = c == delim || c == '\n' || c == '\\' in
  let delim = if String.contains s '\'' then '"' else '\'' in
  let add c =
    if escape delim c then (Buffer.add_char buf '\\'); Buffer.add_char buf c in
  Buffer.add_char buf delim;
  String.iter add s;
  Buffer.add_char buf delim

let bprintf_internal buf format =
  let rec doprn i args =
    if i >= String.length format then
      begin
        let res = Buffer.contents buf in
        Buffer.clear buf; (* just in case [bs]printf is partially applied *)
        res
      end
    else begin
      let c = String.get format i in
      if c <> '%' then begin
        Buffer.add_char buf c;
        doprn (succ i) args
      end else begin
        let j = skip_args (succ i) in
        (* Lua conversions: d i o u x X   e E    f g   c  s  p %   q *)
        (*                  ^ ^ ^ ^ ^ ^   ^ ^    ^ ^   ^  ^    ^     *)
        match String.get format j with
        | '%' ->
            Buffer.add_char buf '%';
            doprn (succ j) args
        | c ->
            let arg, args =
              match args with h :: t -> h, t
              | [] -> I.error 
                      "Not enough arguments to string-library function `format'" in
            match c with 
            | 's' ->
                let s = V.string.V.project arg in
                if j <= i+1 then
                  Buffer.add_string buf s
                else begin
                  let p =
                    try
                      int_of_string (String.sub format (i+1) (j-i-1))
                    with _ ->
                      invalid_arg
                        ("format: bad %s format `" ^ String.sub format i (j-i) ^ "'")in
                  if p > 0 && String.length s < p then begin
                    Buffer.add_string buf (String.make (p - String.length s) ' ');
                    Buffer.add_string buf s
                  end else if p < 0 && String.length s < -p then begin
                    Buffer.add_string buf s;
                    Buffer.add_string buf (String.make (-p - String.length s) ' ')
                  end else
                    Buffer.add_string buf s
                end;
                doprn (succ j) args
        | 'c' ->
            let c =
              try Char.chr (V.int.V.project arg)
              with Invalid_argument _ -> V.projection arg "Character code" in  
            Buffer.add_char buf c;
            doprn (succ j) args
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            let n = V.int.V.project arg in
            Buffer.add_string buf (format_int (String.sub format i (j-i+1)) n);
            doprn (succ j) args
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            let f = V.float.V.project arg in
            Buffer.add_string buf (format_float (String.sub format i (j-i+1)) f);
            doprn (succ j) args
        | 'p' ->
            I.error   ("string library does not implement format specifier '%" ^
                       String.make 1 c ^ "'")
        | 'q' ->
            if j <= i+1 then
              add_quoted_string buf (V.string.V.project arg)
            else
              I.error "length not permitted with format specifier '%q'";
            doprn (succ j) args
        | c ->
            I.error   ("bad format specifier '%" ^ String.make 1 c ^ "'")
      end
    end

  and skip_args j =
    match String.get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | _ -> j

  in doprn 0

let format fmt args = bprintf_internal (Buffer.create 16) fmt args

type 'a parse = int -> (int -> (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a

let strindex = { V.embed   = (fun n -> V.int.V.embed (n+1))
               ; V.project = (fun v -> V.int.V.project v - 1)
               ; V.is = V.int.V.is
               } 

let string_builtins =
  let invalid f x =
    try f x with Invalid_argument m -> I.error ("Invalid argument: " ^ m) in
  let wrap_inv = function
    | V.LuaValueBase.Function (l, f) -> V.LuaValueBase.Function(l, invalid f)
    | v -> raise (V.Projection (v, "function")) in
  let ifunc ty f = wrap_inv (V.efunc ty f) in
  let explode s =
  let rec add n cs = if n = 0 then cs else add (n-1) (s.[n-1] :: cs) in
  add (String.length s) []  in
let orp  p1 p2 c = p1 c || p2 c   in
let range l h c = l <= c && c <= h    in
let lower = range 'a' 'z'   in
let upper = range 'A' 'Z'   in
let digit = range '0' '9'   in
let space c = c = ' ' || c = '\t' || c = '\r' || c = '\n'   in
let letter = orp lower upper   in
let alnum  = orp letter digit   in
let non p c = not (p c)   in
let percent = function
  | 'a' -> letter | 'A' -> non letter
  | 'd' -> digit  | 'D' -> non digit 
  | 'l' -> lower  | 'L' -> non lower 
  | 's' -> space  | 'S' -> non space
  | 'u' -> upper  | 'U' -> non upper
  | 'w' -> alnum  | 'W' -> non alnum
  | c when non alnum c -> (=) c
  | _ -> I.error "bad % escape in pattern"   in
let cclass cs = 
  let orr p (p', cs) = orp p p', cs in
  let rec pos cs = match cs with
    | ']' :: cs -> orr ((=) ']') (pos2 cs)
    | cs -> pos2 cs
  and pos2 cs = match cs with
  | '-' :: cs -> orr ((=) '-') (pos3 cs)
  | ']' :: cs -> (fun _ -> false), cs
  | _         -> pos3 cs
  and pos3 cs = match cs with
  | '%' :: c :: cs -> orr (percent c) (pos3 cs)
  | ']' :: cs      -> (fun _ -> false), cs
  | c   :: '-' :: ']' :: cs -> orp ((=) c) ((=) '-'), cs
  | c   :: '-' :: c'  :: cs -> orr (range c c') (pos3 cs)
  | c   :: cs               -> orr ((=) c) (pos3 cs)
  | [] -> I.error "bad character class in pattern" in
  match cs with
  | '^' :: cs -> let p, cs = pos cs in non p, cs
  | _ -> pos cs   in

let find pat s =
  let prerr_string _ = () in
  let length = String.length s in
  let () = prerr_string "=========\n" in
  let lefts  = ref [] in
  let pairs = ref [] in
  let push l x = l := x :: !l in
  let pop l =
    match !l with n :: ns -> (l := ns; n) | [] -> I.error "unmatched )" in
  let lparen lp i succ fail =
    push lefts (lp, i); succ i (fun () -> ignore (pop lefts);  fail()) in
  let rparen i succ fail =
    let lp, start = pop lefts in
    push pairs (lp, start, i);
    succ i (fun () -> ignore (pop pairs); push lefts (lp, start); fail()) in
  let captures () =
    let rec insert ((i, _l, _r) as p) = function
      | [] -> [p]
      | (i', _, _) as p' :: ps -> if i < i' then p :: p' :: ps else p' :: insert p ps
    in let pairs = List.fold_right insert (!pairs) [] in
    List.map (fun (_, l, r) -> String.sub s l (r-l)) pairs in
  let atend i   succ fail = if i = length then succ i fail else fail () in
  let atstart i succ fail = if i = 0      then succ i fail else fail () in
  let opt  r i succ fail = r i succ (fun () -> succ i fail) in
  let (||) r r' i succ fail =
    r i succ (fun () -> r' i succ fail) in
  let (>>) r r' i succ fail =
    r i (fun i' resume -> r' i' succ resume) fail in
  let atzero r i succ fail = r i (succ 0) fail in
  let rec anywhere r i succ fail =
    r i (succ i) (fun () -> if i = length then fail ()
                            else anywhere r (i+1) succ fail) in
  let nonempty (r:'a parse) i succ fail =
    r i (fun i' resume -> if i = i' then resume () else succ i' resume) fail in
  let empty i succ fail = succ i fail in
  let rec star (r : 'a parse) = ((fun i -> ((nonempty r >> star r) || empty) i) : 'a parse) in
  let char p i succ fail =
    if (try p s.[i] with _ -> false) then succ (i+1) fail else fail () in
  let comp pat =
    let rec comp lps c cs = 
      let rec finish (p, cs) = 
        match cs with
        | '*' :: cs -> finish (star p, cs)
        | '?' :: cs -> finish (opt  p, cs)
        | []        -> p
        | c :: cs   -> p >> comp lps c cs in
      match c, cs with
      | '%', c :: cs -> finish (char (percent c), cs)
      | '$', []      -> atend
      | '.', cs      -> finish (char (fun _ -> true), cs)
      | '[', cs      -> let p, cs = cclass cs in finish(char p, cs)
      | '(', c :: cs -> lparen lps >> comp (lps+1) c cs
      | ')', c :: cs -> rparen     >> comp lps     c cs
      | ')', []      -> rparen
      | c  , cs      -> finish (char ((=) c), cs) in
    match pat with
    | [] -> fun i succ fail -> if i <= length then succ i i fail else fail ()
    | '^' :: [] -> atzero atstart
    | '^' :: c :: cs -> atzero (atstart >> comp 0 c cs)
    | c :: cs -> anywhere (comp 0 c cs) in
  let with_caps p i succ fail = p i (fun i res -> succ (captures()) i res) fail in
  with_caps (comp pat) in
 
  let quote_char c t = if alnum c then c :: t else '%' :: c :: t in
  let quote_pat p = List.fold_right quote_char (explode p) [] in
  let strfind s pat init plain =
    let int    i = V.int.V.embed    i in
    let string s = V.string.V.embed s in
    let pat = match plain with Some _ -> quote_pat pat | None -> explode pat in
    find pat s init
      (fun caps i j _ -> int (i+1) :: int j :: List.map string caps)
      (fun () -> [V.LuaValueBase.Nil]) in
  [ "strfind", V.efunc (V.string **-> V.string **-> V.default 0 strindex **->
                        V.option V.int **-> V.resultvs) strfind
  ; "strlen",  V.efunc (V.string **->> V.int) String.length
  ; "strsub",
    (V.efunc (V.string **-> strindex **-> V.option strindex **->> V.string))
    (fun s start last ->
      let maxlast = String.length s - 1 in
      let last = match last with None -> maxlast
                               | Some n -> min n maxlast in
      let len = last - start + 1 in
      invalid (String.sub s start) len)
  ; "strlower", V.efunc (V.string **->> V.string) String.lowercase_ascii
  ; "strupper", V.efunc (V.string **->> V.string) String.uppercase_ascii
  ; "strrep",   V.efunc (V.string **-> V.int **->> V.string)
                (fun s n ->
                  if n < 0 then
                    raise (Invalid_argument ("number of replicas " ^ string_of_int n ^
                                             " is negative"))
                  else
                    let rec list l = function 0 -> l | n -> list (s::l) (n-1) in
                    String.concat "" (list [] n))
  ; "ascii",   V.efunc (V.string **-> V.default 0 strindex **->> V.int)
               (fun s i -> Char.code (String.get s i))
  ; "format",  ifunc (V.string **-> V.value *****->> V.string) format
  ; "gsub",    V.caml_func (fun _ -> I.error "string library does not implement gsub")
  ] 

  let init = I.register_globals string_builtins
end
