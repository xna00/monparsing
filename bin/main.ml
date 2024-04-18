type char_list = char list

let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)

type 'a t = char_list -> ('a * char_list) list

let result : 'a -> 'a t = fun v inp -> [ (v, inp) ]

let bind : 'a t -> ('a -> 'b t) -> 'b t =
 fun p f inp ->
  let t = p inp in
  List.concat_map (fun (v, out) -> f v out) t

let ( >>= ) = bind
let zero : 'a t = fun _ -> []

let item : char t =
 fun inp -> match inp with [] -> [] | c :: cs -> [ (c, cs) ]

let plus : 'a t -> 'a t -> 'a t = fun p q inp -> p inp @ q inp
let ( ++ ) = plus

let sat : (char -> bool) -> char t =
 fun p -> bind item (fun x -> if p x then result x else zero)

let char : char -> char t = fun c -> sat (fun x -> x == c)
let digit : char t = sat (function '0' .. '9' -> true | _ -> false)
let lower = sat (function 'a' .. 'z' -> true | _ -> false)
let upper = sat (function 'A' .. 'Z' -> true | _ -> false)
let letter = lower ++ upper
let alphanum = letter ++ digit

let rec string s =
  match explode s with
  | [] -> result ""
  | x :: xs ->
      char x >>= fun _ ->
      string (implode xs) >>= fun _ -> result (implode (x :: xs))

let rec many : 'a t -> 'a list t =
 fun p ->
  ( p >>= fun x ->
    many p >>= fun xs -> result (x :: xs) )
  ++ result []

let word = many letter

let ident =
  lower >>= fun x ->
  many alphanum >>= fun xs -> result (x :: xs)

let many1 p =
  p >>= fun x ->
  many p >>= fun xs -> result (x :: xs)

let nat = many1 digit >>= fun n -> result (int_of_string (implode n))

let int =
  ( char '-' >>= fun _ ->
    nat >>= fun n -> result (-n) )
  ++ nat

(* [1,-2,3] *)
let ints =
  char '[' >>= fun _ ->
  int >>= fun n ->
  many (char ',' >>= fun _ -> int) >>= fun ns ->
  char ']' >>= fun _ -> result (n :: ns)

let sepby1 p sep =
  p >>= fun x ->
  many (sep >>= fun _ -> p) >>= fun xs -> result (x :: xs)

let ints =
  char '[' >>= fun _ ->
  sepby1 int (char ',') >>= fun ns ->
  char ']' >>= fun _ -> result ns

let bracket opn p close =
  opn >>= fun _ ->
  p >>= fun ns ->
  close >>= fun _ -> result ns

let ints = bracket (char '[') (sepby1 int (char ',')) (char ']')
let sepby p sep = sepby1 p sep ++ result []

module Json = Monparsing.Json

