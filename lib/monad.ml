module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type Monoid = sig
  type 'a t

  val zero : 'a t
  val ( ++ ) : 'a t -> 'a t -> 'a t
end

module StateMMonad (M : sig
  type 'a t

  include Monad with type 'a t := 'a t
  include Monoid with type 'a t := 'a t
end) (N : sig
  type s
end) =
struct
  type s = N.s
  type 'a t = s -> ('a * s) M.t

  let return : 'a -> 'a t = fun (v : 'a) s -> M.return (v, s)

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
   fun stm1 f s -> M.( >>= ) (stm1 s) (fun (v, s') -> f v s')

  let zero _ = M.zero

  let ( ++ ) (stm : 'a t) (stm' : 'a t) : 'a t =
   fun s -> M.( ++ ) (stm s) (stm' s)

  let update : (s -> s) -> s t =
   fun f s -> try M.return (s, f s) with _ -> zero s

  let set s = update (fun _ -> s)
  let fetch = update (fun x -> x)
end

module ListMonad = struct
  type 'a t = 'a list

  let return v = [ v ]
  let ( >>= ) l f = List.concat_map f l
  let zero = []
  let ( ++ ) l l' = l @ l'
end

module ResultMonad = struct
  type error = Zero
  type 'a t = ('a, error) result

  let return v : 'a t = Ok v

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
   fun r f -> match r with Ok v -> f v | Error e -> Error e

  let zero : 'a t = Error Zero

  let ( ++ ) : 'a t -> 'a t -> 'a t =
   fun r r' -> match r with Ok _ -> r | Error _ -> r'
end

type char_list = char list
type pos = int * int
type pstring = pos * char_list

let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)

module StateM =
  StateMMonad
    (ResultMonad)
    (struct
      type s = pstring
    end)

open StateM

type 'a parser = 'a t

let newstate : pstring -> pstring =
 fun ((l, c), chars) ->
  match chars with
  | [] -> raise End_of_file
  | x :: xs ->
      let newpos = match x with '\n' -> (l + 1, 0) | _ -> (l, c + 1) in
      (newpos, xs)

let item : char parser = update newstate >>= fun (_, s) -> return (List.hd s)

let sat : (char -> bool) -> char parser =
 fun p -> item >>= fun c -> if p c then return c else fun s -> zero s

let char c = sat (fun x -> x = c)
let digit : char t = sat (function '0' .. '9' -> true | _ -> false)
let lower = sat (function 'a' .. 'z' -> true | _ -> false)
let upper = sat (function 'A' .. 'Z' -> true | _ -> false)
let letter = lower ++ upper
let alphanum = letter ++ digit

let rec string s =
  match explode s with
  | [] -> return ""
  | x :: xs ->
      char x >>= fun _ ->
      string (implode xs) >>= fun _ -> return (implode (x :: xs))

let rec many : 'a t -> 'a list t =
 fun p ->
  ( p >>= fun x ->
    many p >>= fun xs -> return (x :: xs) )
  ++ return []

let ident =
  lower >>= fun x ->
  many alphanum >>= fun xs -> return (implode (x :: xs))

let many1 p =
  p >>= fun x ->
  many p >>= fun xs -> return (x :: xs)

let nat = many1 digit >>= fun n -> return (int_of_string (implode n))

let int =
  ( char '-' >>= fun _ ->
    nat >>= fun n -> return (-n) )
  ++ nat

let sepby1 p sep =
  p >>= fun x ->
  many (sep >>= fun _ -> p) >>= fun xs -> return (x :: xs)

let ints =
  char '[' >>= fun _ ->
  sepby1 int (char ',') >>= fun ns ->
  char ']' >>= fun _ -> return ns

let bracket opn p close =
  opn >>= fun _ ->
  p >>= fun ns ->
  close >>= fun _ -> return ns

let test () =
  let t = nat ((1, 0), explode "0123") in
  t
