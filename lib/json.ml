open Monad
open Monad.StateM

type json =
  | Jobj of json_obj
  | Jarr of json_arr
  | Jnum of float
  | Jstr of string
  | Jbool of bool
  | Jnull

and json_obj = (string * json) list
and json_arr = json list

let rec show_json json =
  match json with
  | Jobj obj -> Printf.sprintf "{\n%s\n}" (show_json_obj obj)
  | Jarr arr -> Printf.sprintf "[%s]" (show_json_arr arr)
  | Jnum num -> string_of_float num
  | Jstr str -> str
  | Jbool b -> string_of_bool b
  | Jnull -> "null"

and show_json_obj json_obj =
  json_obj
  |> List.map (fun (k, v) -> k ^ ": " ^ show_json v)
  |> String.concat ",\n"

and show_json_arr json_arr =
  json_arr |> List.map show_json |> String.concat ", "

let except_char c = sat (fun x -> x != c)

let json_str =
  char '"' >>= fun _ ->
  many (except_char '"') >>= fun k ->
  char '"' >>= fun _ -> return (implode k)

let json_bool =
  (string "true" >>= fun _ -> return true)
  ++ (string "false" >>= fun _ -> return false)

let rec json : json parser =
 fun s ->
  let p =
    (json_obj >>= fun obj -> return (Jobj obj))
    ++ (json_arr >>= fun arr -> return (Jarr arr))
    ++ (int >>= fun i -> return (Jnum (float_of_int i)))
    ++ (json_str >>= fun s -> return (Jstr s))
    ++ (json_bool >>= fun b -> return (Jbool b))
    ++ (string "null" >>= fun _ -> return Jnull)
  in
  p s

and json_obj : json_obj parser =
 fun s ->
  let p =
    char '{' >>= fun _ ->
    sepby1
      ( json_str >>= fun k ->
        char ':' >>= fun _ ->
        json >>= fun obj -> return (k, obj) )
      (char ',')
    >>= fun l ->
    char '}' >>= fun _ -> return l
  in
  p s

and json_arr : json_arr parser =
 fun s ->
  let p =
    char '[' >>= fun _ ->
    sepby1 json (char ',') >>= fun l ->
    char ']' >>= fun _ -> return l
  in
  p s

let json_test () =
  let j = json ((1, 0), explode "[{\"a\":1,\"b\":2}]") in
  match j with Ok (json, _) -> print_endline (show_json json) | Error _ -> ()
