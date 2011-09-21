open Core.Std;;

let as_and_bs =
  let push xs x = x :: xs in
  Parser.(Bound (Alt (One (['a'], push), One (['b'], push)), 2, Some 5))
;;

let char_range first last =
  let base = Char.to_int first in
  let limit = (Char.to_int last) - base in
  List.init (limit+1) ~f:(fun n -> Char.of_int_exn (base + n))
;;

let digits = char_range '0' '9';;
let chars = (char_range '0' '9') @ (char_range 'A' 'Z') @ (char_range 'a' 'z')

let quote = Parser.One (['\''], fun x _ -> x);;

let char_lit = Parser.(
  Seq (quote,
       Seq (One (chars, (fun _ char -> Char.to_string char)),
            quote)))
;;

let () =
  match Parser.Tail.apply as_and_bs "aba" with
  | Parser.Success (chars, _remainder) ->
      let chars = List.map chars ~f:String.of_char in
      printf "Success: %s\n%!" (String.concat ~sep:"," chars)
  | Parser.Failure ->
      printf "Failure\n%!"
