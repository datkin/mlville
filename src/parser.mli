type 'a stream = 'a list

type ('a, 'b) result =
  | Success of 'b * 'a stream
  | Failure

type ('a, 'b) parser =
  | Eof of ('b -> 'b)
  (* | Always of ('b -> 'b) *)
  | One of 'a list * ('b -> 'a -> 'b)
  | Seq of ('a, 'b) parser * ('a, 'b) parser
  | Alt of ('a, 'b) parser * ('a, 'b) parser
  (* Bounded repetitions, required lower bound, optional upper bound. *)
  | Bound of ('a, 'b) parser * int * int option
;;

val apply: (char, string) parser -> string -> (char, string) result

module Tail : sig
  val apply: (char, string) parser -> string -> (char, string) result
end
