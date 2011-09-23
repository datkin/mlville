(* Parsers are a pain. Tools like Yacc present the user an ugly pre-processing
 * language, demand understanding and accounting for idiosyncrasies of the
 * implementation (i.e. shift/reduce conflicts) for anything beyond trivial
 * grammars, but yield efficient code. More elegant tools like parser libraries
 * usually suffer high performance costs.
 *
 * I eventually intend to implement a robust and performant MLville compiler in
 * MLville itself. However, for the bootstrap compiler, performance is secondary
 * to simplicity and clarity (of course, reasonable performance is appreciated).
 * Thus, the aim here is to provide a small library for defining and parsing
 * grammars that distances the users from the usual -- painful -- extremes of
 * parsing: tedious grammar tweaking, and awful performance. The design and
 * implementation of this library should inform the design of its eventual
 * MLville counterpart, and has already provided insight to desirable language
 * features.
 *
 * Parsers are represented by an Algebraic Data Type, allowing for analysis to
 * facility both simplicity (e.g. for left recursion support) and performance
 * optimizations. Constructing parsers with combinators hides structural details
 * inside opaque structures (functions) and eliminates the possibility for any
 * optimization outside the compiler itself. The parser type defined below is
 * extremely simple. It may be useful to provide combinator-style shorthands as
 * a convenient front-end for use when defining parsers. It may also be useful
 * to implement a more complex back-end data type for optimizing parsers.
 *)
open Core.Std;;

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

let rec parse rule result stream =
  match rule with
  | Eof f ->
      if List.is_empty stream then Success (f result, []) else Failure
  | One (xs, f) -> begin
    match stream with
    | x :: stream' ->
        if List.mem x ~set:xs then
          Success (f result x, stream')
        else Failure
    | [] -> Failure
  end
  | Seq (p1, p2) -> begin
    match parse p1 result stream with
    | Success (result', stream') -> parse p2 result' stream'
    | fail -> fail
  end
  | Alt (p1, p2) -> begin
    match parse p1 result stream with
    | Failure -> parse p2 result stream
    | pass -> pass
  end
  | Bound (p, lower, upper) ->
    let rec iter result stream n =
      let over =
        Option.value_map upper ~default:false ~f:(fun upper -> upper <= n)
      in
      if over then
        Success (result, stream)
      else match parse p result stream with
      | Failure -> if n < lower then Failure else Success (result, stream)
      | Success (result', stream') -> iter result' stream' (n+1)
    in
    iter result stream 0
;;

let apply rule str =
  parse rule "" (String.to_list str)
;;

(* TODO: what to do about this. *)
module Identity : sig
  type ('a, 'b) t
  val of_parser: ('a, 'b) parser -> ('a, 'b) t
  val equal: ('a, 'b) t -> ('a, 'b) t -> bool
  val hash: ('a, 'b) t -> int
end = struct
  type ('a, 'b) t = ('a, 'b) parser
  let of_parser p = p
  let equal = phys_equal
  let hash _ = 0 (* TODO *)
end

(* A tail recursive parser forces us to reify the stack which will be important
 * for handling left-recursion, and may also facilitate other optimizations.
 *
 * A 'parser' may be a cyclic-graph, and represents a control-flow graph
 * for parsing. Then, 'stack' captures the actual call tree and represents it
 * similarly to a Huet zipper: unzipping the call graph to produce a call tree
 * above and a call graph below.
 *)
module Tail = struct
  type side =
    | Left
    | Right

  (* 'Z' for zipper. Perhaps 'F' would be better? *)
  type ('a, 'b) context =
    | Z_Seq of ('a, 'b) parser
    | Z_Alt of ('a, 'b) parser * 'b
    | Z_Bound of ('a, 'b) parser * int * int * int option * 'b

  (* TODO: These names are awful. *)
  type ('a, 'b) stack =
    | Top
    | Frame of ('a, 'b) frame * ('a, 'b) context
  and ('a, 'b) frame = {
    parent: ('a, 'b) stack;
    stream: 'a stream
  }

  let rec tail_parse ~result rule ({parent; stream} as current_frame) =
    match rule with
      | Eof f -> begin
        match stream with
          | [] -> unwind parent (Success (f result, []))
          | _  -> unwind parent Failure
      end
      | One (xs, f) -> begin
        match stream with
          | x :: stream' ->
            if List.mem x ~set:xs then
              unwind parent (Success ((f result x), stream'))
            else
              unwind parent Failure
          | [] -> unwind parent Failure
      end
      | Seq (left, right) ->
          let new_frame =
            { parent = Frame (current_frame, Z_Seq right)
            ; stream = stream }
          in
          tail_parse ~result left new_frame
      | Alt (left, right) ->
          let new_frame =
            { parent = Frame (current_frame, Z_Alt (right, result))
            ; stream = stream }
          in
        tail_parse ~result left new_frame
      | Bound (p, lower, upper) ->
          let new_frame =
            { parent = Frame (current_frame, Z_Bound (p, 0, lower, upper, result))
            ; stream = stream }
          in
        tail_parse ~result p new_frame
  and unwind (* aka 'return' *) stack current_result =
    match stack with
      | Top -> current_result
      | Frame (({parent; stream = saved_stream} as frame), context) ->
        begin match context, current_result with
          | Z_Seq right,  Success (result, stream) ->
            let new_frame =
              { parent = parent
              ; stream = stream }
            in
            tail_parse ~result right new_frame
          | Z_Seq _, Failure   -> unwind parent current_result

          | Z_Alt (_right, _saved), Success _ -> unwind parent current_result
          | Z_Alt (right, saved), Failure ->
            let new_frame =
              { parent = parent
              ; stream = saved_stream }
            in
            tail_parse ~result:saved right new_frame

          | Z_Bound (p, n, lower, upper, saved), Success (result, stream) ->
            let continue () =
              let new_frame =
                { parent = Frame (frame, Z_Bound (p, n+1, lower, upper, result))
                ; stream = stream }
              in tail_parse ~result p new_frame
            in
            begin match upper with
              | None -> continue ()
              | Some m ->
                if n = m then (* assumes n >= lower *)
                  unwind parent current_result
                else continue ()
            end
          | Z_Bound (p, n, lower, upper, saved), Failure ->
            if n >= lower then
              unwind parent (Success (saved, saved_stream))
            else unwind parent current_result
        end

  let apply rule str =
    let init = "" in
    let stream = String.to_list str in
    tail_parse ~result:init rule {parent = Top; stream}
end
