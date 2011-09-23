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

(* TODO: better names for Success/Fail result and accumulator result *)
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
    | Z_Alt of ('a, 'b) parser * 'b * 'a stream
    | Z_Bound of ('a, 'b) parser * int * int * int option * 'b * 'a stream

  type ('a, 'b) stack =
    | Top
    | Frame of ('a, 'b) stack * ('a, 'b) context

  let rec tail_parse ~result ~stream rule stack =
    match rule with
      | Eof f -> begin
        match stream with
          | [] -> unwind stack (Success (f result, []))
          | _  -> unwind stack Failure
      end
      | One (xs, f) -> begin
        match stream with
          | x :: stream' ->
            if List.mem x ~set:xs then
              unwind stack (Success ((f result x), stream'))
            else
              unwind stack Failure
          | [] -> unwind stack Failure
      end
      | Seq (left, right) ->
        tail_parse ~result ~stream left (Frame (stack, Z_Seq right))
      | Alt (left, right) ->
        let context = Z_Alt (right, result, stream) in
        tail_parse ~result ~stream left (Frame (stack, context))
      | Bound (rule, lower, upper) ->
        let context = Z_Bound (rule, 1, lower, upper, result, stream) in
        tail_parse ~result ~stream rule (Frame (stack, context))
  and unwind (* aka 'return' *) stack current_result =
    match stack with
      | Top -> current_result
      | Frame (parent, context) ->
        begin match context, current_result with
          | Z_Seq right, Success (result, stream) ->
            tail_parse ~result ~stream right parent
          | Z_Seq _, Failure ->
            unwind parent current_result

          | Z_Alt (_right, _result, _stream), Success _ ->
            unwind parent current_result
          | Z_Alt (right, result, stream), Failure ->
            tail_parse ~result ~stream right parent

          | Z_Bound (rule, n, lower, upper, _result, _stream), Success (result, stream) ->
            let continue () =
              let context = Z_Bound (rule, n+1, lower, upper, result, stream) in
              tail_parse ~result ~stream rule (Frame (parent, context))
            in
            begin match upper with
              | None -> continue ()
              | Some m ->
                if n < m then continue ()
                else unwind parent current_result
            end
          | Z_Bound (_rule, n, lower, _upper, result, stream), Failure ->
            if n > lower then
              unwind parent (Success (result, stream))
            else unwind parent current_result
        end

  let apply rule str =
    let init = "" in
    let stream = String.to_list str in
    tail_parse ~result:init ~stream rule Top
end
