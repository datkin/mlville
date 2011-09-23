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
        Option.value_map upper ~default:false ~f:(fun upper -> upper < n)
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
  type ('a, 'b) frame =
    | Z_Eof of ('b -> 'b)
    | Z_One of 'a list * ('b -> 'a -> 'b)
    | Z_Seq of side * ('a, 'b) parser * ('a, 'b) parser
    | Z_Alt of side * ('a, 'b) parser * ('a, 'b) parser * 'b
    | Z_Bound of ('a, 'b) parser * int * int * int option * 'b

  (* TODO: Could Identity be replaced with physical equality on stream and
   * parent? *)
  type ('a, 'b) stack = {
    id: ('a, 'b) Identity.t;
    parent: ('a, 'b) stack option; (* spine *)
    frame: ('a, 'b) frame;
    stream: 'a stream;
  }

  (* NB: 'parser' is not one-to-one with 'stack'. It's possible to create a stack
   * that doesn't correspond to any parser. It would be nice if we could guarantee
   * that *any* stack can be derived (back) to a parser. *)

  let to_frame rule result =
    match rule with
      | Eof f -> Z_Eof f
      | One (xs, f) -> Z_One (xs, f)
      | Seq (l, r) -> Z_Seq (Left, l, r)
      | Alt (l, r) -> Z_Alt (Left, l, r, result)
      | Bound (p, lower, upper) -> Z_Bound (p, 0, lower, upper, result)
  ;;

  let to_stack rule stream result =
    let id = Identity.of_parser rule in
    let parent = None in
    let frame = to_frame rule result in
    {id; frame; parent; stream}
  ;;

  let rec tail_parse ~result ({id; parent; frame; stream} as stack) =
    match frame with
      | Z_Eof f -> begin
        match stream with
          | [] -> unwind parent (Success (f result, []))
          | _  -> unwind parent Failure
      end
      | Z_One (xs, f) -> begin
        match stream with
          | x :: stream' ->
            if List.mem x ~set:xs then
              unwind parent (Success ((f result x), stream'))
            else
              unwind parent Failure
          | [] -> unwind parent Failure
      end
      | Z_Seq (side, l, r) ->
        let p = match side with
          | Left -> l
          | Right -> r
        in
        tail_parse ~result {
          id = Identity.of_parser p;
          parent = Some stack;
          frame = to_frame p result;
          stream = stream
        }
      | Z_Alt (side, l, r, saved) ->
        let p = match side with
          | Left -> l
          | Right -> r
        in
        tail_parse ~result {
          id = Identity.of_parser p;
          parent = Some stack;
          frame = to_frame p result;
          stream = stream;
        }
      | Z_Bound (p, n, lower, upper, saved) ->
        tail_parse ~result {
          id = Identity.of_parser p;
          parent = Some stack;
          frame = to_frame p result;
          stream = stream;
        }
  and unwind (* aka 'return' *) parent current_result =
    match parent with
      | None -> current_result
        (* TODO: may be able to move 'saved_result' into the frames where it's needed? *)
      | Some ({id; frame; parent = grand_parent; stream = saved_stream} as stack) ->
        begin match frame, current_result with
          | Z_Eof _, _ -> assert false
          | Z_One _, _ -> assert false

          | Z_Seq (Left, l, r),  Success (b, s) ->
            tail_parse ~result:b {
              id = Identity.of_parser r;
              parent = grand_parent;
              frame = Z_Seq (Right, l, r);
              stream = s
            }
          | Z_Seq (Right, l, r), Success _ -> unwind grand_parent current_result
          | Z_Seq _,             Failure   -> unwind grand_parent current_result

          | Z_Alt (Left, l, r, res),  Success _ -> unwind grand_parent current_result
          | Z_Alt (Left, l, r, saved),  Failure   ->
            tail_parse ~result:saved {
              id = Identity.of_parser r;
              parent = grand_parent;
              frame = Z_Alt (Right, l, r, saved);
              stream = saved_stream;
            }
          | Z_Alt (Right, l, r, res), _         -> unwind grand_parent current_result

          | Z_Bound (p, n, lower, upper, saved), Success (result, stream) ->
            let continue () =
              let frame = Z_Bound (p, n+1, lower, upper, result) in
              tail_parse ~result {stack with frame = frame; stream = stream}
            in
            begin match upper with
              | None -> continue ()
              | Some m ->
                if n = m then (* assumes n >= lower *)
                  unwind grand_parent current_result
                else continue ()
            end
          | Z_Bound (p, n, lower, upper, saved), Failure ->
            if n >= lower then
              unwind grand_parent (Success (saved, saved_stream))
            else unwind grand_parent current_result
        end

  let apply rule str =
    let init = [] in
    let stream = String.to_list str in
    tail_parse ~result:init (to_stack rule stream init)
end
