open Core.Std;;
open OUnit;;

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

let append_char str c = String.concat [str; String.of_char c];;

let parser_tests prefix p = Parser.(
  let t name = sprintf "%s - %s" prefix name in
  [ (t "eof") >:: (fun () ->
    let eof_rule = Eof ident in
    assert_equal (Success ("", [])) (p eof_rule "");
    assert_equal Failure (p eof_rule "a")
  ); (t "one") >:: (fun () ->
    let one_rule = One (['a'; 'b'], append_char) in
    assert_equal (Success ("a", [])) (p one_rule "a");
    assert_equal (Success ("b", [])) (p one_rule "b");
    assert_equal (Success ("b", ['c'])) (p one_rule "bc");
    assert_equal Failure (p one_rule "");
    assert_equal Failure (p one_rule "c");
    assert_equal Failure (p one_rule "cb")
  ); (t "seq") >:: (fun () ->
    let seq_rule = Seq (One (['a'], append_char), One (['b'], append_char)) in
    assert_equal (Success ("ab", [])) (p seq_rule "ab");
    assert_equal (Success ("ab", ['c'])) (p seq_rule "abc");
    assert_equal Failure (p seq_rule "");
    assert_equal Failure (p seq_rule "a");
    assert_equal Failure (p seq_rule "b");
    assert_equal Failure (p seq_rule "acb")
  ); (t "alt") >:: (fun () ->
    let alt_rule = Alt (One (['a'], append_char), One (['b'], append_char)) in
    assert_equal (Success ("a", [])) (p alt_rule "a");
    assert_equal (Success ("b", [])) (p alt_rule "b");
    assert_equal (Success ("a", ['b'])) (p alt_rule "ab");
    assert_equal (Success ("b", ['a'])) (p alt_rule "ba");
    assert_equal Failure (p alt_rule "c");
    assert_equal Failure (p alt_rule "ca")
  ); (t "bound") >:: (fun () ->
    let bound_rule = Bound (One (['a'], append_char), 0, None) in
    assert_equal (Success ("", [])) (p bound_rule "");
    assert_equal (Success ("a", [])) (p bound_rule "a");
    assert_equal (Success ("aa", [])) (p bound_rule "aa");
    assert_equal (Success ("aaa", [])) (p bound_rule "aaa");
    assert_equal (Success ("aaaa", [])) (p bound_rule "aaaa");
    assert_equal (Success ("aaaaa", [])) (p bound_rule "aaaaa");
    assert_equal (Success ("aaaaaa", [])) (p bound_rule "aaaaaa")
  ); (t "bound (with lower and upper)") >:: (fun () ->
    let bound_rule = Bound (One (['a'], append_char), 2, Some 5) in
    assert_equal Failure (p bound_rule "");
    assert_equal Failure (p bound_rule "a");
    assert_equal (Success ("aa", [])) (p bound_rule "aa");
    assert_equal (Success ("aaa", [])) (p bound_rule "aaa");
    assert_equal (Success ("aaaa", [])) (p bound_rule "aaaa");
    assert_equal (Success ("aaaaa", [])) (p bound_rule "aaaaa");
    assert_equal (Success ("aaaaa", ['a'])) (p bound_rule "aaaaaa")
  ); (t "all") >:: (fun () ->
    let a = One (['a'], append_char) in
    let x = One (['x'], append_char) in
    let xx = Seq (x, x) in
    let a_or_xx = Alt (a, xx) in
    let some_a_or_xx = Bound (a_or_xx, 2, Some 4) in
    let all = Seq (some_a_or_xx, (Eof ident)) in
    assert_equal Failure (p all "");
    assert_equal Failure (p all "a");
    assert_equal Failure (p all "xx");
    assert_equal Failure (p all "xxx");
    assert_equal (Success ("aa", [])) (p all "aa");
    assert_equal (Success ("xxxx", [])) (p all "xxxx");
    assert_equal (Success ("aaaa", [])) (p all "aaaa");
    assert_equal (Success ("xxaaxx", [])) (p all "xxaaxx");
    assert_equal (Success ("xxxxxxxx", [])) (p all "xxxxxxxx");
    assert_equal Failure (p all "aaaaa");
    assert_equal Failure (p all "xxxxxxxxa");
  );
  (* TODO: test upper < lower, negative lower, etc? *)
]);;

let tests = "Parser" >::: (parser_tests "simple" Parser.apply) @ (parser_tests "tail" Parser.Tail.apply)

let () =
  ignore (run_test_tt ~verbose:true tests)
