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

let parser_tests prefix apply = Parser.(
  let t name = sprintf "%s - %s" prefix name in
  [ (t "eof") >:: (fun () ->
    let eof_rule = Eof ident in
    assert_equal (Success ("", [])) (apply eof_rule "");
    assert_equal Failure (apply eof_rule "a")
  ); (t "one") >:: (fun () ->
    let one_rule = One (['a'; 'b'], append_char) in
    assert_equal (Success ("a", [])) (apply one_rule "a");
    assert_equal (Success ("b", [])) (apply one_rule "b");
    assert_equal (Success ("b", ['c'])) (apply one_rule "bc");
    assert_equal Failure (apply one_rule "");
    assert_equal Failure (apply one_rule "c");
    assert_equal Failure (apply one_rule "cb")
  ); (t "seq") >:: (fun () ->
    let seq_rule = Seq (One (['a'], append_char), One (['b'], append_char)) in
    assert_equal (Success ("ab", [])) (apply seq_rule "ab");
    assert_equal (Success ("ab", ['c'])) (apply seq_rule "abc");
    assert_equal Failure (apply seq_rule "");
    assert_equal Failure (apply seq_rule "a");
    assert_equal Failure (apply seq_rule "b");
    assert_equal Failure (apply seq_rule "acb")
  ); (t "alt") >:: (fun () ->
    let alt_rule = Alt (One (['a'], append_char), One (['b'], append_char)) in
    assert_equal (Success ("a", [])) (apply alt_rule "a");
    assert_equal (Success ("b", [])) (apply alt_rule "b");
    assert_equal (Success ("a", ['b'])) (apply alt_rule "ab");
    assert_equal (Success ("b", ['a'])) (apply alt_rule "ba");
    assert_equal Failure (apply alt_rule "c");
    assert_equal Failure (apply alt_rule "ca")
  ); (t "bound") >:: (fun () ->
    let bound_rule = Bound (One (['a'], append_char), 0, None) in
    assert_equal (Success ("", [])) (apply bound_rule "");
    assert_equal (Success ("a", [])) (apply bound_rule "a");
    assert_equal (Success ("aa", [])) (apply bound_rule "aa");
    assert_equal (Success ("aaa", [])) (apply bound_rule "aaa");
    assert_equal (Success ("aaaa", [])) (apply bound_rule "aaaa");
    assert_equal (Success ("aaaaa", [])) (apply bound_rule "aaaaa");
    assert_equal (Success ("aaaaaa", [])) (apply bound_rule "aaaaaa")
  ); (t "bound (with lower and upper)") >:: (fun () ->
    let bound_rule = Bound (One (['a'], append_char), 2, Some 5) in
    assert_equal Failure (apply bound_rule "");
    assert_equal Failure (apply bound_rule "a");
    assert_equal (Success ("aa", [])) (apply bound_rule "aa");
    assert_equal (Success ("aaa", [])) (apply bound_rule "aaa");
    assert_equal (Success ("aaaa", [])) (apply bound_rule "aaaa");
    assert_equal (Success ("aaaaa", [])) (apply bound_rule "aaaaa");
    assert_equal (Success ("aaaaa", ['a'])) (apply bound_rule "aaaaaa")
  ); (t "all") >:: (fun () ->
    let a = One (['a'], append_char) in
    let x = One (['x'], append_char) in
    let xx = Seq (x, x) in
    let a_or_xx = Alt (a, xx) in
    let some_a_or_xx = Bound (a_or_xx, 2, Some 4) in
    let all = Seq (some_a_or_xx, (Eof ident)) in
    assert_equal Failure (apply all "");
    assert_equal Failure (apply all "a");
    assert_equal Failure (apply all "xx");
    assert_equal Failure (apply all "xxx");
    assert_equal (Success ("aa", [])) (apply all "aa");
    assert_equal (Success ("xxxx", [])) (apply all "xxxx");
    assert_equal (Success ("aaaa", [])) (apply all "aaaa");
    assert_equal (Success ("xxaaxx", [])) (apply all "xxaaxx");
    assert_equal (Success ("xxxxxxxx", [])) (apply all "xxxxxxxx");
    assert_equal Failure (apply all "aaaaa");
    assert_equal Failure (apply all "xxxxxxxxa");
  );
  (* TODO: test upper < lower, negative lower, etc? *)
]);;

let tests = "Parser" >::: (parser_tests "simple" Parser.apply) @ (parser_tests "tail" Parser.Tail.apply)

let () =
  ignore (run_test_tt ~verbose:true tests)
