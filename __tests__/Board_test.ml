open Jest
open Sgf
(* open Belt_MapString *)

let () =

describe "SGF parsing" (fun() ->
  let open Expect in

  test "Empty input" (fun() ->
    let expected = None in
    expect (parse "") |>  toEqual expected
  );
  test "Tree with no nodes" (fun() ->
    let expected = None in
    expect (parse "()") |> toEqual expected
  );

  test "Node without tree" (fun() ->
    let expected = None in
    expect (parse ";") |> toEqual expected
  );

);
