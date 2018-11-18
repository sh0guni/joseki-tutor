open Jest
open Sgf

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

  test "Node without properties" (fun() ->
    let expected = Some (Node (Map.ofList [], [])) in
    expect (parse "(;)") |> toEqual expected
  );

  test "Single node tree" (fun() ->
    let expected = Some (Node (Map.ofList [("A", ["B"])], [])) in
    expect (parse "(;A[B])") |> toEqual expected
  );

  test "Properties without delimiter" (fun() ->
    let expected = None in
    expect (parse "(;A)") |> toEqual expected
  );

  test "All lowercase property" (fun() ->
    let expected = None in
    expect (parse "(;a[b])") |> toEqual expected
  );

  test "Upper and lowercase property" (fun() ->
    let expected = None in
    expect (parse "(;Aa[b])") |> toEqual expected
  );

  test "Two nodes" (fun() ->
    let expected = Some (Node (Map.ofList [("A", ["B"])], [Node (Map.ofList [("B", ["C"])], [])])) in
    expect (parse "(;A[B];B[C])") |> toEqual expected
  );

  test "Two child trees" (fun() ->
    let expected = Some (Node (Map.ofList [("A", ["B"])], [Node (Map.ofList [("B", ["C"])], []); Node (Map.ofList [("C", ["D"])], [])])) in
    expect (parse "(;A[B](;B[C])(;C[D]))") |> toEqual expected
  );

  test "Multiple property values" (fun() ->
    let expected = Some (Node (Map.ofList [("A", ["b"; "c"; "d"])], [])) in
    expect (parse "(;A[b][c][d])") |> toEqual expected
  );

  test "Escaped property" (fun() ->
    let expected = Some (Node (Map.ofList [("A", ["]b\nc\nd  e \n]"])], [])) in
    expect (parse "(;A[\]b\nc\nd\t\te \n\]])") |> toEqual expected
  );
);
