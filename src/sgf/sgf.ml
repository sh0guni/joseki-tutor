(* 
  Collection = GameTree+
  GameTree   = '(' Sequence GameTree* ')'
  Sequence   = Node+
  Node       = ';' Property*
  Property   = PropIdent PropValue+
  PropIdent  = UcLetter+
  PropValue  = '[' CValueType ']'
  CValueType = (ValueType | Compose)
  ValueType  = (None | Number | Real | Double | Color | SimpleText | Text | Point  | Move | Stone)
  UcLetter   = 'A'..'Z'
  Compose    = ValueType ':' ValueType
*)

open Belt_MapString

type propertyMap = string list t

type tree = Node of propertyMap * tree list

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

type token =
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | SEMICOLON
  | IDENTIFIER of string
  | INVALID of char

let rec scanIdentifier = function
  | 'A'..'Z' | 'a'..'z' as c :: xs -> let (id, xs) = scanIdentifier xs in (String.make 1 c ^ id, xs)
  | xs -> ("", xs)

let rec scan = function 
  | [] -> []
  | c :: xs as tokens -> match c with
    | '(' -> LEFT_BRACE :: scan xs
    | ')' -> RIGHT_BRACE :: scan xs
    | ']' -> RIGHT_BRACKET :: scan xs
    | '[' -> LEFT_BRACKET :: scan xs
    | ';' -> SEMICOLON :: scan xs
    | 'A'..'Z' | 'a'..'z' -> let (s, xs) = scanIdentifier tokens in IDENTIFIER(s) :: scan xs
    | _ -> INVALID c :: []

let debugToken = function
  | LEFT_BRACE -> "("
  | RIGHT_BRACE -> ")"
  | LEFT_BRACKET -> "["
  | RIGHT_BRACKET -> "]"
  | SEMICOLON -> ";"
  | IDENTIFIER x -> "ID " ^ x
  | INVALID c -> "INVALID " ^ (String.make 1 c)

let debug f x = 
  Js.log("Debug");
  Js.log(List.map debugToken x);
  let x = f @@ x in
    Js.log x;
    x

let rec parseValue = function
  | LEFT_BRACKET :: IDENTIFIER value :: RIGHT_BRACKET :: xs ->
    (match parseValue xs with
      | None -> Some([value], xs)
      | Some (value2, xs) -> Some(value :: value2, xs))
  | _ -> None

let parseProperty = function
  | IDENTIFIER name :: xs ->
    if String.uppercase name == name 
    then
      match (debug parseValue) xs with
        | None -> None
        | Some (value, xs) -> Some([name, value;], xs)
    else
      (Js.log ("Not uppercase" ^ name); None)
  | _ -> None

let parseProperties tokens =
  match parseProperty tokens with
    | None -> None
    | Some (p, xs) ->
      (match parseProperty xs with
        | Some(p2, xs) -> Some (p @ p2, xs)
        | None -> Some (p, xs))

let parseNode = function
  | SEMICOLON :: xs -> 
    (match parseProperty xs with
      | None -> Some(empty, xs)
      | Some (props, xs) -> Some (fromArray(Array.of_list props), xs))
  | _ -> None

let rec parseSequence sequence = match parseNode sequence with
  | None -> None
  | Some (prop, xs) -> match parseSequence xs with
    | Some (seq, xs) -> Some (Node (prop, [seq]), xs)
    | None -> match parseTree xs with
      | None -> Some (Node (prop, []), xs)
      | Some (tree, xs) -> Some (Node (prop, tree), xs)

and parseTree = function
  | LEFT_BRACE :: xs ->
      (match parseSequence xs with
        | Some (sequence, RIGHT_BRACE :: xs) -> 
          (match parseTree xs with
           | None -> Some ([sequence], xs)
           | Some (tree, xs) -> Some(sequence :: tree, xs))
        | _ -> None)
  | _ -> None

let parseCollection tokens =
  match parseTree tokens with
    | None -> None
    | x -> x

let tokenize chars =
  match scan chars with
  | [] -> None
  | tokens ->
    match List.nth tokens (List.length tokens - 1) with
    | INVALID c ->
      Js.log(Printf.sprintf "Invalid character %c at position %d" c (List.length tokens));
      None
    | _ -> Some tokens


let parse (sgf: string): tree option = 
  match tokenize @@ explode sgf with 
  | None -> None
  | Some tokens ->
    Js.log("Tokens");
    Js.log(List.map debugToken tokens);
    match parseTree tokens with
      | Some(tree, _) -> Some (List.hd tree)
      | _ -> None
