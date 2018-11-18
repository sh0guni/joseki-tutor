[@bs.deriving abstract]
type jsProps = {
  /* some example fields */
  vertexSize: int,
  signMap: array(array(int))
};

[@bs.module "@sabaki/shudan"] external goban : ReasonReact.reactClass = "Goban";

let make = (~signMap, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=goban,
    ~props=jsProps(
      ~vertexSize=24,
      ~signMap,
    ),
    children
  );
