[@bs.deriving abstract]
type jsProps = {
  /* some example fields */
  vertexSize: int,

};

[@bs.module "@sabaki/shudan"] external goban : ReasonReact.reactClass = "Goban";
/* [@bs.module] external goban: { Goban: ReasonReact.reactClass } = "@sabaki/shudan"; */

let make = (children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=goban,
    ~props=Js.Obj.empty(),
    children
  );
