let component = ReasonReact.statelessComponent("Board");

let make = (_children) => {
...component,
  render: _self =>
  <div>
    <Goban />
  </div>,
};
