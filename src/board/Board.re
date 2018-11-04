let component = ReasonReact.statelessComponent("Board");

let make = (_children) => {
...component,
  render: _self =>
  <div>
    <svg width="100" height="100">
      <rect x="0" y="0" width="100" height="100" />
    </svg>
  </div>,
};
