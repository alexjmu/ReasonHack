let str = ReasonReact.stringToElement;

let valueFromEvent = (evt: ReactEventRe.Form.t) : string => (
  evt
  |> ReactEventRe.Form.target
  |> ReactDOMRe.domElementToObj
)##value;

let safeTail = (ls: list('a)) : option(list('a)) => switch(ls) {
  | [] => None
  | [_, ...xs] => Some(xs)
};

let defaultOption = (value: option('a), default: 'a) : 'a => switch(value) {
  | Some(x) => x
  | None => default
};

/* App */

let component = ReasonReact.statelessComponent("App");

let make = (_children) => {
  ...component,
  render: (_self) =>
    <div>
      <h1>(str("CamlHack"))</h1>
    </div>
}
