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

/* Represents one square on the board */
module Tile {
  let component = ReasonReact.statelessComponent("Tile");

  let make = (~tile: Data.square, _children) => {
    ...component,
    render: (_self) =>
      <span>(str(Control.squareToStr(tile)))</span>
  }
};

/* Represents the game board */
module Room {
  let component = ReasonReact.statelessComponent("Room");

  let make = (~room: Data.room, _children) => {
    ...component,
    render: (_self) =>
      <table className={"room"}>
        {
          room |> List.map((row: list(Data.square)) => {
          <tr>
            {
              row |> List.map((tile: Data.square) => {
                <td><Tile tile></Tile></td>
              })
              |> Array.of_list |> ReasonReact.arrayToElement
            }
          </tr>
          })
          |> Array.of_list |> ReasonReact.arrayToElement
        }
      </table>
  }
};

/* App */

let component = ReasonReact.statelessComponent("App");

let make = (_children) => {
  ...component,
  render: (_self) =>
    <div>
      <h1>(str("CamlHack"))</h1>
      <Room room={Control.initialState |> Control.stateToRoom} />
    </div>
}
