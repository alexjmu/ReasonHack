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
        <tbody>
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
        </tbody>
      </table>
  }
};

/* App */

type state = Data.state;
let component = ReasonReact.reducerComponent("App"); /* TODO */

let make = (_children) => {
  ...component,
  initialState: () => Control.initialState,
  reducer: (action, state) => {
    ReasonReact.Update(
      Control.playerTurn(action, state) /* TODO: |> moveMobs |> clearCorpses */
    )
  },
  render: ({state, reduce}) =>
    <div
      tabIndex=1
      onKeyDown=((evt) => {
        /* TODO: Register this at the window level via JS interop */
        let key = ReactEventRe.Keyboard.key(evt);
        let dir: option(Data.direction) = switch(key) {
          | "ArrowUp" => Some(Data.Up)
          | "ArrowRight" => Some(Data.Right)
          | "ArrowDown" => Some(Data.Down)
          | "ArrowLeft" => Some(Data.Left)
          | _ => None
        };
        switch(dir) {
          | Some(d) => {Js.Console.log("move"); reduce(() => Data.Move(d))()}
          | None => ()
        }
      })
    >
      <h1>(str("CamlHack"))</h1>
      <Room room={state |> Control.stateToRoom} />
    </div>
}
