let str = ReasonReact.stringToElement;

let valueFromEvent = (evt: ReactEventRe.Form.t) : string => (
  evt
  |> ReactEventRe.Form.target
  |> ReactDOMRe.domElementToObj
)##value;

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

[@bs.val] external window : Dom.window = "";
[@bs.send] external addEventListener : (Dom.window, string, ('a => unit)) => unit = "";

type state = Data.state;
let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  let keyToAction = (key: string) : option(Data.action) => {
    switch(key) {
      | "ArrowUp" => Some(Data.Move(Data.Up))
      | "ArrowRight" => Some(Data.Move(Data.Right))
      | "ArrowDown" => Some(Data.Move(Data.Down))
      | "ArrowLeft" => Some(Data.Move(Data.Left))
      | "a" => {Js.log("Attack!"); Some(Attack)}
      | _ => None
    };
  };
  {
    ...component,
    initialState: () => Control.initialState,
    didMount: ({reduce}) => {
      let keyPressHandler = (evt) : unit => {
        let key = ReactEventRe.Keyboard.key(evt);
        switch(keyToAction(key)) {
          | Some(action) => reduce(() => action)() /* TODO: New API is self.send */
          | None => ()
        }
      };
      /* Call reducer with action defined by keypress */
      addEventListener(window, "keypress", keyPressHandler);
      ReasonReact.NoUpdate;
    },
    reducer: (action, state) => ReasonReact.Update(Control.nextTurn(action, state)),
    render: ({state}) =>
      <div className={"game"}>
        <h1>(str("AltHack"))</h1>
        <Room room={state |> Control.stateToRoom} />
        <p>(str("Use the arrow keys to move. Press 'a' to attack the adjacent square to the right. Refresh for a new game."))</p>
      </div>
  }
}
