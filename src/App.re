let str = ReasonReact.stringToElement;

let valueFromEvent = (evt: ReactEventRe.Form.t) : string => (
                                                               evt
                                                               |> ReactEventRe.Form.target
                                                               |> ReactDOMRe.domElementToObj
                                                             )##value;

/* Represents one square on the board */
module Tile = {
  let component = ReasonReact.statelessComponent("Tile");

  let make = (~tile: Data.square, _children) => {
    ...component,
    render: _self => <span> (str(Control.squareToStr(tile))) </span>,
  };
};

/* Represents the game board */
module Room = {
  let component = ReasonReact.statelessComponent("Room");

  let make = (~room: Data.room, _children) => {
    ...component,
    render: _self =>
      <table className="room">
        <tbody>
          (
            room
            |> List.map((row: list(Data.square)) =>
                 <tr>
                   (
                     row
                     |> List.map((tile: Data.square) =>
                          <td> <Tile tile /> </td>
                        )
                     |> Array.of_list
                     |> ReasonReact.arrayToElement
                   )
                 </tr>
               )
            |> Array.of_list
            |> ReasonReact.arrayToElement
          )
        </tbody>
      </table>,
  };
};

/* App */

[@bs.val] external window : Dom.window = "";
[@bs.send]
external addEventListener : (Dom.window, string, 'a => unit, bool) => unit =
  "";

type state = Data.state;
let component = ReasonReact.reducerComponent("App");

let make = _children => {
  let keyToAction = (key: string) : option(Data.action) =>
    switch (key) {
    | "w" => Some(Data.Move(Data.Up))
    | "d" => Some(Data.Move(Data.Right))
    | "s" => Some(Data.Move(Data.Down))
    | "a" => Some(Data.Move(Data.Left))
    | _ => None
    };
  {
    ...component,
    initialState: () => Control.initialState,
    didMount: ({reduce}) => {
      let keyPressHandler = evt : unit => {
        let key = ReactEventRe.Keyboard.key(evt);
        switch (keyToAction(key)) {
        | Some(action) => reduce(() => action, ()) /* TODO: New API is self.send */
        | None => ()
        };
      };
      /* Call reducer with action defined by keypress */
      addEventListener(window, "keypress", keyPressHandler, false);
      ReasonReact.NoUpdate;
    },
    reducer: (action, state) =>
      ReasonReact.Update(Control.nextTurn(action, state)),
    render: ({state}) =>
      <div className="game">
        <h1> (str("ReasonHack")) </h1>
        <Room room=(state |> Control.stateToRoom) />
        <p>
          (
            str(
              "Use the WASD keys to move. Move into a monster to attack. Refresh for a new game.",
            )
          )
        </p>
        <p>
          (str("Created by Alex Mueller. Check it out on "))
          <a href="https://github.com/lemonpkt/ReasonHack">
            (str("GitHub"))
          </a>
          (str("."))
        </p>
      </div>,
  };
};