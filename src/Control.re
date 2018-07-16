/* utilities */

let rec range = (a: int, b: int) : list(int) => {
  if (a > b) {
    []
  }
  else {
    [a, ...range(a + 1, b)]
  };
};

/* List.find_opt doesn't work for some reason... */
let find_opt = (f: ('a => bool), ls: list('a)) : option('a) => {
  switch(List.find(f, ls)) {
    | x => Some(x)
    | exception Not_found => None
  };
};

let initialState: Data.state = {
  player: {
    health: 10,
    weapon: Data.sword
  },
  loc: (1, 1),
  mobs: Data.LocationMap.(
    empty
    |> add((2, 3), Data.slime)
    |> add((3, 3), Data.slime)
  ),
  size: (10, 10)
};

let getMobAtLoc = (loc: Data.location, mobs: Data.mobStore) : option(Data.mob) => {
  switch(Data.LocationMap.find(loc, mobs)) {
    | mob => Some(mob)
    | exception Not_found => None
  }
};

let isEmpty = (state: Data.state, (x, y) as loc: Data.location) : bool => {
  /* No other mob */
  getMobAtLoc(loc, state.mobs) == None
  /* No player */
  && state.loc != loc
  /* Not off the board */
  && 0 <= x && x <= fst(state.size)
  && 0 <= y && y <= snd(state.size)
};

/* Move every mob one step. Do not move into non-empty squares */
let moveMobs = ({mobs} as state: Data.state) : Data.state => {
  {...state,
    mobs: Data.LocationMap.fold(
      ((x, y): Data.location, mob: Data.mob, newMobs: Data.mobStore) : Data.mobStore => {
        let newLoc = (x + 1, y);
        Data.LocationMap.add({
          if (isEmpty(state, newLoc)) newLoc else (x, y)
        }, mob, newMobs)
      }, mobs, Data.LocationMap.empty)
  }
};

let clearCorpses = ({mobs} as state: Data.state) : Data.state => {
  {...state,
    mobs: mobs |> Data.LocationMap.filter((_, mob: Data.mob) => mob.health > 0)
  }
};

let attack = ({mobs} as state: Data.state, mobLoc: Data.location, mob: Data.mob) : Data.state => {
  /* TODO: You get the mob via the state here so you don't really need to
  have `mob` as an argument... */
  let attackedMob = {
    ...mob,
    health: mob.health - state.player.weapon.damage
  };
  {...state,
    /* Replace mob */
    mobs: Data.LocationMap.add(mobLoc, attackedMob, mobs)
  }
};

/* Compute next state given current state */
let playerTurn = (action: Data.action, state: Data.state) : Data.state => switch(action) {
  | Move(dir) => {
    let (x, y) = state.loc;
    let newLoc = switch(dir) {
      | Left => (x, y - 1)
      | Right => (x, y + 1)
      | Up => (x - 1, y)
      | Down => (x + 1, y)
    };
    if (isEmpty(state, newLoc)) {
      {...state, loc: newLoc}
    }
    else {
      /* Can't move into a non-empty square */
      state
    }
  }
  | Attack => {
    let (x, y) = (0, 1); /* Square to right of player */
    let attackLoc = (fst(state.loc) + x, snd(state.loc) + y);
    switch(getMobAtLoc(attackLoc, state.mobs)) {
      | Some(mob) => attack(state, attackLoc, mob)
      | None => state
    }
  }
};

let nextTurn = (action: Data.action, state: Data.state) : Data.state => {
  state
  |> playerTurn(action)
  /* 60% chance monsters move */
  |> (s) => {if (Js.Math.random() <= 0.6) moveMobs(s) else s}
  |> clearCorpses;
};

/* String representation of game room */
let stateToRoom = ({size: (width, height)} as state: Data.state) : Data.room => {
  /* TODO: Build up the array from state rather than checking for mobs for *every* coordinate */
  range(0, width) |> List.map((row) => {
    range(0, height) |> List.map((col) => {
      if ((row, col) == state.loc) {
        Data.Player
      }
      else {
        switch(Data.LocationMap.find((row, col), state.mobs)) {
          | mob => Data.Mob(mob)
          | exception Not_found => Data.Empty
        }
      }
    })
  })
};

let roomToStr = (r: Data.room) : string => {
  let tileToStr = (t: Data.square) : string => switch(t) {
    | Data.Empty => "."
    | Data.Player => "P"
    | Data.Mob({repr}) => repr
  };
  r |> List.map((row) => {
    row |> List.map(tileToStr) |> String.concat("")
  })
  |> String.concat("\n")
};

let squareToStr = (s: Data.square) : string => switch(s) {
  | Empty => {js|⬛|js}
  | Player => {js|🤺|js}
  | Mob(mob) => mob.repr
}

/* TODO:

1. (Fix and) compile what we have so far [X]
2. View function [X]
2.5: Easiest way to move mobs is to have a list of mobs and their locations as first thought.
BUT, modify "view" to create a "room" based on a list of mobs and locations, for easier printing [X]
2.75: Test moving player, monsters, attacking, defeating monsters [X]
3. Hook up to React app with elements for board display (move all this code to react app!) [X]
~~4. Allow player interaction by clicking highlighted tiles -> give callback function to Tile
5. Prevent the player or mobs from moving off the screen [X]
6. Mob attacks
7. Player death
8. Mob movement AI (BFS!)
9. Randomly generated obstacles on the board

0. Learn about functors/modules in OCaml create a Map[location -> mob] for the state :)

*/

/* README

So, Alex, in case you don't look at this again for quite a while:

The game is represented by a `state` record type, which stores the room dimensions,
the player, the player's location, and a list of (location, mob) pairs.

In case you want to display/draw the current state of the game, `stateToRoom` will
output a list(list(square)) (array of items like empty, monster, player, etc.) which
is easy to convert into a string on the terminal or a html table or whatever.

moveMobs is the current (bad) mob movement AI. Yes, (x + 1, y) is "up" because x is
row index and y is column index.

Every turn, you need to do the following:

1. Get the state of the game
2. Get the player's next action
3. Player's turn: Feed the state to `playerTurn` with the player's action
4. Monster's turn: Call `moveMobs` on the result (and mobsAttack eventually?)
5. Remove any mobs from the map which have been killed via `clearCorpses`
6. Draw/display this new state

state |> playerTurn(action) |> moveMobs |> clearCorpses

User interaction on web could involve the user clicking on a highlighted tile
to move there, or a highlighted mob to attack it.

*/
