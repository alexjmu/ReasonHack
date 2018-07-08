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
  mobs: [((2, 3), Data.slime), ((3, 3), Data.slime)],
  size: (10, 10)
};

let getMobAtLoc = (loc: Data.location, mobs: list((Data.location, Data.mob))) : option(Data.mob) => {
  switch(find_opt(((l, m)) => l == loc, mobs)) {
    | Some((loc, mob)) => Some(mob)
    | None => None
  }
};

let moveMobs = ({mobs} as state: Data.state) : Data.state => {
  {...state,
    mobs: mobs |> List.map(
      (((x, y): Data.location, mob)) => ((x + 1, y), mob))
  }
};

let clearCorpses = ({mobs} as state: Data.state) : Data.state => {
  {...state,
    mobs: mobs |> List.filter(((_, mob: Data.mob)) => mob.health > 0)
  }
};

let attack = (state: Data.state, mobLoc: Data.location, mob: Data.mob) : Data.state => {
  /* TODO: You get the mob via the state here so you don't really need to
  have `mob` as an argument... */
  let attackedMob = {
    ...mob,
    health: mob.health - state.player.weapon.damage
  };
  {...state,
    mobs:
      /* Replace mob */
      List.map(((l: Data.location, m: Data.mob)) => {
        if (l == mobLoc) {
          (l, attackedMob)
        }
        else {
          (l, m)
        }
      }, state.mobs)
  }
};

/* Compute next state given current state */
let playerTurn = (action: Data.action, state: Data.state) : Data.state => switch(action) {
  | Move(dir) => {
    let (x, y) = state.loc;
    {...state, loc: switch(dir) {
      | Left => (x, y - 1)
      | Right => (x, y + 1)
      | Up => (x - 1, y)
      | Down => (x + 1, y)
    }}
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

/* String representation of game room */
let stateToRoom = ({size: (width, height)} as state: Data.state) : Data.room => {
  range(0, width) |> List.map((row) => {
    range(0, height) |> List.map((col) => {
      if ((row, col) == state.loc) {
        Data.Player
      }
      else {
        /* TODO: state.mobs should be a map[location, mob] */
        state.mobs |> List.fold_left((cur, (loc, mob)) => {
          if (cur == Data.Empty && loc == (row, col)) {
            Data.Mob(mob)
          }
          else {
            cur
          }
        }, Data.Empty)
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
4. Allow player interaction by clicking highlighted tiles -> give callback function to Tile
5. Prevent the player or mobs from moving off the screen
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
