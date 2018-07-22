/* utilities */

let rec range = (a: int, b: int) : list(int) => {
  assert (a <= b);
  if (a >= b) {
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

let getAllCoords = ({size: (height, width)}: Data.state) : list(Data.location) => {
  range(0, height) |> List.fold_left(
    (ls, row) => {
      range(0, width) |> List.map((col) => (row, col)) |> List.append(ls)
    },
    []
  )
};

let getMobAtLoc = (loc: Data.location, mobs: Data.mobStore) : option(Data.mob) => {
  switch(Data.LocationMap.find(loc, mobs)) {
    | mob => Some(mob)
    | exception Not_found => None
  }
};

let getStuckAtLoc = (loc: Data.location, stucks: Data.stuckStore) : option(Data.stuck) => {
  switch(Data.LocationMap.find(loc, stucks)) {
    | stuck => Some(stuck)
    | exception Not_found => None
  }
};

let isEmpty = ({size: (height, width)} as state: Data.state, (x, y) as loc: Data.location) : bool => {
  /* No other mob */
  getMobAtLoc(loc, state.mobs) == None
  /* No stuck */
  && getStuckAtLoc(loc, state.stucks) == None
  /* No player */
  && state.loc != loc
  /* Not off the board */
  && 0 <= x && x < height
  && 0 <= y && y < width
};

let generateRocks = (state: Data.state) : Data.state => {
  let newStucks = {
    let addRock = (coord, rock, stucks) : Data.stuckStore => {
        if (Js.Math.random() <= 0.25)
          Data.LocationMap.add(coord, rock, stucks)
        else
          stucks
    };
    let rock = Data.rock;
    let all_coords = getAllCoords(state);
    List.fold_left((stucks, coord) => {
      if (isEmpty(state, coord)) {
        addRock(coord, rock, stucks)
      }
      else {
        stucks
      }
    }, Data.LocationMap.empty, all_coords);
  };
  let union : (Data.stuckStore => Data.stuckStore => Data.stuckStore) = Data.LocationMap.merge(
    (_key: Data.location, stuck1: option(Data.stuck), stuck2: option(Data.stuck)) : option(Data.stuck) => {
      switch((stuck1, stuck2)) {
        | (None, None) => None
        | (Some(s), None) => Some(s)
        | (None, Some(s)) => Some(s)
        | (Some(_s1), Some(s2)) => Some(s2)
      }
    }
  );
  {
    ...state,
    stucks: union(state.stucks, newStucks)
  }
};

let generateMobs = (numMobs: int, state: Data.state) : Data.state => {
  let mob = Data.slime;
  let addRandomMob = (mobs: Data.mobStore) : Data.mobStore => {
    let potentialCoords = getAllCoords(state) |> List.filter(isEmpty({...state, mobs}));
    let randomCoord = List.length(potentialCoords) |> Random.int |> List.nth(potentialCoords);
    Data.LocationMap.add(randomCoord, mob, mobs);
  };
  {...state,
    mobs: List.fold_left((newMobs, _) => addRandomMob(newMobs),
      state.mobs, range(0, numMobs))
  }
};

module LocationSet = Set.Make({
  type t = Data.location;
  let compare = compare
});

/* Find a path from one square to another, using only empty squares (not counting target square) */
/* Returns Some([startLoc, ..., endLoc]) or None */
let findPath = (startLoc: Data.location, endLoc: Data.location, ~checkEmpty: (Data.location => bool)) : option(list(Data.location)) => {
  /* TODO: Could be made faster by checking whether endLoc is accessible and returning
  * None without a search in that case */
  let getNeighbours = ((x, y): Data.location) : list(Data.location) => {
    [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
  };

  let unvisited : Queue.t(Data.location) = Queue.create();
  let visited : ref(LocationSet.t) = ref(LocationSet.empty);
  let history : ref(Data.LocationMap.t(Data.location)) = ref(Data.LocationMap.empty);
  let isFound : ref(bool) = ref(false);
  /* Initialise */
  Queue.add(startLoc, unvisited);

  while (!Queue.is_empty(unvisited) && !isFound^) {
    let current : Data.location = Queue.pop(unvisited);
    if (current == endLoc) {
      isFound := true;
    }
    else {
      getNeighbours(current)
      |> List.filter((loc) => (checkEmpty(loc) || loc == endLoc) && !LocationSet.mem(loc, visited^))
      |> List.iter((loc) => {
        history := Data.LocationMap.add(loc, current, history^);
        Queue.push(loc, unvisited)
      });
      visited := LocationSet.add(current, visited^);
    }
  };
  if (isFound^) {
    /* Get path from history, [endLoc, ..., startLoc] */
    let rec mapToList = (loc: Data.location, aMap: Data.LocationMap.t(Data.location)) : list(Data.location) => {
      [loc, ...{switch(Data.LocationMap.find(loc, aMap)) {
          | parentLoc => mapToList(parentLoc, aMap)
          | exception Not_found => []
      }}]
    };
    Some(mapToList(endLoc, history^) |> List.rev)
  }
  else {
    /* No path found */
    None
  }
};

/* Move every mob one step. Do not move into non-empty squares */
/* N.B. mobs are moved one-by-one in turn order, not as a unit */
let moveMobs = ({mobs, loc: playerLoc} as state: Data.state) : Data.state => {
  {...state,
    mobs: Data.LocationMap.fold(
      (curLoc: Data.location, mob: Data.mob, newMobs: Data.mobStore) : Data.mobStore => {
        /* Use updated game board */
        let canMove : (Data.location => bool) = isEmpty({...state, mobs: newMobs});
        /* Monsters move closer to player even if portion of path passes over other mobs */
        let pathCandidate : (Data.location => bool) = (loc) => (canMove(loc) || getMobAtLoc(loc, newMobs) != None);
        let newLoc = switch(findPath(curLoc, playerLoc, ~checkEmpty=pathCandidate)) {
          | Some([_, nextLoc, ..._]) when canMove(nextLoc) => nextLoc
          | Some(_) => curLoc /* Don't move if no empty path */
          | None => curLoc
        };
        newMobs
          |> Data.LocationMap.remove(curLoc)
          |> Data.LocationMap.add(newLoc, mob)
      }, mobs, mobs)
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
  /* TODO: Make the tiles/squares its own module with Data.Tile.t, Data.Tile.Mob, etc. */
  range(0, width) |> List.map((row) => {
    range(0, height) |> List.map((col) => {
      /* Each square has either a player, a mob, a stuck, or it is empty */
      if ((row, col) == state.loc) {
        Data.Player
      }
      else {
        switch(Data.LocationMap.find((row, col), state.mobs)) {
          | mob => Data.Mob(mob)
          | exception Not_found => {
            switch(Data.LocationMap.find((row, col), state.stucks)) {
              | stuck => Data.Stuck(stuck)
              | exception Not_found => Data.Empty
            }
          }
        }
      }
    })
  })
};

let roomToConsoleStr = (r: Data.room) : string => {
  let tileToStr = (t: Data.square) : string => switch(t) {
    | Data.Empty => "."
    | Data.Player => "P"
    | Data.Mob({repr}) => repr
    | Data.Stuck({repr}) => repr
  };
  r |> List.map((row) => {
    row |> List.map(tileToStr) |> String.concat("")
  })
  |> String.concat("\n")
};

let squareToStr = (s: Data.square) : string => switch(s) {
  | Empty => {js|⬛|js}
  | Player => {js|🤺|js}
  | Data.Mob({repr}) => repr
  | Data.Stuck({repr}) => repr
};

let initialState: Data.state = ({
  player: {
    health: 10,
    weapon: Data.sword
  },
  loc: (1, 1),
  mobs: Data.LocationMap.empty,
  stucks: Data.LocationMap.(
    empty
    |> add((5, 3), Data.rock)
    |> add((6, 6), Data.rock)
  ),
  size: (10, 10)
} : Data.state) |> generateRocks |> generateMobs(3);

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
6.5. Action log to display mob attacks, player health, etc. [ ]
6.5.5. Fix glitch where mobs run into eachother and one disappears [ ]
7. Player death
8. Mob movement AI (BFS!) [X]
9. Randomly generated obstacles on the board [X]

0. Learn about functors/modules in OCaml create a Map[location -> mob] for the state :) [X]

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
