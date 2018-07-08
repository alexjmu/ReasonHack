type weapon = {
  name: string,
  damage: int
};

type player = {
  health: int,
  weapon: weapon,
};

type direction =
  | Left
  | Right
  | Up
  | Down;

type action =
  | Move(direction)
  | Attack;

type mob = {
  name: string,
  health: int,
  repr: string
};

type location = (int, int);

let sword: weapon = {
  name: "wood sword",
  damage: 1
};

let slime: mob = {
  name: "green slime",
  health: 2,
  repr: "S"
};

type square =
  | Empty
  | Player
  | Mob(mob);

type room = list(list(square));

type state = {
  player: player,
  loc: location,
  mobs: list((location, mob)),
  size: (int, int)
};
