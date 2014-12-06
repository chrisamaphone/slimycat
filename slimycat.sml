datatype direction = N | S | E | W
datatype tile = Cat of direction | Wall | Slime of bool (* active *)
                (* NB: no "Empty" -- only tiles with stuff are represented *)

(* XXX make this whatever it needs to be to work *)
datatype board = Board of tile IntPairMap.map

fun move (x, y) N = (x, y-1)
  | move (x, y) S = (x, y+1)
  | move (x, y) W = (x-1, y)
  | move (x, y) E = (x+1, y)

fun influenced (pos as (x, y)) tile =
  case tile of
      Wall => []
    | Slime false => []
    | Slime true => map (move pos) [N, S, E, W]
    | Cat dir = [pos, move pos dir]
