structure SlimyCat = 
struct
  datatype direction = N | S | E | W
  datatype tile = Cat of direction | Wall | Slime of bool (* active *)
                  (* NB: no "Empty" -- only tiles with stuff are represented *)

  structure IntPairOrd =
  struct
    type ord_key = int * int
    fun compare ((x1, y1), (x2, y2)) =
      if x1 = x2 andalso y1 = y2 then EQUAL
      else if x1 < x2 then LESS
        else if x1 = x2 then
          Int.compare (y1, y2)
          else
            GREATER
  end

  structure IntPairMap = SplayMapFn (IntPairOrd)
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
      | Cat dir => [pos, move pos dir]


  (* functioning stuff *)
  type state = board

  val init_list =
    [((0,0),(Cat E)),
     ((0,1),(Slime false)),
     ((0,2),(Slime true)),
     ((0,3),Wall)]

 (*  val initstate : state = Board (IntPairMap.empty) *)
  val initstate = Board (foldl (IntPairMap.insert') IntPairMap.empty init_list)
  

  val tiles_wide = 8
  val tiles_high = 8

  fun render_pos pos board =
    case IntPairMap.find (board, pos) of
         NONE => "_"
      | SOME Wall => "X"
      | SOME (Slime true) => "*"
      | SOME (Slime false) => "#"
      | SOME (Cat _) => "&"

  fun render screen (Board b) =
  let
    val rows = 
      List.tabulate (tiles_wide,
        (fn x => (List.tabulate (tiles_high, (fn y => render_pos (x,y) b)))))
    val string =
      String.concatWith "\n" (map (String.concatWith "|") rows)
  in
    print string
  end
end
