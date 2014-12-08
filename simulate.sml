structure Simulate =
struct
  local
    open Board
  in

  fun move (x, y) Board.N = (x, y-1)
    | move (x, y) Board.S = (x, y+1)
    | move (x, y) Board.W = (x-1, y)
    | move (x, y) Board.E = (x+1, y)

  fun rotate Board.N = Board.E
    | rotate Board.E = Board.S
    | rotate Board.S = Board.W
    | rotate Board.W = Board.N

  (* XXX not used *)
  fun influenced (pos as (x, y)) tile =
    case tile of
        Board.Wall => []
      | Board.Treat => []
      | Board.Slime false => []
      | Board.Slime true => map (move pos) [Board.N, Board.S, Board.E, Board.W]
      | Board.Cat dir => [pos, move pos dir]

  (*
  fun inBounds (x, y) = 0 <= x andalso x < tiles_wide andalso
                        0 <= y andalso y < tiles_high
  *)
  (* Is a board position empty? *)
  fun clear board pos = (* inBounds pos andalso *)
                        Board.find (board, pos) = NONE

  (* Is board position edible? *)
  fun edible board pos = Board.find (board, pos) = SOME Treat

  datatype update = Del of int * int
                  | Set of (int * int) * tile

  fun ortho dir = (rotate dir, rotate (rotate (rotate dir)))
    
  fun update board (pos, Wall) = []
    | update board (pos, Treat) = []
    | update board (pos, cat as (Cat dir)) =
        let
          val (l, r) = ortho dir
        in
          case (edible board (move pos l), edible board (move pos r)) of
               (* turn toward treat if one is to my left or right *)
               (true, _) => [Set (pos, Cat l)]
             | (_, true) => [Set (pos, Cat r)]
             | _ => (* otherwise, move straight or turn *)
              let val pos' = move pos dir in
                if clear board pos' orelse edible board pos'
                (* Cats move in the direction they're facing if it's clear/edible *)
                then [Del pos, Set (pos', cat)]
                (* Otherwise, they rotate in place *)
                else [Set (pos, Cat (rotate dir))]
              end
        end
    | update board (pos, Slime false) = []
    | update board (pos, Slime true) =
        (* Active slime becomes inactive slime *)
        Set (pos, Slime false) ::
        (* Any clear neighbors of active slime become active slime *)
        map (fn p => Set (p, Slime true))
            (List.filter (clear board) (map (move pos) [N, S, E, W]))

  fun cats board =
    IntPairMap.foldri
      (fn (pos, c as Cat _, pos_tiles) => (pos, c) :: pos_tiles
        | (pos, _, pos_tiles) => pos_tiles)
      []
      board

  fun slimes board = 
    IntPairMap.foldri
      (fn (pos, s as Slime true, pos_tiles) => (pos, s) :: pos_tiles
        | (pos, _, pos_tiles) => pos_tiles)
      []
      board

  (*
  fun updates board =
    IntPairMap.foldli
      (fn ((x, y), tile, us) => update board (x, y) tile @ us)
      []
      board
  *)

  fun apply ((Del pos), board) = #1 (IntPairMap.remove (board, pos))
    | apply ((Set (pos, tile)), board) = Board.insert ((pos, tile), board)

  fun applyAll updates board = foldl apply board updates

  fun doUpdates board pos_tiles =
    foldl
      (fn ((pos, t), board) => applyAll (update board (pos, t)) board)
      board
      pos_tiles
  end

  (* Run the cats in order, then the slimes in order *)
  fun step board = doUpdates board (cats board @ slimes board)
end
