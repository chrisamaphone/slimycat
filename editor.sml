 (* EDITOR DESIGN:
  * while editing, we need to track what brush is selected (if any)
  * on mouseup (in EDIT mode only!):
  *   if mouse is over palette, figure out which brush,
  *     and change to that brush
  *   if mouse is over board and a brush is selected,
  *     figure out which board index we're over,
  *     and do a table insert (w/overwrite/replace behavior!)
  *     on the board 
  *)
structure Editor =
struct
  val palette = [Board.Wall, Board.Treat, Board.Slime true,
                  Board.Cat Board.E, Board.Cat Board.W, 
                  Board.Cat Board.N, Board.Cat Board.S]

  (* bounding box calculation for square targets *)
  fun within (x, y) (target_x, target_y) target_size =
    x >= target_x andalso y >= target_y andalso
    x < target_x + target_size andalso
    y < target_y + target_size

  (* tile position to screen position *)
  fun tile_screen_pos (tx,ty) =
    (tx * Consts.tile_size, ty * Consts.tile_size)

  (* palette index to screen position *)
  fun palette_screen_pos i =
  let
    val (px,py) = Consts.palette_pos
  in
    (px + Consts.tile_size * (i mod Consts.palette_width),
     py + Consts.tile_size * (i div Consts.palette_width))
  end

  datatype target = BoardTile of int * int | PaletteTile of int

  fun filteri' f [] _ = []
    | filteri' f (x::xs) n = if f(x,n) then x::(filteri' f xs (n+1)) else
        filteri' f xs (n+1)
  (* filter : ('a * int -> bool) -> 'a list -> 'a list *)
  fun filteri f l = filteri' f l 0

  (* from screen position to a target option *)
  fun screen_pos_target (board,brush) (x,y) =
  let
    val board_targets =
      Board.filteri
        (fn ((tx,ty),tile) => 
          within (x,y) (tile_screen_pos (tx,ty)) Consts.tile_size)
        board
    val board_targets = Board.listItemsi board_targets
    val palette_targets =
      filteri
      (fn (tile,i) => within (x,y) (palette_screen_pos i) Consts.tile_size)
      palette
    val board =
      (case board_targets of
            [] => board
          | (((tx,ty),tile)::_) =>
              (case brush of
                    NONE => #1 (Board.remove (board, (tx,ty)))
                  | SOME tile =>
                    Board.insert (((tx,ty),tile), board)))
    val brush =
      (case palette_targets of
            [] => brush
          | (tile::_) => SOME tile)
  in
    (board,brush)
  end

  (* mapping from screen location & palette index to bool for
  *  whether the (x,y) is within bounds of the palette tile *)

  (* mapping from screen location x,y & board tile i,j to
  * whether (x,y) is in bounds of tile (i,j) *)

  (* return a board * editor_state *)
  fun handle_event e (board, state as {mouse_x,mouse_y,brush}) =
    case e of
         SDL.E_MouseMotion {x,y,...} =>
            (board, {mouse_x=x, mouse_y=y, brush=brush})
       | SDL.E_MouseUp {button, x, y} =>
           let
             val (board,brush) = screen_pos_target (board,brush) (x,y)
           in
             (board, {mouse_x=mouse_x,mouse_y=mouse_y,brush=brush})
           end
       | _ => (board, state)
end
