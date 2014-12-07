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
            (board, 
            {mouse_x=mouse_x,mouse_y=mouse_y, brush = SOME Board.Wall}) (* XXX *)
       | _ => (board, state)
end
