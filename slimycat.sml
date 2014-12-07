structure SlimyCat : GAME = 
struct

  (* SDL setup *)
  type screen = SDL.surface
  fun initscreen s = () (* ? *)
  val width = 1024
  val height = 768
  val use_gl = false
  val ticks_per_second = 1

  (* editor stuff *)
  datatype mode = EDIT | PLAY
  fun flip EDIT = PLAY
    | flip PLAY = EDIT
  val palette = [Board.Wall, Board.Treat, Board.Slime true,
                  Board.Cat Board.E, Board.Cat Board.W, 
                  Board.Cat Board.N, Board.Cat Board.S]

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

  type state = Board.board * mode

  (*
  val init_list =
    [((0,0),(Board.Cat Board.E)), ((4,0), Board.Wall),
     ((1,1),(Board.Slime false)),
     ((2,2),(Board.Slime true)),
     ((3,3),Board.Wall)]

  val initstate = foldl (Board.insert) Board.empty init_list
  *)
  val initstate = (Board.loadBoard "boards/board1.txt", EDIT)

  (* Board and rendering *)
  val tiles_wide = 8
  val tiles_high = 8
  val tile_size = 64

  val imageFloor = Graphics.requireimage "assets/carpet.png"
  val imageWall = Graphics.requireimage "assets/wall.png"
  val imageTreat = Graphics.requireimage "assets/treat.png"
  val imageSlime_active = Graphics.requireimage "assets/slime_active.png"
  val imageSlime_dormant = Graphics.requireimage "assets/slime_dormant.png"
  val imageCatW = Graphics.requireimage "assets/cat_L.png"
  val imageCatN = Graphics.requireimage "assets/cat_U.png"
  val imageCatE =  Graphics.requireimage "assets/cat_R.png"
  val imageCatS = Graphics.requireimage "assets/cat_D.png"

  fun tile_image tile =
    case tile of
        Board.Wall => imageWall
      | Board.Treat => imageTreat
      | (Board.Slime true) => imageSlime_active
      | (Board.Slime false) => imageSlime_dormant
      | (Board.Cat Board.W) => imageCatW
      | (Board.Cat Board.E) => imageCatE
      | (Board.Cat Board.N) => imageCatN
      | (Board.Cat Board.S) => imageCatS

  (* XXX add editor stuff; buttons depend on mode *)
  (* editor stuff for rendering *)
  val top_bar_height = 128 (* no top bar yet tho... *)
  val palette_pos = (tile_size * (tiles_wide + 2), top_bar_height)
  val pallet_width = 2

  fun render screen (board, mode) = 
  let in
    SDL.clearsurface (screen, SDL.color(0wxff,0wxff,0wxff,0wxff));
    (* floor *)
    List.tabulate (tiles_wide,
      (fn x => List.tabulate (tiles_high, (fn y => 
        SDL.blitall(imageFloor, screen, x*tile_size, y*tile_size)))));
    (* board tiles *)
    Board.mapi
      (fn ((x,y), tile) =>
        SDL.blitall(tile_image tile, screen, x*tile_size, y*tile_size))
      board;
    (* palette *)
    ListUtil.appi
      (fn (tile,i) =>
        SDL.blitall(tile_image tile, screen, 
          (#1 palette_pos) + tile_size* (i mod pallet_width),
          (#2 palette_pos) + tile_size * (i div pallet_width)))
      palette;
    SDL.flip screen
  end

  (*
  let
    val rows = 
      List.tabulate (tiles_wide,
        (fn x => (List.tabulate (tiles_high, (fn y => render_pos (x,y) board)))))
  in
    ListUtil.appi 
      (fn (row,y) => 
        ListUtil.appi 
          (fn (pic,x) => SDL.blitall(pic, screen, x*tile_size, y*tile_size))
        row    
      ) rows;
  end
  *)

  (* Game logic *)

  (* Input handling *)
  (* SPACE - start/stop sim
  *  q - quit
  *  r - reset board
  *)
  fun handle_event e (board, mode) = 
    case e of
         SDL.E_KeyDown {sym=SDL.SDLK_q} =>
         let in
           Board.saveBoard board tiles_wide tiles_high "boards/board-out.txt";
           NONE
         end
      | SDL.E_KeyDown {sym=SDL.SDLK_SPACE} =>
          SOME (board, flip mode)
      | SDL.E_KeyDown {sym=SDL.SDLK_r} =>
          SOME initstate
          (* XXX this should reload the board saved from the editor *)
      | _ =>  SOME (board, mode)

  fun tick (board, PLAY) = SOME (Simulate.step board, PLAY)
    | tick (board, EDIT) = SOME (board, EDIT)

end


structure Main =
struct
    structure S = RunGame (SlimyCat)
end
