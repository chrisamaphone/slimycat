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
  datatype mode = PLAY of Board.board (* original board *)
                | PAUSE of Board.board (* original board *)
                | EDIT of {mouse_x:int, mouse_y:int, brush:Board.tile option}
  val editor_init = {mouse_x=0, mouse_y=0, brush=NONE}
  fun flip (EDIT _) orig = PLAY orig
    | flip (PLAY orig) _ = PAUSE orig
    | flip (PAUSE orig) _ = PLAY orig
    
  (*
  fun reset (PLAY orig) _ = (orig, PLAY orig)
    | reset (PAUSE orig) _ = (orig, PAUSE orig)
    | reset (EDIT e) b = (b, EDIT e)
  *)

  fun edit (PLAY orig) b = (orig, EDIT editor_init)
    | edit (PAUSE orig) b = (orig, EDIT editor_init)
    | edit (EDIT e) b = (b, EDIT e)
 
  type state = Board.board * mode

  (*
  val init_list =
    [((0,0),(Board.Cat Board.E)), ((4,0), Board.Wall),
     ((1,1),(Board.Slime false)),
     ((2,2),(Board.Slime true)),
     ((3,3),Board.Wall)]

  val initstate = foldl (Board.insert) Board.empty init_list
  *)
  val initstate = (Board.loadBoard "boards/board1.txt", EDIT editor_init)

  (* Board and rendering *)
  val tiles_wide = Consts.tiles_wide
  val tiles_high = Consts.tiles_high
  val tile_size = Consts.tile_size

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
        SOME Board.Wall => imageWall
      | SOME Board.Treat => imageTreat
      | SOME (Board.Slime true) => imageSlime_active
      | SOME (Board.Slime false) => imageSlime_dormant
      | SOME (Board.Cat Board.W) => imageCatW
      | SOME (Board.Cat Board.E) => imageCatE
      | SOME (Board.Cat Board.N) => imageCatN
      | SOME (Board.Cat Board.S) => imageCatS
      | NONE => imageFloor


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
        (* render only things in bounds *)
        if 0 <= x andalso x < tiles_high andalso 0 <= y andalso y < tiles_wide
        then SDL.blitall(tile_image (SOME tile), screen,
                         x*tile_size, y*tile_size)
        else ())
      board;
    (* palette *)
    ListUtil.appi
      (fn (tile,i) =>
        SDL.blitall(tile_image tile, screen, 
          (#1 Consts.palette_pos) + tile_size * (i mod Consts.palette_width),
          (#2 Consts.palette_pos) + tile_size * (i div Consts.palette_width)))
      Editor.palette;
    (* brush *)
    case mode of
         EDIT {mouse_x,mouse_y,brush}
          => SDL.blitall(tile_image brush, screen, mouse_x - tile_size div 2,
                                                   mouse_y - tile_size div 2)
      | _ => ();
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


  (* Input handling *)
  (* SPACE - start/stop sim
  *  q - quit
  *  r or e - reset board and enter the editor
  *  (* not impl'd yet: *)
  *  s - save board (reset to that board)
  *
  *  XXX factor out edit vs play mode event handling
  *)
  fun handle_event e (board, mode) = 
    case e of
         SDL.E_KeyDown {sym=SDL.SDLK_q} =>
         let in
           Board.saveBoard board tiles_wide tiles_high "boards/board-out.txt";
           NONE
         end
      | SDL.E_KeyDown {sym=SDL.SDLK_SPACE} =>
          SOME (board, flip mode board)
      | SDL.E_KeyDown {sym=SDL.SDLK_r} =>
          SOME (edit mode board)
      | SDL.E_KeyDown {sym=SDL_SDLK_e} =>
          SOME (edit mode board)
      | e =>  
          (case mode of
               PLAY _ => SOME (board, mode)
             | PAUSE _ => SOME (board, mode)
             | EDIT editor_state => 
                 let
                   val (board, state) = Editor.handle_event e (board, editor_state)
                 in
                   SOME (board, EDIT state)
                 end)

  fun tick (board, PLAY orig) = SOME (Simulate.step board, PLAY orig)
    | tick (board, PAUSE orig) = SOME (board, PAUSE orig)
    | tick (board, EDIT e) = SOME (board, EDIT e)

end


structure Main =
struct
    structure S = RunGame (SlimyCat)
end
