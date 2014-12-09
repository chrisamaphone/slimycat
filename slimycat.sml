structure SlimyCat : GAME = 
struct

  (* SDL setup *)
  type screen = SDL.surface
  fun initscreen s = () (* ? *)
  val width = 1200
  val height = 640
  val use_gl = false
  val ticks_per_second = 2

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
 
  type state = Board.board * mode * (string * bool) list

  (*
  val init_list =
    [((0,0),(Board.Cat Board.E)), ((4,0), Board.Wall),
     ((1,1),(Board.Slime false)),
     ((2,2),(Board.Slime true)),
     ((3,3),Board.Wall)]

  val initstate = foldl (Board.insert) Board.empty init_list
  *)
  val cheevoFlood = "flash flood!"
  val cheevoSlimy = "slimy cat!"
  val initstate = 
    (Board.loadBoard "boards/board1.txt",
     EDIT editor_init,
     map (fn s => (s, false))
      [cheevoFlood, cheevoSlimy])

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


  (* font stuff *)
  local
    val charmap = " ABCDEFGHIJKLMNOPQRSTUVWXZY"
                ^ "abcdefghijklmnopqrstuvwxyz"
                ^ "0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?"
  in
  structure HugeFont =
    FontFn
    (struct
      val surf = Graphics.requireimage "media/graphics/fonthuge.png"
      val styles = 7
      val charmap = charmap
      val width = 27
      val height = 48
      val overlap = 3
      val dims = 3
     end)
  structure NormalFont =
    FontFn
    (struct
      val surf = Graphics.requireimage "media/graphics/font.png"
      val styles = 6
      val charmap = charmap
      val width = 9
      val height = 16
      val overlap = 0
      val dims = 3
     end)
  end

  val instructions = 
    ["Welcome to SlimyCat!",
    "Interface:",
    "[SPACE]: start/pause",
    "r:       restart in edit mode.",
    "m:       toggle music.",
    "q:       quit.",
    "EDIT MODE:",
    "Click a palette tile to select a brush.",
    "Click the board to change a tile."]

  val credits =
    ["Credits:",
     "Game code: Chris Martens and William Lovas",
     "Libraries and fonts: Tom Murphy VII",
     "Music and pixels: Chris Martens",
     "Made in Standard ML for Ludum Dare 31"]

  fun fontDrawLines screen x y lines =
    ListUtil.mapi 
      (fn (l,i) => NormalFont.draw (screen, x, y + i*18, l))
      lines

  fun render screen (board, mode, cheevos) = 
  let in
    SDL.clearsurface (screen, SDL.color(0wx1b,0wx38,0wx04,0wxff));
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
    (* text *)
    (*
    NormalFont.draw (screen, 32, Consts.tile_size * (Consts.tiles_high + 1),
       "welcome to SlimyCat!  Cats can't actually be slimy");
    *)
    fontDrawLines screen 
      (#1 Consts.palette_pos) 
      (Editor.palette_bottom)
      instructions;
    fontDrawLines screen
      10 520
      credits;
    fontDrawLines screen Consts.cheevo_x Consts.cheevo_y
      ("Achievements:"::map (fn (s, true) => s | _ => "") cheevos);
    (*
    NormalFont.draw (screen, Consts.tile_size * (Consts.tiles_wide + 1),
                      Editor.palette_bottom + Consts.tile_size,
                      instructions); *)
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
  fun handle_event e (board, mode, cheevos) = 
    case e of
         SDL.E_KeyDown {sym=SDL.SDLK_q} =>
         let in
           Board.saveBoard board tiles_wide tiles_high "boards/board-out.txt";
           NONE
         end
      | SDL.E_KeyDown {sym=SDL.SDLK_SPACE} =>
          SOME (board, flip mode board, cheevos)
      | SDL.E_KeyDown {sym=SDL.SDLK_r} =>
          let
            val (board,mode) = edit mode board
          in
            SOME (board,mode,cheevos)
          end
      (*| SDL.E_KeyDown {sym=SDL.SDLK_e} =>
          SOME (edit mode board, cheevos)*)
      | SDL.E_KeyDown {sym=SDL.SDLK_m} =>
          (
            (if SDLMusic.is_paused () then
              SDLMusic.resume ()
            else
              SDLMusic.pause ());
            SOME (board, mode, cheevos)
          )
      | e =>  
          (case mode of
               PLAY _ => SOME (board, mode, cheevos)
             | PAUSE _ => SOME (board, mode, cheevos)
             | EDIT editor_state => 
                 let
                   val (board, state) = Editor.handle_event e (board, editor_state)
                 in
                   SOME (board, EDIT state, cheevos)
                 end)

  (*** achievements ***)

  (* XXX some of this should be in Simulate?  or factored out, maybe.. *)

  local open Board in

  fun isSlime board pos = Board.find (board, pos) = SOME (Slime false)
    (*
    case Board.find (board, pos) of
        SOME (Slime _) => true (* should this only be true on inactive slime? *)
      | _ => false
    *)

  (* slimyCat is achieved if any Cat is surrounded completely by inactive slime 
  *)
  fun any board p =
    Board.IntPairMap.foldli
      (fn (pos, entity, b) => b orelse p (pos, entity))
      false
      board

  fun slimyCat board =
    let fun isCat (Cat _) = true
          | isCat _ = false
        fun neighbors pos = map (Simulate.move pos) [N, S, E, W] (* [N, S, E, W] *)
    in
      any board
        (fn (pos, entity) =>
              isCat entity andalso
              List.all (isSlime board) (neighbors pos))
    end

  (* filledBoard is achieved if the previous board wasn't full and the current
     board is *)
  fun filledBoard (board, board') =
    let val totalSquares =
      (* main board *)
      Consts.tiles_wide * Consts.tiles_high +
      (* top and bottom "implicit walls" *)
      Consts.tiles_wide * 2 +
      (* left and right "implicit walls" *)
      Consts.tiles_high * 2
    in
      Board.IntPairMap.numItems board < totalSquares andalso
      Board.IntPairMap.numItems board' = totalSquares
    end
  
  end (* local open Board *)

  fun achieve s cheevos = map (fn (s', got) => (s', got orelse s = s')) cheevos

  fun updateCheevos board board' c =
    let
      val c = if slimyCat board' then achieve cheevoSlimy c else c
      val c = if filledBoard (board, board') then achieve cheevoFlood c else c
    in
      c
    end

  (*** end achievements ***)

  fun tick (board, PLAY orig, c) =
        let val board' = Simulate.step board
            (* XXX need an "achievement state" for history-based achievements *)
            val c' = updateCheevos board board' c
        in
          SOME (board', PLAY orig, c')
        end
    | tick (board, PAUSE orig, c) = SOME (board, PAUSE orig, c)
    | tick (board, EDIT e, c) = SOME (board, EDIT e, c)


  val (SOME song) = SDLMusic.load "assets/cat.wav"
  val () = SDLMusic.loop song
  (* XXX make "s" or "m" toggle sound *)
end


structure Main =
struct
    structure S = RunGame (SlimyCat)
end
