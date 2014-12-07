structure SlimyCat : GAME = 
struct

  (* SDL setup *)
  type screen = SDL.surface
  fun initscreen s = () (* ? *)
  val width = 1024
  val height = 768
  val use_gl = false
  val ticks_per_second = 1

  (* Board and rendering *)
  type state = Board.board

  val init_list =
    [((0,0),(Board.Cat Board.E)),
     ((1,1),(Board.Slime false)),
     ((2,2),(Board.Slime true)),
     ((3,3),Board.Wall)]

  val initstate = foldl (Board.insert) Board.empty init_list

  val tiles_wide = 8
  val tiles_high = 8
  val tile_size = 64

  val imageFloor = Graphics.requireimage "assets/carpet.png"
  val imageWall = Graphics.requireimage "assets/wall.png"
  val imageSlime_active = Graphics.requireimage "assets/slime_active.png"
  val imageSlime_dormant = Graphics.requireimage "assets/slime_dormant.png"
  val imageCatW = Graphics.requireimage "assets/cat_L.png"
  val imageCatN = Graphics.requireimage "assets/cat_U.png"
  (* XXX make CatR and CatD w/graphics functions? *)
  val imageCatE = imageCatW
  val imageCatS = imageCatN

  fun tile_image tile =
    case tile of
        Board.Wall => imageWall
      | (Board.Slime true) => imageSlime_active
      | (Board.Slime false) => imageSlime_dormant
      | (Board.Cat Board.W) => imageCatW
      | (Board.Cat Board.E) => imageCatE
      | (Board.Cat Board.N) => imageCatN
      | (Board.Cat Board.S) => imageCatS

  fun render screen board = 
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

  fun move (x, y) Board.N = (x, y-1)
    | move (x, y) Board.S = (x, y+1)
    | move (x, y) Board.W = (x-1, y)
    | move (x, y) Board.E = (x+1, y)

  fun influenced (pos as (x, y)) tile =
    case tile of
        Board.Wall => []
      | Board.Slime false => []
      | Board.Slime true => map (move pos) [Board.N, Board.S, Board.E, Board.W]
      | Board.Cat dir => [pos, move pos dir]

  (* Input handling *)
  fun handle_event e board = 
    case e of
         SDL.E_KeyDown {sym=SDLK_q} => NONE
      | _ =>  SOME board

  fun tick board = SOME board

end


structure Main =
struct
    structure S = RunGame (SlimyCat)
end
