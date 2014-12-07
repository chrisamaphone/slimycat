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

  (*
  val init_list =
    [((0,0),(Board.Cat Board.E)), ((4,0), Board.Wall),
     ((1,1),(Board.Slime false)),
     ((2,2),(Board.Slime true)),
     ((3,3),Board.Wall)]

  val initstate = foldl (Board.insert) Board.empty init_list
  *)
  val initstate = Board.loadBoard "boards/board1.txt"

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
  (* XXX make CatR and CatD w/graphics functions? *)
  val imageCatE = imageCatW
  val imageCatS = imageCatN

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

  fun rotate Board.N = Board.E
    | rotate Board.E = Board.S
    | rotate Board.S = Board.W
    | rotate Board.W = Board.N

  fun influenced (pos as (x, y)) tile =
    case tile of
        Board.Wall => []
      | Board.Treat => []
      | Board.Slime false => []
      | Board.Slime true => map (move pos) [Board.N, Board.S, Board.E, Board.W]
      | Board.Cat dir => [pos, move pos dir]

  (* Input handling *)
  fun handle_event e board = 
    case e of
         SDL.E_KeyDown {sym=SDL.SDLK_q} =>
         let in
           Board.saveBoard board tiles_wide tiles_high "boards/board-out.txt";
           NONE
         end
      | _ =>  SOME board

  (**** william ****)

  local
    open Board
  in

  (* Is a board position empty? *)
  fun inBounds (x, y) = 0 <= x andalso x < tiles_wide andalso
                        0 <= y andalso y < tiles_high
  fun clear board pos = inBounds pos andalso Board.find (board, pos) = NONE

  datatype update = Del of int * int
                  | Set of (int * int) * tile
    
  fun update board pos Wall = []
    | update board pos (cat as (Cat dir)) =
        let val pos' = move pos dir in
          if clear board pos'
          (* Cats move in the direction they're facing if clear *)
          then [Del pos, Set (pos', cat)]
          (* Otherwise, they rotate in place *)
          else [Set (pos, Cat (rotate dir))]
        end
    | update board pos (Slime false) = []
    | update board pos (Slime true) =
        (* Active slime becomes inactive slime *)
        Set (pos, Slime false) ::
        (* Any clear neighbors of active slime become active slime *)
        map (fn p => Set (p, Slime true))
            (List.filter (clear board) (map (move pos) [N, S, E, W]))

  fun updates board =
    IntPairMap.foldli
      (fn ((x, y), tile, us) => update board (x, y) tile @ us)
      []
      board

  fun apply ((Del pos), board) = #1 (IntPairMap.remove (board, pos))
    | apply ((Set (pos, tile)), board) = Board.insert ((pos, tile), board)

  fun applyAll updates board = List.foldl apply board updates

  fun posGt ((x1, y1), (x2, y2)) = x1 > x2 orelse (x1 = x2 andalso y1 > y2)
  fun updatePos (Set (pos, _)) = pos
    | updatePos (Del pos) = pos
  fun updateGt (u1, u2) = posGt (updatePos u1, updatePos u2)

  fun (* delete + u = u *)
      resolvePair (Del _, u) = u
    | resolvePair (u, Del _) = u
      (* cats beat slime *)
    | resolvePair (u_cat as (Set (_, Cat _)), Set (_, Slime _)) = u_cat
    | resolvePair (Set (_, Slime _), u_cat as (Set (_, Cat _))) = u_cat
      (* arbitrary? *)
    (* | resolvePair (u1, u2) = u2 *)

  fun resolvePairs [] = []
    | resolvePairs [u] = [u]
    | resolvePairs (u1::u2::us) =
        if updatePos u1 <> updatePos u2
        (* Updates touching different positions are already resolved *)
        then u1 :: resolvePairs (u2::us)
        (* Resolve each same-position pair individually, then keep resolving *)
        else resolvePairs (resolvePair (u1, u2) :: us)

  fun resolve updates = resolvePairs (ListMergeSort.sort updateGt updates)
  end

  (**** / william ****)

  fun tick board = SOME (applyAll (updates board) board)

end


structure Main =
struct
    structure S = RunGame (SlimyCat)
end
