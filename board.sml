structure Board =
struct
  datatype direction = N | S | E | W
  datatype tile = Cat of direction | Wall | Slime of bool (* active *)
                | Treat
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

  type board = tile IntPairMap.map
  val empty = IntPairMap.empty
  val insert = IntPairMap.insert'
  val find = IntPairMap.find
  val mapi = IntPairMap.mapi

  (* Parsing text file representation *)
  fun loadBoard fname =
  let
    val ins = TextIO.openIn fname
    val board = empty
    fun stringToTile s =
      (case s of
            "X" => SOME Wall
          | "%" => SOME Treat
          | "E" => SOME (Cat E)
          | "N" => SOME (Cat N)
          | "S" => SOME (Cat S)
          | "W" => SOME (Cat W)
          | "#" => SOME (Slime false)
          | "*" => SOME (Slime true)
          | _ => NONE)
    fun processLine line =
      let
        val tokens = String.tokens (Char.isSpace) line
      in
        map stringToTile tokens
      end
    fun processLines stream y board =
      (case (TextIO.inputLine stream) of
           NONE => board
         | SOME line =>
             let
               val tiles = processLine line
               val boardWithAddedLine =
                  ListUtil.foldli
                  (fn (x,tile,b) => 
                      case tile of
                          NONE => b
                        | SOME tile => IntPairMap.insert (b, (x,y), tile)
                  ) board tiles
             in
               processLines stream (y+1) boardWithAddedLine
             end)
  in
    processLines ins 0 board
  end

  fun saveBoard board width height fname =
  let
    val outs = TextIO.openOut fname
    fun tileToString t =
      (case t of
            Wall => "X"
          | Treat => "%"
          | (Cat E) => "E"
          | (Cat W) => "W"
          | (Cat N) => "N"
          | (Cat S) => "S"
          | (Slime true) => "*"
          | (Slime false) => "#")
  in
    IntPairMap.mapi
    (fn (pos, tile) => ...)
    board
  end

end
