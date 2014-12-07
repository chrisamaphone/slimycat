structure Board =
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

  type board = tile IntPairMap.map
  val empty = IntPairMap.empty
  val insert = IntPairMap.insert'
  val find = IntPairMap.find
  val mapi = IntPairMap.mapi

end
