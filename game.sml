(* signature GAME =
sig
  type state
  type screen = SDL.surface
  val initstate : state
  val initscreen : screen -> unit
  val width : int
  val height : int
  val render : screen -> state -> unit
  val use_gl : bool
  val handle_event : SDL.event -> state -> state option

  (* Take one step of game logic. Typically this is much cheaper than
     rendering, particularly because of the cost of SDL.flip. Zero,
     one, or multiple ticks may be taken between renders. *)
  val tick : state -> state option

  (* Target rate of game logic steps. Game time elapses
     (1.0 / ticks_per_second) seconds every tick. Rendering only
     occurs once game time is caught up to real time.
     It is important that calls to tick are cheap enough
     that this rate can be comfortably achieved --- otherwise
     rendering will occur less and less frequently. *)
  val ticks_per_second : int
end
*)

structure SlimyCat : GAME =
struct

  type state = unit
  type screen = SDL.surface

  val initstate = ()
  fun initscreen s = ()

  val width = 1024
  val height = 768

  fun render s () = ()

  val use_gl = false

  fun handle_event e () = SOME ()

  fun tick () = SOME ()

  val ticks_per_second = 1

end

structure Main =
struct
    structure S = RunGame (Game)
end
