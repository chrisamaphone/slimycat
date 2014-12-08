structure Consts =
struct
  val tiles_wide = 8
  val tiles_high = 8
  val tile_size = 64
  val top_bar_height = 0 (* no top bar yet tho... *)
  val palette_pos = (tile_size * (tiles_wide + 2), top_bar_height)
  val palette_width = 4
end
