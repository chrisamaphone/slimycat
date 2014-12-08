structure Consts =
struct
  val tiles_wide = 8
  val tiles_high = 8
  val tile_size = 64
  val top_bar_height = 0 (* no top bar yet tho... *)
  val spacer = 20
  val palette_pos = (tile_size * tiles_wide + spacer, top_bar_height)
  val palette_width = 4
  val cheevo_x = tile_size * (tiles_wide + palette_width + 2) + spacer
  val cheevo_y = spacer
end
