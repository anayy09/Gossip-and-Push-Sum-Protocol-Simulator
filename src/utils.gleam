import gleam/io

@external(erlang, "gh_diag_ffi", "diag_line_bin")
pub fn diag_line() -> String

pub fn print() -> Nil {
  io.println(diag_line())
}

@external(erlang, "monotime_ffi", "now_milliseconds")
pub fn now_milliseconds() -> Int

pub fn elapsed_ms(start_ms: Int, end_ms: Int) -> Int {
  let diff = end_ms - start_ms
  case diff < 0 {
    True -> 0
    False -> diff
  }
}
