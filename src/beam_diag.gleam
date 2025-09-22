import gleam/io

@external(erlang, "gh_diag_ffi", "diag_line_bin")
pub fn diag_line() -> String

pub fn print() -> Nil {
  io.println(diag_line())
}
