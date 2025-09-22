/// Monotonic time in milliseconds, suitable for measuring elapsed durations.
/// Backed by Erlang's monotonic clock via a tiny FFI.
@external(erlang, "monotime_ffi", "now_milliseconds")
pub fn now_milliseconds() -> Int

/// Convenience to compute elapsed milliseconds between two readings.
pub fn elapsed_ms(start_ms: Int, end_ms: Int) -> Int {
  let diff = end_ms - start_ms
  case diff < 0 {
    True -> 0
    False -> diff
  }
}
