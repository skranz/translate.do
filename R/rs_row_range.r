

examples.RSRowRange = function() {
  .n()[1:10]
  .n() - 1
  
}

rs.row.range = function(start=1, end) {
  r = start:end
  class(r) = c("RSRowRange","integer")
  r  
}

.N = function() {
  by = get.rs.by()
  if (!is.null(by)) {
    stop(".N() is not yet correctly implemented, for commands grouped by 'by'. It should return the number of rows within the current group.")
  }
  NROW(get.rs.data())  
}

.n = function() {
  by = get.rs.by()
  if (!is.null(by)) {
    stop(".n() is not yet correctly implemented, for commands grouped by 'by'. It should return the range of all rows within the current group.")
  }
  
  N = NROW(get.rs.data())
  r = 1:N
  class(r) = c("RSRowRange","integer")
  r  
}


"+.RSRowRange" <- function(x,y,...) {
  restore.point("+.RSRowRange")
  nx = as.integer(x) + as.integer(y)
  N = NROW(get.rs.data())
  nx[nx<1 | nx >N] = NA
  class(nx) = c("RSRowRange","integer")
  nx
}

"-.RSRowRange" <- function(x,y,...) {
  restore.point("-.RSRowRange")
  nx = as.integer(x) - as.integer(y)
  N = NROW(get.rs.data())
  nx[nx<1 | nx >N] = NA
  class(nx) = c("RSRowRange","integer")
  nx
}