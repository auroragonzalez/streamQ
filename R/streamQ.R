#library("tsoutliers")

streamQ <- function(x) {
  dat.ts<- ts(x,frequency=1)
  data.ts.outliers <- tso(dat.ts, types = c("AO", "LS", "TC", "IO", "SLS"))
  Q = 1 - nrow(data.ts.outliers$outliers)/nrow(xy)

  return(Q)
}
