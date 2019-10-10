#library("tsoutliers")

streamQ <- function(datastream) {
  df <- jsonlite::stream_in(url(datastream))
  dfjson <- jsonlite::fromJSON(jsonlite::toJSON(df))
  mydata <- dfjson$nameValuePairs
  columnnames <-colnames(mydata)

  x<-data.frame(mydata$inDateTime$nameValuePairs)

  tempout_index <- grep(columnnames[grepl('temperature_indoor',columnnames)], colnames(mydata))


  xy <- data.frame(x,mydata[tempout_index])
  xy <-na.omit(xy)

  colnames(xy) <- c("time","tempout")
  #  xy <- subset(xy, xy$tempout > -30)
  summary(xy)

  dat.ts<- ts(xy$tempout,frequency=1)
  data.ts.outliers <- tso(dat.ts, types = c("AO", "LS", "TC", "IO", "SLS"))
  Q = 1 - nrow(data.ts.outliers$outliers)/nrow(xy)

  return(Q)
}
