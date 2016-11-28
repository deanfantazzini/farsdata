## ---- echo=TRUE, eval=TRUE-----------------------------------------------
 library(farsdata)
 data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
 setwd(dirname(data_file))
 file="accident_2013.csv.bz2"
 dat<-fars_read(file)
 head(dat, n=5)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
 file=make_filename(2013)
 file

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
 data_file<-system.file("extdata","accident_2014.csv.bz2", package = "farsdata")
 setwd(dirname(data_file))
 dat=fars_read_years(c(2013,2014))
 head(dat)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
 data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
 setwd(dirname(data_file))
 dat=fars_summarize_years(c(2013,2014,2015))
 dat

## ---- echo=TRUE, eval=TRUE, warning=FALSE--------------------------------
 data_file<-system.file("extdata","accident_2013.csv.bz2", package = "farsdata")
 setwd(dirname(data_file))
 library(maps)
 fars_map_state(48, 2014)

