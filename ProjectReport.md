# PMQ Report
Kipp Johnson  
May 4, 2015  

Reading in the data from my Github:

```r
tmp <- tempfile()
download.file("https://raw.githubusercontent.com/kippjohnson/PMQ/master/SurveyResponses.csv", destfile=tmp, method="curl")
infile <- read.csv(tmp,header=TRUE)
```

We have 212 observations, with 33 possible variables per observation.

