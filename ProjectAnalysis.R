
## Attitudes Toward Personalized Genomics Testing Data Analysis
tmp=tempfile()
download.file("https://raw.githubusercontent.com/kippjohnson/PMQ/master/SurveyResponses.csv", destfile=tmp, method="curl")
read.csv(tmp,header=TRUE)
