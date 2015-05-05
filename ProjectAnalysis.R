## Attitudes Toward Personalized Genomics Testing Data Analysis
library(plyr)
library(psych)
rm(list=ls())

sums <- function(somevector){
  n <- length(somevector)
  xbar <- mean(somevector)
  s <- sd(somevector)
  mn <- min(somevector)
  mx <- max(somevector)

  return(c(n, xbar, s, mn, mx))
}

# Read in data from Github
tmp=tempfile()
download.file("https://raw.githubusercontent.com/kippjohnson/PMQ/master/SurveyResponses.csv", destfile=tmp, method="curl")
infile = read.csv(tmp,header=TRUE)

### Compute Demographic Information
# Q26 = Age range
table(infile$Q26)

# Q27 = Gender
table(infile$Q27)
table(infile$Q27, exclude=NULL)

# Q28.2 = year of medical school
table(infile$Q28.2)
table(infile$Q28.2, exclude=NULL)

# Q29.1 = dual degree program? (1=yes)
table(infile$Q29.1)

# Q30.1 = research interest (1=yes)
table(infile$Q30.1)

### ACtually do demographics
nMales = table(infile$Q27)[[1]]
nFemales = table(infile$Q27)[[2]]
nNoSex = table(infile$Q27,exclude=NULL)[[3]]

nMS1 = table(infile$Q28.2)[[1]]
nMS2 = table(infile$Q28.2)[[2]]
nMS3 = table(infile$Q28.2)[[3]]
nMS4 = table(infile$Q28.2)[[4]]
nMSNone = table(infile$Q28.2,exclude=NULL)[[5]]

nDual = table(infile$Q29.1)[[1]]
nResearch = table(infile$Q30.1)[[1]]

demog <-  data.frame(c(nMales,nFemales,nNoSex,nMS1,nMS2,nMS3,nMS4,nMSNone,nDual,nResearch))
colnames(demog) <- "Number of Students"
row.names(demog) <- c("Male","Female","No Sex Given","MS1","MS2","MS3","MS4","No Year Given","Total Dual Degree","Total with Research Interest")

demog <- mutate(demog, PercentTotal = demog[,1] / 212)

kable(demog, digits=2)



### Space to print a demographics table

#Because questions 2,4,5,6 and reversed on our likert scale, we need to convert them back to have
# the same meaning (i.e., change 5-->1 , 3-->3, and 1-->5)
divergence_corrected <- data.frame(c( abs( infile[,c(3,5,6,9)])))
infile1 <- data.frame(c(infile[,c(2,4,7,8,10,11,12,13)] ,divergence_corrected))

### Compute EBPAS Scores
# First, add a column with the total EBPAS Score
# This is calculated by summing the first 12 questions
infile2 <- mutate(infile1, EBPAS = Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q11+Q12)
length(which(is.na(infile2$EBPAS))) # There are 18 people who did not completely fill out the survey

#Generate a new file (infile3), completely dropping those observations who did not fill out the entirety of Questions 1-12
infile3 <- infile2[-which(is.na(infile2$EBPAS)),]
infile3 <- mutate(infile3, openness = Q1+Q7+Q10+Q12)
infile3 <- mutate(infile3, divergence = Q2+Q4+Q5+Q8)
infile3 <- mutate(infile3, education = Q3+Q6+Q9+Q11)

# Calculate N, Mean, SD, Min, Max for EBPAS questions, and for subsets
EBPASstats <- sums(infile3$EBPAS)
opennessstats <- sums(infile3$openness)
divergencestats <- sums(infile3$divergence)
educationstats <- sums(infile3$education)

# Compute Cronbach's alpha for each subset of EBPAS
# Note: EBPAS alpha value is taken from Overby et al., J Pers. Med. 2014
EBPAS_alpha <- 0.78
openness_alpha <- alpha(infile3[,c("Q1","Q7","Q10","Q12")]) # alpha=0.81
divergence_alpha <- alpha(infile3[,c("Q2","Q4","Q5","Q8")]) # alpha=0.55
education_alpha <- alpha(infile3[,c("Q3","Q6","Q9","Q11")]) # alpha=0.53

EBPASstats <- c(EBPASstats, EBPAS_alpha)
opennessstats <- c(opennessstats, openness_alpha$total[[1]])
divergencestats <- c(divergencestats, divergence_alpha$total[[1]])
educationstats <- c(educationstats, education_alpha$total[[1]])

### Print a Table with these results
EBPAStable <- t(data.frame(EBPASstats,educationstats, divergencestats, educationstats))
colnames(EBPAStable)  <- c("N","Mean","SD","Min","Max","Alpha")
rownames(EBPAStable) <- c("EBPAS", "education", "divergence","openness")
kable(EBPAStable)

### Frequency Calculations

# Attitudes
table(infile$Q13) # Have heard of DTC: 50 no, 101 yes
table(infile$Q14) # Would use DTC: 73 would not use, 121 would use, 9 did use
table(infile$Q15) # Can interpret DTC: 35 str. disagree, 30 dis, 72 uncertain, 58 agree, 14 str. agree
table(infile$Q16) # Vast majority comfortable with computers
table(infile$Q17) # Comfort with using EPIC

table(infile$Q18) # Comfort with basic genomic testing concepts and terminology
table(infile$Q19) # Comfort with pharmacogenomics knowledge
table(infile$Q20) # Comfort with knowledge about genetic variation
table(infile$Q21) # Comfort with knolwedge about NGS

table(infile$Q22) # Ability to recommend genetic testing
table(infile$Q23) # Ability to understand genomic test results
table(infile$Q24) # Ability to explain genomic test results to patients
table(infile$Q25) # Ability to make treatment recommendations based upon genomic test results


### T Tests

t.test(infile3$EBPAS ~ infile3$Q13) #NS

summary(aov(infile3$EBPAS ~ (infile3$Q14))) #NS
  boxplot(EBPAS ~ Q14, data=infile3)

summary(aov(infile3$EBPAS ~ (infile3$Q15))) #significant
  boxplot(EBPAS ~ Q15, data=infile3)


