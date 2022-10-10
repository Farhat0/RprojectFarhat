#Using the library dplyr functions
library(dplyr)

#Uploading the 401k local data set
myDataset = read.csv("E:\\Farhat\\University\\Fall 2022\\ITC 255 Statistical Data Science\\401k.csv")
varDescription = read.csv(url("http://fmwww.bc.edu/ec-p/data/wooldridge/401k.des"))
varDescription

#Case 1: implementing the functions of dplyr package on two qualitative variables
#First transferring quantitative data to qualitative:  prate(participation rate) to qlPrate and age to age group 
summary(myDataset$prate)
qlPrate = c()

for (k in 1:length(myDataset$prate)) {
  if(myDataset$prate[k]<90.00){
    qlPrate[k]="smallPrate"
  } else if (myDataset$prate[k] >=90.00 & myDataset$prate[k]<96.00) {
    qlPrate[k]="averagePrate"
  } else {
    qlPrate[k]="largePrate"
  }
}

cbind(myDataset$prate, qlPrate)

summary(myDataset$age)
ageGroup = c()

for (k in 1:length(myDataset$age)) {
  if(myDataset$age[k]<13){
    ageGroup[k]="smallAge"
  } else if (myDataset$age[k] >=13 & myDataset$age[k]<25) {
    ageGroup[k]="youngAge"
  } else {
    ageGroup[k]="adultAge"
  }
}

#Second adding qlPrate and ageGroup to the data set
myDataset$new = cbind(qlPrate, ageGroup)
head(myDataset)

#filter(); small age group participants who have large participation rate
PrateAndAge = filter(myDataset, new[,'qlPrate'] == 'largePrate' & new[,'ageGroup'] == 'smallAge')
head(smlPrateAndAge)

#arrange()
names(myDataset)
head(arrange(myDataset, new[,'ageGroup']))
head(arrange(myDataset, new[,'qlPrate']))
#select()
head(select(myDataset, new))
head(select(myDataset, -new))

#mutate()
head(mutate(myDataset, qualPrate = new[,'qlPrate']))
head(mutate(myDataset, particepantsAgeGroup = new[,'ageGroup']))

#summarize(); the participation rate mean of three different age groups
ageGroup = group_by(myDataset, new[,'ageGroup'])
summarise(ageGroup, mean(prate))

#pull()
head(pull(myDataset, new))

#sample_n()
dim(myDataset)
sample_n(myDataset, 10)

##Case 2: implementing the functions of dplyr package on 1 quantitative and 1 qualitative variable
#filter(); young participants who have a participation rate of 100
head(filter(myDataset, new[,'ageGroup'] == 'youngAge' & prate == 100))

#arrange()
head(arrange(myDataset, new[,'ageGroup']))
head(arrange(myDataset, age))

#select()
head(select(myDataset, c(prate, age, new)))

#rename()
head(rename(myDataset, participationRate = prate))

#summarize(); three level of participation rate with their match rate mean 
qlParticipationRate = group_by(myDataset, new[,'qlPrate'])
summarise(qlParticipationRate, mean(mrate))

###Case 3: implementing the functions of dplyr package on two quantitative variables 
#filter(); 7 years old participants who are either firm's sole plan or not
head(filter(myDataset, (sole == 1 | sole ==0) & age == 7))

#arrange()
head(arrange(myDataset, sole))
head(arrange(myDataset, desc(totelg)))

#select()
head(select(myDataset, totelg:age))

#rename()
head(rename(myDataset, totNumEmpl = totemp))

#mutate(); creating a variable for total number of participants plus total number of eligible people 
head(mutate(myDataset, totNumPpl = totpart +totelg ))

#summarize(); 
summarise(myDataset, mean(totemp), mean(ltotemp))

#pull()
head(pull(myDataset, totelg))




