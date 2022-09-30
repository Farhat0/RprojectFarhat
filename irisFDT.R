##Constructing FDT of a numerical variable from iris data
#Uploading iris data from the web 
dfIris=read.csv(url('https://gist.githubusercontent.com/netj/8836201/raw/6f9306ad21398ea43cba4f7d537619d0e07d5ae3/iris.csv'))
head(dfIris)

#Specifying
names(dfIris)

#sepal.length
summary(dfIris$sepal.length)

#defining categories: <6 is small, 6 >= sepal length >7 is medium, otherwise large
 #Selection and Loop 
catSLength = c()

for (a in 1:length(dfIris$sepal.length)){
  if (dfIris$sepal.length[a]<6){
    catSLength[a] = 'smallLength'
  }else if (dfIris$sepal.length[a]<= 6 & dfIris$sepal.length[a]<7){
    catSLength[a] = 'mediumLength'
  }else {
    catSLength[a] = 'largeLength'
  } 
}
    
sLengthAmount = cbind(dfIris$sepal.length, catSLength)
View(sLengthAmount)
  

   

