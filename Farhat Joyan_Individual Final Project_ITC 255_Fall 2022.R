#Farhat Joyan - ID 43568 - ITC 255 - Fall 2022
#Professor Asadullah Jawid 
#Final Individual Project 


#For this project, I am using Binary Logistic Regression to study the relationship between two variables
#x:customers income and y:customers making a purchase or not to see if y is dependent on x or not


#The first step is to generate my sample data on both x and y using rbinom and rnorm functions 

y = rbinom(1000, 1, prob = 0.5)#the chance of a customer making a purchase or not
x = rnorm(1000, 50, 10)#customers' income

#The second step is to create the data frame for my dataset 

df = data.frame(Customer_Chance_of_Purchase = y, Customer_Income = x)#creating the data frame for customers income and possibility of purchasing
View(df)

#In order to apply Binary Logistic Regression, I will use Generalized Linear Model to estimate a, b and p-value

help(glm)
Generalized_lm = glm(df$Customer_Chance_of_Purchase~df$Customer_Income, family = 'binomial')
summary(Generalized_lm)

#a and b estimation:
#ahat = 0.127
#bhat = -0.004<0 (with income increasing the chance of purchasing goes down which is true for sample data)
# in order to see it for our population, we have to estimate the p-value)

#Hypothesis Testing:
#H0:b=0//there is no relationship between customers' income and the chance of them purchasing 
#H1:b<0//there is an indirect relationship between customers' income and the chance of them purchasing

#Making inferences:
#Alpha = 0.05
#p-value = 0.588

### p-value > alpha hence we do not reject H0 

######################################################################################################################

#changing Customer_Chance_of_Purchase binary variable to a qualitative variable and adding it to our data set 
Customer_Chance_of_Purchase_QL = c()
for(a in 1:length(df$Customer_Chance_of_Purchase) ) {
  if(df$Customer_Chance_of_Purchase[a]==0){
    Customer_Chance_of_Purchase_QL[a]="No Purchase"
  }else {
    Customer_Chance_of_Purchase_QL[a]="Yes Purchase"
  }
}
df = cbind(df, Customer_Chance_of_Purchase_QL)
View(df)

#######################################################################################################################

#Using Bar Chart to visualize the number of customers either purchasing or not

library(ggplot2)
Customer_table=table(df$Customer_Chance_of_Purchase_QL)
Customer_table=as.data.frame(Customer_table)
colnames(Customer_table)=c('Customer_Purchasing', 'Count')

g0=ggplot(Customer_table, aes(x=Customer_Purchasing, y=Count, fill=Customer_Purchasing))
g0+geom_bar(stat='identity')+
  theme_classic()+
  theme(legend.position = '')+
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(),
        plot.title = element_text(face = 'bold', hjust=.5))+
  ggtitle('Customer Purchasing Rate')+
  geom_text(aes(label = Count), vjust=2)+
  scale_fill_manual(values=c('#FF0000', '#FF3366'))
ggsave('MyBar.pdf')

#######################################################################################################################

#Using histogram to plot the density graph of customers' Income 
g0=ggplot(df, aes(x=Customer_Income))
g0+geom_histogram(bins = 10, fill='#00CC99', colour=4)+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title.x = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Income Distribution')+
  xlab('Income')+
  ylab('Customer No')+
  geom_vline(xintercept = 50.32813,
             linetype='dashed',
             color='red', 
             size=1)

ggsave('Customer_Income_His.png')

########################################################################################################################
#Plotting joint distribution graph of Customer Income and Purchase using ggridges package 
install.packages('ggridges')
library(ggridges)

ggplot(df, aes(x=Customer_Income, y=Customer_Chance_of_Purchase_QL, fill=Customer_Chance_of_Purchase_QL))+
  geom_density_ridges(color=4, 
                      lwd=.3)+
  theme_gray()+
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'))+
  ggtitle('Joint dist of Customers Income and Purchasing')+
  xlab('Income')
ggsave('Customer_Income_Purchase_JointDist.pdf')
