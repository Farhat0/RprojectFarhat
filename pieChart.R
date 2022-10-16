install.packages("ggplot2")
library(ggplot2)

#Creating pie chart for age group 
##first creating the data frame for age group vairable 
fdtAgeGroup = table(myDataset$new[,"ageGroup"])
fdtAgeGroup = as.data.frame(fdtAgeGroup)
fdtAgeGroup
colnames(fdtAgeGroup) = c("Age_Group", "Count")

##second plotting the pie chart
pieChart=ggplot(fdtAgeGroup, aes(x="", y=Count, fill=Age_Group))
pieChart1=pieChart+geom_col()+
  coord_polar(theta = "y")+
  theme_void()+
  theme(plot.title = element_text(colour = "black",
                                  size = 14, 
                                  face = "bold", 
                                  hjust = .5))+
  ggtitle('Age Group Distribution of Participants')+
  geom_text(aes(label=Count), 
            position = position_stack(vjust = .5))+
  scale_fill_manual(values = c('#993399', '#CC99CC','#FFCCFF'))
pieChart1
 ggsave('AgeGroupPieChart.png')

