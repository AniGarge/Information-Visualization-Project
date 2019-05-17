fname<-file.choose()
fname
fifa_data<-read.csv(fname,header = T,sep = ',')
install.packages("ggplot2")
View(fifa_data)
library(ggplot2)
library(stringr)
library(dplyr)
data <- fifa_data[, -which(names(fifa_data) %in% c("ï..","Photo","Flag","Club.Logo","Real.Face"))]
sum(is.na(data))
data <- na.omit(data)
data <- fifa_data[, -which(names(fifa_data) %in% c("ï..","Photo","Flag","Club.Logo","Real.Face"))]
View(data)
data$Value <- sapply(strsplit(as.character(data$Value),"â,¬"),tail,1)
data$Wage <- sapply(strsplit(as.character(data$Wage),"â,¬"),tail,1)
data$Release.Clause <- sapply(strsplit(as.character(data$Release.Clause),"â,¬"),tail,1)
View(data)

data$Value_last <- (str_sub(as.character(data$Value),-1,-1))
data$Wage_last <-  (str_sub(as.character(data$Wage),-1,-1))
data$Release.Clause_last <- (str_sub(as.character(data$Release.Clause),-1,-1))

table(data$Value_last)
table(data$Release.Clause_last)
table(data$Wage_last)

data <- data[which(data$Value_last=='M'|data$Value_last=='K'),]
data <- data[which(data$Wage_last=='M'|data$Wage_last=='K'),]
data <- data[which(data$Release.Clause_last=='M'|data$Release.Clause_last=='K'),]


extract <- function(x){
  regexp <- "[[:digit:]]+"
  str_extract(x, regexp)
}
data$Value <- sapply(data$Value,extract)
data$Wage <- sapply(data$Wage,extract)
data$Release.Clause <- sapply(data$Release.Clause,extract)

data$Value <- as.numeric(data$Value)
data$Wage <- as.numeric(data$Wage)
data$Release.Clause <- as.numeric(data$Release.Clause)
View(data)

data$Value <- ifelse(data$Value_last == 'M',(data$Value)*1000000,(data$Value)*1000)
data$Wage <- ifelse(data$Wage_last == 'M',(data$Wage)*1000000,(data$Wage)*1000)
data$Release.Clause <- ifelse(data$Release.Clause_last == 'M',(data$Release.Clause)*1000000,(data$Release.Clause)*1000)

data <- data[, -which(names(data) %in% c("Value_last","Wage_last","Release.Clause_last"))]
table(data$Age)

for(i in 1:nrow(data)){
  sample <- data[i,]
  
  if(sample$Age >=16 & sample$Age <=20){
    data[i,'Age_Bin'] <- '16-20 years'
  } else if(sample$Age >=21 & sample$Age <=25){
    data[i,'Age_Bin'] <- '21-25 years'
  } else if(sample$Age >= 26 & sample$Age <= 30){
    data[i,'Age_Bin'] <- '26-30 years'
  } else if (sample$Age >= 31 & sample$Age <= 35){
    data[i,'Age_Bin'] <- '31-35 years'
  } else {
    data[i,'Age_Bin'] <- '35+ years'
  }
}

Plot1 <- ggplot(data = data, aes(data$Age)) + geom_histogram(color='blue',fill = 'lightblue',bins = 20) +
  labs(x='Age',y='Count',title='Histogram showing the distribution of Age in the dataset') + 
  geom_vline(aes(xintercept=mean(data$Age)),color="skyblue", linetype="dashed", size=0.8)
Plot1

data %>%
  select(Overall,Age_Bin) %>%
  group_by(Age_Bin) %>%
  summarise(n=mean(Overall, na.rm = TRUE)) %>%
  ggplot(aes(x=reorder(Age_Bin,n),y=n)) + geom_bar(stat = 'identity',position = 'identity',fill='lightblue',color='black') + coord_flip() +
  labs(x='Mean Overall Rating',y='Age Bin',title='Comparison of Overall Rating with Age') + 
  geom_text(aes(label=round(n,2)),position=position_stack(vjust=0.5)) 

ggplot(data,aes(x=Overall,y=Value)) + geom_point(alpha=0.2,color='yellow') + 
  labs(x='Overall Rating',y='Value',title='Comparing Market Value of Players with Overall Rating')

Fifa %>%
  group_by(Club)%>%
  summarise(Total.Wage = round(sum(Wage)/1000000, digits =2))%>%
  arrange(-Total.Wage)%>%
  head(10)%>%
  ggplot(aes(x = as.factor(Club) %>%
               fct_reorder(Total.Wage), y = Total.Wage, label = Total.Wage))+
  geom_text(hjust = 0.01,inherit.aes = T, position = "identity")+
  geom_bar(stat = "identity", fill = "violetred1")+
  coord_flip()+
  xlab("Club")+
  ylab("Squad Wages in Million")

class(data$Wage)
install.packages("xlsReadWrite")


subset_wage<-aggregate(data$Wage,list(data$Club),sum)
subset_wage
susbset_values<-aggregate(data$Value,list(data$Club),sum)
susbset_values
subset_wage$value<-susbset_values$x
subset_wage


write.csv(subset_wage,'new_data.csv')
