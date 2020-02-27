rm(list = ls())
library(ggplot2)
library(dplyr)

#1
#load the dataset
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")

#Filter by states on super tuesday
superTuesday = c('Alabama','Arkansas','California','Colorado','Maine','Massachusetts','Minnesota','North Carolina','Oklahoma','Tennesee','Texas','Utah','Vermont')
primaryPolls = primaryPolls %>%
  filter(state %in% superTuesday)

#Filter by the top 6 frontrunner candidates as of Feb 26
frontrunners = c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg")
primaryPolls = primaryPolls %>%
  filter(candidate_name %in% frontrunners)

#Lets add in the number of delegates associated with each state
state = superTuesday
delegates = c(52,31,415,67,24,91,75,110,37,64,228,29,16)
primaryPolls = left_join(primaryPolls,data.frame(state,delegates,stringsAsFactors = F),by = 'state')

ggplot(primaryPolls) +
  geom_point(mapping = aes(x = start_date,y = pct,color = delegates,),alpha = .8) +
  geom_smooth(mapping = aes(x = start_date,y = pct),se = F,color = 'violet',alpha = .6) + 
  facet_wrap(~ candidate_name,nrow = 2) +
  theme_minimal() +
  labs(title = "Super Tuesday Polls by Date for Democratic Race Frontrunners") + 
  labs() + 
  xlab("Start Date") + 
  ylab("Polling Percentage") + 
  labs(color = "State \nDelegates")


