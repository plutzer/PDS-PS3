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

#2
#Start again with the original data
data = read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
data$start_date<-as.Date(data$start_date, "%m/%d/%Y")

new_data = data %>%
  filter(candidate_name %in% frontrunners) %>%
  select(candidate_name,state,pct,start_date)
library(pryr)
object_size(new_data)
pivoted = pivot_wider(new_data,names_from = start_date,values_from = pct)
object_size(pivoted)
# This gives 211 rows for 6 candidates, meaning that not every possible combo of candidate-state actually shows up.

#3
library(fivethirtyeight)
polls = read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements = endorsements_2020
Endorsements = rename(Endorsements,candidate_name = endorsee)
Endorsements = as_tibble(Endorsements)
#Filtering and subsetting the polls data
polls = polls %>%
  filter(candidate_name %in% frontrunners) %>%
  select(candidate_name,sample_size,start_date,party,pct)
#Two discrepancies between the data...
polls$candidate_name = recode(polls$candidate_name, "Bernard Sanders" = "Bernie Sanders", "Joseph R. Biden Jr." = "Joe Biden")
#Doing an inner join of these two datasets (i have no idea why you want us to do this...)
joined_data = inner_join(Endorsements, polls, by = 'candidate_name')
#Counting the number of endorsements
num_endorsements = Endorsements %>%
  count(candidate_name) %>%
  filter(candidate_name %in% polls$candidate_name)
# Plotting
library(ggplot2)
p = ggplot(num_endorsements, aes(x = candidate_name,y = n)) + geom_bar(stat = "identity")
p = p + theme_dark()
p + ylab("Number of Endorsements") + xlab("Candidate")

#4
