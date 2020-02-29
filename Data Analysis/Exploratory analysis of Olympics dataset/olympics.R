
setwd('/Users/suguthansekar/Large files/Datasets/120-years-of-olympic-history-athletes-and-results/Work folder')
list.files(pattern = '*.csv')
noc <- read.csv('noc_regions.csv')
df_left <- read.csv('athlete_events.csv')
df_right <- read.csv('BirthdaysFinal.csv')
df_right <- read.csv('example.csv')

df_left$Name <- tolower(df_left$Name)
df_right$Name <- tolower(df_right$Name)

mydf <- read.csv('myBirthday2.csv')
mydf1 <- read.csv('myBirthday3.csv')
library(dplyr)
library(stringr)
library(magrittr)
library(fuzzyjoin)

str(events)

(events$Name)
str(events)
str(noc)
head(noc)
length(events$ID)
head(events$Name)
events$Name
AFG = events[events$NOC == "AFG"] 
?filter 
afg <- filter(events$NOC == "AFG")
iris

str(events)
events %>%
  filter(Name=='Stefanie')
AFG %>% filter(events, NOC %in%"AFG")
str(AFG)
?join
head(events$Name)
str(iris)
head(iris$Species)
myhist <- hist(iris$Sepal.Width)
myhist$equidist

hist(iris$Sepal.Width)
cor(iris)
iris1  <- iris %>%
  select(-Species)

iris %>% 
  select(-Species) %>%
  cor

cor(iris1)

# Questions
myModel <- glm(iris$Species ~ iris$Sepal.Length + iris$Sepal.Width + iris$Petal.Length + iris$Petal.Width)
summary(myModel)
myModel
plot(iris$Petal.Width, iris$Species)
iris%>%
  factor(iris$Species)


# VLOOKUP of birthdays into events 
colnames(birthdays)[2] <- "Name"

test1 <- merge(events[1:1000,],birthdays[1:1000,], all.x = TRUE, match = TRUE)
test2 <- fuzzy_join(events, birthdays, match_fun = NULL, multi_by = NULL, 
                    multi_match_fun = NULL,
                    index_match_fun = NULL)
test3 <- stringdist_inner_join(events[1:100000,], birthdays[1:100000,])

sum(is.finite(test1$birthdate))
sum(is.finite(test3$birthdate))
head(test3$birthdate,100)
str(test3)
events[1:100,]

sum(is.finite(test1$Name))
head(test1$birthdate,25)
?stringdist_inner_join
head(events)
head(test1)
str(test1)
str(birthdays)
agrep("text","tetx")
agrep(events$Name, birthdays$Name)

?agrep
is.factor(birthdays$names)
xxx = "This is a test"

# to remove middle name 

df_left$Name <- sub('^(\\w+).*\\b(\\w+)$', '\\1 \\2', df_left$Name)
df_right$Name <- sub('^(\\w+).*\\b(\\w+)$', '\\1 \\2', df_right$Name)
head(df_left)
write.csv(df_left,'df_left.csv')
write.csv(df_right, 'df_right.csv')


# Example Kernels 

# ------------------------------------------------------------------------------------------

# Things that I can analyse:
# Check notes app 

# Birthday analysis 

str(mydf)
dfHockeyCAN <- mydf %>%
  filter(Sport == "Hockey",NOC == 'CAN') 
  

testdf <- events %>%
  filter(Sport == "Hockey",NOC == 'CAN')
#count(.,birthmonth)
  #select(birthmonth) %>%
count(mydf,birthmonth)
unique(mydf$Sport)
#  group_by(12) %>%

#  summarize(count = n())

unique(mydf$Sport)
unique(mydf$NOC)


testdf <- mydf1 %>%
  filter(is.na(birthyear))

#--------------------------------------------------------------------------------------------
# Merging datasets. 
?merge
# Reading CSV 
df_left <- read.csv('athlete_events.csv', encoding = "ASCII")
# df_right1 <- read.csv('BirthdaysFinal.csv')
df_right <- read.csv('example.csv', encoding = 'UTF-8')
warnings()
str(df_right)

df_left$Name <- tolower(df_left$Name)
df_right$Name <- tolower(df_right$Name)

mergeddf <- merge(df_left, df_right, all.x = TRUE)
str(mergeddf)
canadadf <- mergeddf %>%
  filter(Team == 'Canada',Sport == 'Hockey', Event == "Hockey Men's Hockey")

mergedftidy <- df_left %>%
  left_join(.,df_right)

sum(is.na(mergedftidy$birthyear))
write.csv(mergedftidy,"mergedftidy_wo_midname.csv")

sum(is.na(mergedftidy$birthyear))


mergedftidy <- left_join(df_left, df_right, by="Name")
str(mergedftidy)

  

df_right %>%
  filter(Name == "gunnar aaby")

merge_antijoin <- anti_join(df_left, df_right,by = "Name")
str(merge_antijoin)
?left_join
str(mergeddf)


name <- c('Jim', 'Jim', 'tom')
region <- c('East', 'West', 'North')
df1 <- data.frame(name, region)
name <- c('Jim', 'Kim', 'Jim')
type <- c('Urban', 'Rural','urban1' )
df2 <- data.frame(name, type)
test_left <- left_join(df1,df2, by = "name", all.x = TRUE)
test_left

sum(is.na(mergeddf$birthdate))
str(df_left)
str(df_right)
# Kernels
# 1. https://www.kaggle.com/marcogdepinto/let-s-discover-more-about-the-olympic-games


head(df_right)
df_right$Name <- df_right$Name %>% 
  iconv("UTF-8","ASCII", sub="")
