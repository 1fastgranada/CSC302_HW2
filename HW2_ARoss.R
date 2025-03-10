#Posted on GitHub at https://github.com/1fastgranada/CSC302_HW2/

df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                      'Richards','George','Ema','Samantha','Catherine'),
               State=c('Alaska','California','Texas','North Carolina','California','Texas',
                       'Alaska','Texas','North Carolina','Alaska','California','Texas'),
               Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))

aggregate(df1$Sales, by=list(df1$State), FUN=sum)

library(dplyr)

df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))

#Use R to read the WorldCupMatches.csv from the DATA folder on Google Drive. Then perform the followings (48 points):
df = read.csv("C:/Users/User/OneDrive - Umich/15_CSC302 Intro to Data Visualization/Rscripts/WorldCupMatches.csv", header=T)
head(df)
# (a) Find the size of the data frame. How many rows, how many columns?
dim(df)

# (b) Use summary function to report the statistical summary of your data.
summary(df)

# (c) Find how many unique locations olympics were held at.
length(unique(df$City))

# (d) Find the average attendance.
df2 = df[is.na(df["Attendance"])==F, ] # create df2 which excludes any "attendance" entries with "NA"
mean(df2$Attendance) # find the average attendance of the remaining dataset

# (e) For each Home Team, what is the total number of goals scored? (Hint: Please refer to question 1)
aggregate(df$Home.Team.Goals, by=list(df$Home.Team.Name), FUN=sum)

# (f) What is the average number of attendees for each year? Is there a trend or pattern in the data in that sense?
aggregate(df2$Attendance, by=list(df2$Year), FUN=mean) #using df2 to throw out the years with "NA" attendance


#3.	Use R to read the metabolites.csv from the DATA folder on Google Drive. Then perform the followings (32 points): 
df = read.csv("C:/Users/User/OneDrive - Umich/15_CSC302 Intro to Data Visualization/Rscripts/metabolite.csv", header=T)
head(df)

#a.	Find how many Alzheimers patients there are in the data set. (Hint: Please refer to question 1)
sum(df$Label == "Alzheimer")

#b.	Determine the number of missing values for each column. (Hint: is.na( ) )
colSums(is.na(df))

#c.	Remove the rows which has missing value for the Dopamine column and assign the result to a new data frame. (Hint: is.na( ) )
df2 = df[is.na(df["Dopamine"])==F, ] # create df2 which excludes any "Dopamine" entries with "NA"
head(df2)

#d.	In the new data frame, replace the missing values in the c4-OH-Pro column with the median value of the same column. (Hint: there is median( ) function.)
df2$c4.OH.Pro[is.na(df2$c4.OH.Pro)] <- median(df2$c4.OH.Pro, na.rm=T)
head(df2)

#e.	(Optional) Drop columns which have more than 25% missing values. (Hint: when you slice your data frame, you can use -c(.., ..., ...) where ... represent one column name)
missing_values <- colSums(is.na(df2)) / nrow(df2)
columns2drop <- names(missing_values[missing_values > .25])
print(columns2drop)
#I couldn’t get the -c(…,…,…) to work with column names. I got the list of column names, but didn’t actually drop them.