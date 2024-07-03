
# The leadership data
# A researcher studied how men and women differ in the ways they lead their organizations. Typical questions might be:
  # •	Do men and women in management positions differ in the degree to which they defer to superiors?
  # •	Does this vary from country to country, or are these gender differences universal?
  # One way to address these questions is to have bosses in multiple countries rate their managers on deferential behavior, using questions like the following: This manager asks my opinion before making personnel decisions.
# 1	2	3	4	5
# strongly disagree	disagree	neither agree nor disagree	agree	strongly agree
# In the resulting data, each row represents the ratings given to a manager by his or her boss. Here, each manager is rated by their boss on five statements (q1 to q5) related to deference to authority. For example, manager 1 is a 32-year-old male working in the US and is rated deferential by his boss, while manager 5 is a female of unknown age (99 probably indicates missing) working in the UK and is rated low on deferential behavior. The date column captures when the ratings were made. Although a dataset might have dozens of variables and thousands of observations, we’ve only included 10 columns and 5 rows to simplify the examples. Additionally, we’ve limited the number of items pertaining to the managers’ deferential behavior to 5. In a real-world study, you’d probably use 10–20 such items to improve the reliability and validity of the results. You can create a data frame containing the data using the following code.
# Creating the leadership data frame
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, stringsAsFactors=FALSE)
leadership

# In order to address the questions of interest, we must first address several data management issues. Here’s a partial list:
  # •	The five ratings (q1 to q5) will need to be combined, yielding a single mean deferential score from each manager.
# •	In surveys, respondents often skip questions. For example, the boss rating manager 4 skipped questions 4 and 5. We’ll need a method of handling incomplete data. We’ll also need to recode values like 99 for age to missing.
# •	There may be hundreds of variables in a dataset, but we may only be interested in a few. To simplify matters, we’ll want to create a new dataset with only the variables of interest.
# •	Past research suggests that leadership behavior may change as a function of themanager’s age. To examine this, we may want to recode the current values of ageinto a new categorical age grouping (for example, young, middle-aged, elder).
# •	Leadership behavior may change over time. We might want to focus on deferen- tial behavior during the recent global financial crisis. To do so, we may want to limit the study to data gathered during a specific period of time (say, January 1, 2009 to December 31, 2009).
# Exercise 1
# Let’s say that you want to recode the ages of the managers in our leadership dataset from the continuous variable age to the categorical variable agecat (Young, Middle Aged, Elder).
# a)	First, recode the value 99 for age to missing with NA.
leadership$age[leadership$age == 99] <- NA
# b)	Change the variables manager to managerID and date to testDate.
names(leadership)[names(leadership) == "manager"] <- "managerID"
names(leadership)[names(leadership) == "date"] <- "testDate"

# c)	The date is coded as a character variable in mm/dd/yy format. Please replace it in the data frame as a date variable.
leadership$testDate <- as.Date(leadership$testDate, format="%m/%d/%y")

# d)	Create a new dataset containing rows sorted:
# •	from youngest manager to oldest manager.
sorted_by_age <- leadership[order(leadership$age, decreasing = TRUE), ]
sorted_by_age <- leadership[order(leadership$age, na.last = TRUE), ]
#I dont know why it failed to work....

leadership
# •	into female followed by male, and youngest to oldest within each gender.
sorted_by_gender_age <- leadership[order(leadership$gender, leadership$age), ]

# •	by gender, and then from oldest to youngest manager within each gender.
sorted_by_gender_age_desc <- leadership[order(leadership$gender, -leadership$age), ]

# e)	Select variables q1, q2, q3, q4, and q5 from the leadership data frame and save them to the data frame newdata.
newdata <- leadership[, c("q1", "q2", "q3", "q4", "q5")]

newdata
# Exercise 2
# Selecting or excluding observations (rows) is typically a key aspect of successful data preparation and analysis.
# a)	Select all men over 30 from the leadership dataframe.
men_over_30 <- leadership[leadership$gender == "M" & leadership$age > 30, ]
men_over_30
# b)	Select all rows that have a value of age greater than or equal to 35 or age less than 24 and keep the variables q1 through q4.
age_specific <- leadership[leadership$age >= 35 | leadership$age < 24, c("q1", "q2", "q3", "q4")]
age_specific
# c)	Select all men over the age of 25 and keep variables gender through q4 (gender, q4, and all columns between them). Hint: the colon operator from:to provides all variables in a data frame between the from variable and the to variable, inclusive.
men_over_25 <- leadership[leadership$gender == "M" & leadership$age > 25, c("gender", "q1", "q2", "q3", "q4")]
men_over_25

# d)	Take a random sample of size 3 from the leadership dataset without replacement.
set.seed(123) 
random_sample <- leadership[sample(nrow(leadership), 3), ]
random_sample
# e)	Transpose the leadership dataframe so that the column names (variable names) become the row names.
transposed_leadership <- t(leadership)
transposed_leadership
# f)	Aggregate the mtcars data by number of cylinders (cyl) and gears (gear), returning means on each of the numeric variables. The mtcars data is part of the R package (run help(mtcars) to view its description)
library(dplyr)
aggregated_mtcars <- mtcars %>%
  group_by(cyl, gear) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
aggregated_mtcars


# Exercise 3
# Consider the provided “state_income” data which contains hypothetical income generated by US states from year 2002 to 2015. For this exercise, please use use the package Dplyr. For a cheatsheet visit https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf or for direct access in Rstudio, please click Help -> Cheatsheets -> Data Transformation with Dplyr
# a)	Suppose you are asked to select only a few variables. Selects variables “Index”, columns from “State” to “Y2008”. Hint: use select() function.
library(rio)
state_income <- import("E:/Projects/Session 3/state_income.txt")
state_income
mydata1 <- state_income %>%
  select(Index, State:Y2008)

mydata1
# b)	Select (call this mydata2) and then drop variables (call this mydata3) starts with ‘Y’. Hint: The starts_with() function is used to select variables starts with an alphabet. Adding a negative sign before starts_with() implies dropping the variables starts with the alphabet.
mydata2 <- state_income %>%
  select(Index, starts_with("Y"))
mydata2
mydata3 <- state_income %>%
  select(-starts_with("Y"))
mydata3
# c)	Suppose you need to subset the data. Please filter rows and retain only those values in which Index is equal to A. Hint: use filter() function.
filtered_data <- state_income %>%
  filter(Index == "A")
filtered_data
# d)	Calculat the mean and median for the variable Y2015. Hint: use summarise() function
summary_stats <- state_income %>%
  summarise(mean_Y2015 = mean(Y2015, na.rm = TRUE), 
            median_Y2015 = median(Y2015, na.rm = TRUE))
summary_stats
# e)	Calculating count and mean of variables Y2011 and Y2012 by variable Index.
count_mean_stats <- state_income %>%
  group_by(Index) %>%
  summarise(count_Y2011 = n(), mean_Y2011 = mean(Y2011, na.rm = TRUE),
            count_Y2012 = n(), mean_Y2012 = mean(Y2012, na.rm = TRUE))
count_mean_stats

# Exercise 4: combining datasets using join() function
# Generate the two datasets by running the following codes:
  df1 = data.frame(ID = c(1, 2, 3, 4, 5),
                   w = c('a', 'b', 'c', 'd', 'e'),
                   x = c(1, 1, 0, 0, 1),
                   y=rnorm(5),
                   z=letters[1:5])
df2 = data.frame(ID = c(1, 7, 3, 6, 8),
                 a = c('z', 'b', 'k', 'd', 'l'),
                 b = c(1, 2, 3, 0, 4),
                 c =rnorm(5),
                 d =letters[2:6])
df1
df2
# a)	Merge df1 and df2 with ID as common variable (primary key). Hint: use inner_join
joined_data <- inner_join(df1, df2, by = "ID")
joined_data
# b)	Select all rows from the left table (df1), even if there are no matches in the right table (df2). Hint: use left_join
left_joined_data <- left_join(df1, df2, by = "ID")
left_joined_data
# c)	Include rows of df1 that match df2 but only keep the columns from df1. Hint: use semi_join()
semi_joined_data <- semi_join(df1, df2, by = "ID")
semi_joined_data

# d)	Select the opposite of what was selected in c) above. Hint: use anti_join()
anti_joined_data <- anti_join(df1, df2, by = "ID")
anti_joined_data
