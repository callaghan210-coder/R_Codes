# Session 2 Exercises
# Prerequisites
# 1.	You should have installed the following packages by running the following code. You should be connected to a stable internet during the installation (Note: Remove the # comment symbols to run the code line)
                                                                                                                                                      install.packages("writexl", dependencies = TRUE); install.packages("foreign", dependencies = TRUE); install.packages("haven", dependencies=TRUE);
                                                                                                                                                       # 2.	Download and save all the files sent in the session 2 email
                                                                                                                                                         # 3.	Set your working directory to the folder you have saved today’s files sent via email
getwd()                                           setwd("E:/Projects/Session 2")                                                                      # Exercise 1
# a)	Use the c() / concatenate function to create a “neurosis” dataframe consisting of the following variables. The datapoints in the variables are arranged in the same order for each case (name)

# •	Variable “name” “Ben”, “Martin”,“Andy”,“Paul”, “Graham”,“Carina”,“Karina”,“Doug”,“Mark”, “Zoe”
                                                  
                                                                                                      name <- c("Ben", "Martin","Andy","Paul", "Graham","Carina","Karina","Doug","Mark", "Zoe")
                                                                                                      
# •	Variable “birth_date”
# “1977-07-03”, “1969-05-24”, “1973-06-21”, “1970-07-16”, “1949-10-10”, “1983-11-05”, “1987-10-08”, “1989-09-16”, “1973-05-20”, “1984-11-12”
                                                                                                      birth_date <- c("1977-07-03", "1969-05-24", "1973-06-21", "1970-07-16", "1949-10-10", "1983-11-05", "1987-10-08", "1989-09-16", "1973-05-20", "1984-11-12")
                                                                                                      
# •	Variable “marriage_date” “1997-04-03”, “1999-04-24”, “2003-12-25”, “1996-03-11”, “1976-05-07”, “2010-03-02”, “2012-02-01”, “2011-10-05”, “1996-03-12”, “2009-02-01”

                                                                                                      marriage_date <- c("1997-04-03", "1999-04-24", "2003-12-25", "1996-03-11", "1976-05-07", "2010-03-02", "2012-02-01", "2011-10-05", "1996-03-12", "2009-02-01")
# •	Variable “friends” 5,2,0,4,1,10,12,15,12, 17
                                                                                                      friends <- c(5,2,0,4,1,10,12,15,12, 17)
# •	Variable “alcohol” 10,15,20,5,30,25,20,16,17,18
                                                                                                      alcohol <- c(10,15,20,5,30,25,20,16,17,18)
                                                                                                      
# •	Variable “initial_income” 20000,40000,35000,22000,50000,5000,100,3000,10000,10
                                                                                                      initial_income <- c(20000,40000,35000,22000,50000,5000,100,3000,10000,10)
# •	Variable “neurotic” 10,17,14,13,21,7,13,9,14,13
                                                                                                      neurotic<- c(10,17,14,13,21,7,13,9,14,13)                                                 
# b)	You collect another income variable after 5 years. Please add this variable to the “neurosis” dataset using the c() function. The new income variable is as follows:
                                                                                                     neurosis$sex <- data.frame(name, birth_date, marriage_date, friends, alcohol, initial_income, neurotic)

  # •	Variable “current_income” 30000,55000,50000,35000,65000,15000,1500,5500,23000,510
                                                                                                      current_income <- c(30000, 55000, 50000, 35000, 65000, 15000, 1500, 5500, 23000, 510)
                                                                                                      neurosis$current_income <- current_income
                                                                                                      
# c)	Calculate and add a new variable “income_increase” to the “neurosis” dataset which expresses the percent increase in income since employment Hint: percent increase is ((initial_income - current_income)/ initial_income) * 100
                                                                                                      neurosis$income_increase <- ((neurosis$current_income - neurosis$initial_income) / neurosis$initial_income) * 100


# d)	Calculate the age at marriage for each of the study participants
                                                                                                      neurosis$birth_date <- as.Date(neurosis$birth_date)
                                                                                                      neurosis$marriage_date <- as.Date(neurosis$marriage_date)
                                                                                                      neurosis$age_at_marriage <- as.numeric(difftime(neurosis$marriage_date, neurosis$birth_date, units = "weeks")) / 52.25
                                                                                                      neurosis$age_at_marriage                                                                                                    
# e)	Compute and add a coding /factor variable “sex” where the first half of the participants are female and the next half are male.
                                                                                                      neurosis$sex <- factor(c(rep("Female", length.out = length(neurosis$name)/2), rep("Male", length.out = length(neurosis$name)/2)))
                                                                                                      head(neurosis) 
                                                                                                      summary(neurosis)
                                                                                                      str(neurosis)

# Exercise 2
# The data airquality which is already in your R environment presents daily air quality measurements in New York, May to September 1973. As is typical of many data collection exercises, this data contains missing values.
# a)	Calculate the mean Ozone concentration
# b)	List rows of the data that have missing values
# c)	Create a new dataset “Ozone_2” without missing data and use it to calculate the mean and standard deviation of the wind speed, temperature and ozone concentration
?airquality
View(airquality)
is.na(airquality)
mean(airquality$Ozone, na.rm = TRUE)
#[1] 42.12931
is.na(airquality)
# Exercise 3
# The dataset “Soil_composition.xlsx” consists of soil characteristics that were measured on samples from three types of contours (Top, Slope, and Depression) and at four depths (0-10cm, 10-30cm, 30-60cm, and 60-90cm). The area was divided into 4 blocks, in a randomized block design.
# a)	Save the dataset as a Comma delimited file (with a .csv file extension) extension and as a Text file (Tab Delimited with .txt file extension)
install.packages("rio")
library(rio)

soil_data <- import("E:/Projects/Session 2/Soil_composition.xlsx")

export(soil_data, "Soil_compositions.csv")

export(soil_data, "Soil_compositions.txt", format = "tsv")

Soil_compositions_csv <- import("Soil_compositions.csv")

Soil_compositions_txt <- import("Soil_compositions.txt")


# b)	Import and view (using View()) the two datasets from a) above into R and name them Soil_compositions_csv (for csv) and Soil_compositions_txt (for text)

View(Soil_compositions_csv)
View(Soil_compositions_txt)
# c)	Import and view (using View()) the dataset directly into R naming it Soil_compositions_xlsx
Soil_compositions_xlsx <- import("Soil_composition.xlsx")


View(Soil_compositions_xlsx)

# Exercise 4
# The dataset “Monitoring_the_future.sav” presents surveys of 8th- and 10th-grade students are part of a series that explores changes in important values, behaviors, and lifestyle orientations of contemporary American youth. You are presented with this data after entry in SPSS and need to analyze it in R.
# a)	Import the dataset into R
library(haven)
Monitoring_the_future <- read_sav("E:/Projects/Session 2/Monitoring_the_future.sav")
# b)	Examining the first and last 5 rows in the dataset.
head(Monitoring_the_future, 5)
tail(Monitoring_the_future, 5)

# c)	Examining the data by listing the variable names, looking at the structure of the data, number of rows and columns.

names(Monitoring_the_future)
str(Monitoring_the_future)
nrow(Monitoring_the_future)
ncol(Monitoring_the_future)



# Exercise 5
# Export the dataset created in Exercise 1 to:
  # a)	A tab delimited text file
write.table(neurosis, "neurosis.txt", sep = "\t", row.names = FALSE)
# b)	An Excel spreadsheet
install.packages("writexl")
library(writexl)
write_xlsx(neurosis, "neurosis.xlsx")



# Exercise 6
# This exercise is optional depending on availability of own data
# Repeat Exercise 4 using your own dataset and if necessary compute and append any necessary new variables as done in Exercise 1
