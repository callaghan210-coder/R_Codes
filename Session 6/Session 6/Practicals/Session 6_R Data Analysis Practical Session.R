#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

######Initiate packages
##if you have problems installing package ggm start
install.packages("BiocManager")
BiocManager::install("graph")

install.packages("ggm")
library(ggm)
##if you have problems installing package ggm end

install.packages("pacman")         # Install pacman package if you have not

#Run the following R code:
pacman::p_load(pacman, Hmisc, polycor, ggm, ggplot2, boot)

#After executing this, we have loaded the packages Hmisc, polycor, ggplot2, boot and ggm.


#--------Entering data----------
#Packets of sweets taken for each number of adverts watched 

adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15) 
advertData<-data.frame(adverts, packets)

# Covariance
cov(advertData)

#Variance
var(adverts)


#-----Dealing with misisng cases

adverts<-c(5,4,4,6,8)
packetsNA<-c(8,9,10,NA,15)
age<-c(5, 12, 16, 9, 14)
advertNA<-data.frame(adverts, packetsNA, age)

library(car)
?scatterplot
scatterplot(adverts ~ age, data=advertNA, ellipse=TRUE, 
            smooth=list(style="lines"))
#With NAs
cor(advertNA, use = "everything",  method = "pearson")

# Only complete observations
# We can get a different type of correlation (e.g., Kendall's tau) by changing the method 
# command:
cor(advertNA, use = "complete.obs",  method = "kendall")


#--------Pearson r----------

# cor(x,y, use = "everything", method = "correlation type")
# Method can be either "pearson", "kendall" or "spearman"

# Correlation test for significance 
#cor.test(x,y, alternative = "string", method = "correlation type", conf.level = 0.95)
getwd()
setwd("/home/erickoukos/Downloads/Project R/Essential Level/Session Six/Practicals")
examData = read.csv("exam_anxiety.csv")
# The first issue we have is that some of the variables are not numeric (Gender) and others 
# are not meaningful numerically 

examData2 <- examData[, c("exam_grade", "anxiety", "revise")]
scatterplot(exam_grade ~ anxiety, data=examData2, ellipse=TRUE, 
            smooth=list(style="lines"))

# Pearson correlations between all continuous variables
#  by specifying the dataframe (examData2):

cor(examData2, use = "complete.obs", method = "pearson")

#If we want a single correlation between a pair of variables (e.g., Exam and Anxiety) then 
#we'd specify both variables instead of the dataframe:

cor(examData2$exam_grade, examData2$anxiety, use = "complete.obs", method = 'pearson')


# Correlation coefficients are effect sizes, so we can interpret these values without really 
# needing to worry about p-values. However fir p-values, then you can use the rcorr() 
#We need to convert our dataframe into a matrix using the as.matrix() command. 
# We can include only numeric variables so, just as we did above, we need to select only the 
# numeric variables within the examData dataframe. 
library(Hmisc)
examMatrix<-as.matrix(examData[, c("exam_grade", "anxiety", "revise")])
head(examMatrix)
Hmisc::rcorr(examMatrix)

#The method above makes it clear what we're doing, but more experienced users 
# could combine the previous two commands into a single one:
Hmisc::rcorr(as.matrix(examData[, c("exam_grade", "anxiety", "revise")]))

#The results shows the same previously seen correlation matrix, except rounded to 2 decimal 
# places. In addition, we are given the sample size on which these correlations are based, and 
# also a matrix of p-values that corresponds to the matrix of correlation coefficients above. 
# Exam performance is negatively related to exam anxiety with a Pearson correlation coefficient 
# of r = ???.44 and the significance value is less than .001 (it is approximately zero). This significance 
#value tells us that the probability of getting a correlation coefficient this big in a sample 
# of 103 people if the null hypothesis were true (there was no relationship between these variables) is 
# very low (close to zero in fact). Hence, we can gain confidence that there is a genuine 
# relationship between exam performance and anxiety. Our criterion for significance is usually 
# .05 so we can say that all of the correlation coefficients are significant.


# It can also be very useful to look at confidence intervals for correlation coefficients. Sadly, 
# we have to do this one at a time (we can't do it for a whole dataframe or matrix). Let's look 
# at the correlation between exam performance (Exam) and exam anxiety (Anxiety). We can 
# compute the confidence interval using cor.test() by executing:

cor.test(examData$anxiety, examData$exam_grade)
cor.test(examData$revise, examData$exam_grade)
cor.test(examData$anxiety, examData$revise)

# Pearson correlation between exam performance and anxiety was -.441, but 
# tells us that this was highly significantly different from zero, t(101) = -4.94, p < .001. 
#Most important, the 95% confidence ranged from -.585 to - .271, which does not cross zero. 

# The correlation coefficient 
# squared (known as the coefficient of determination, R 2 ) is a measure of the amount of 
# variability in one variable that is shared by the other. 

#-------------------------------- The  coefficient of determinatio (R-squared)

#correlation coefficient #squared (known as the coefficient of determination, R2 ) 
# is a measure of the amount of variability in one variable that is shared by the other. 


# Below is a matrix containing r2  instead of r 
cor(examData2)^2

#Note that for exam performance and anxiety the value is 0.194, which is what we calcu-
# lated above. If you want these values expressed as a percentage then simply multiply by 
# 100, so the command would become:

cor(examData2)^2 * 100





#--------Spearman's Rho----------

# Spearman's correlation coefficient (Spearman, 1910), rs, is a non-parametric 
#statistic and so can be used when the data have violated parametric assumptions such 
# as non-normally distributed data. The test is alsoreferred to as Spearman's rho 
# Spearman's test works by first ranking the data  
 
# The example used is from the World's Biggest Liar competition held annually at the 
# Santon Bridge Inn in Wasdale (in the Lake District). 
#  Each year locals are encouraged to attempt to tell the biggest lie in the 
# world    

# Assume we wanted to test a theory that more creative people will be able to create taller 
# tales.  68 past contestants from this competition were asked where they were placed 
# in the competition (first, second, third, etc.) and were also given a creativity 
# questionnaire (maximum score 60). The position in the competition is an ordinal variable 
# because the places are categories but have a meaningful order (first place is better 
# than second place and so on). Therefore, Spearman's correlation coefficient should 
# be used (Pearson's r requires interval or ratio data). 

liarData = as.data.frame(read.csv("biggest_liar.csv"))

head(liarData)

#The procedure for doing a Spearman correlation is the same as for a Pearson correlation 
#except that we need to specify that we want a Spearman correlation instead of Pearson,

#To obtain the correlation coefficient for a pair of variables we can execute:

cor(liarData$position, liarData$creativity, method = "spearman")

# We hypotheeize that more creative people would tell better lies.
# Therefore, we predict that the correlation will be less than zero, and we can 
# reflect this prediction by using alternative = "less" in the command:

cor.test(liarData$position, liarData$creativity, alternative = "less", method = "spearman")

#If we want a significance value for this correlation we could either use simply use cor.test(), 
# which has the advantage that we can set a directional hypothesis. 

cor.test(liarData$position, liarData$creativity, method = "spearman")


#--------Kendall's Tau----------
#Kendall's tau is another non-parametric correlation and it should be used rather than 
#Spearman's coefficient when you have a small data set with a large number of tied ranks. 
# This means that if you rank all of the scores and many scores have the same rank, then 
# Kendall's tau should be used. 


cor(liarData$position, liarData$creativity, method = "kendall")

# We predict that the correlation will be less than zero, and we can 
# reflect this prediction by using alternative = "less" in the command:

cor.test(liarData$position, liarData$creativity, alternative = "less", method = "kendall")

# We can also use alternative = more. Below we compare earlier pearson and kendall coefficient
# with alternative = "more" for the first data on number of adverts watched versus and packets
# of sweets bought

adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15) 
advertData<-data.frame(adverts, packets)

# Two sided test not assuming more adverts lead to more sweets bought
cor(advertData$adverts, advertData$packets, method = "pearson")

# One sided test testing if  more adverts lead to more sweets bought
# Note: alternative = "more"
cor.test(advertData$adverts, advertData$packets, alternative = "greater", method = "pearson")

cor.test(advertData$adverts, advertData$packets, alternative = "greater", method = "kendall")


#-------Point Biserial-----

#The point-biserial correlation coefficient, rpb, quantifies the relationship 
#between a continuous variable and a variable that is a discrete dichotomy 
# (e.g., there is no continuum underlying the two categories, such as dead or alive).

#The biserial correlation coefficient, rb, quantifies the relationship between a 
#continuous variable and a variable that is a continuous dichotomy 
# (e.g., there is a continuum underlying the two categories, such as passing or failing an exam
# where people may just pass while others pass excellently).

# Example data catData studying the relationship between the gender of a cat and how 
# much time it spent away from home. 
# We heard that male cats disappeared for substantial amounts of time on long-distance 
# roams around the neighbourhood whereas female cats tended to be more homebound.
# There are three variables:
  # time, which is the number of hours that the cat spent away from home (in a week).
  # gender, is the gender of the cat, coded as 1 for male and 0 for female.
  # recode, is the gender of the cat but coded the opposite way around (i.e., 0 for male 
  # and 1 for female). 


 # Using the polycor package and using the polyserial() function. 
 # Simply specify the two variables of interest within this function 
 #just as you have been doing for every other correlation 

catData <- read.csv("roaming_cats.csv")

polyserial(catData$time, catData$sex)



#---------------Partial and semi-partial correlation
# A partial correlation quantifies the relationship between two variables while controlling for the effects of a third variable on 
# both variables in the original correlation.
# A semi-partial correlation quantifies the relationship between two variables while controlling for the effects of a third variable 
# on only one of the variables in the original correlation.

#Exam Anxiety Data file: create a dataframe containing only the 
# three variables of interest. We will conduct a partial correlation between exam anxiety and 
# exam performance while 'controlling' for the effect of revision time.

# Use the ggm package
library(ggm)

# The general form of pcor() is:
#pcor(c("var1", "var2", "control1", "control2" etc.), var(dataframe))

# # For the current example, we want the correlation between exam 
# anxiety and exam performance (so we list these variables first) controlling for exam revision 
# (so we list this revision variable afterwards).

pc<-pcor(c("exam_grade", "anxiety", "revise"), var(examData2))

#We can then see the partial correlation and the value of R 2  in the console by executing:
pc
pc^2

#The general form of pcor.test() is:   pcor(pcor object, number of control variables, sample size)
# Basically, you enter an object that you have created with pcor() 
# or you can put the pcor() command directly into the function). 
#We created a partial correlation object called pc, had only one control variable (Revise) 
#and there was a sample size of 103; 
# therefore, to see the significance of the partial correlation, we can execute:
pcor.test(pc, 1, 103)





