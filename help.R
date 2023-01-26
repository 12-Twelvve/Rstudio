# remove all existing variables in the working environment
rm(list = ls())
# reading data
#1
data <- read.csv('test.csv')
attach(data)
summary(data)
sd(Methane)

table(Method)
tapply(Methane, Method, mean)

table(Sex)
tapply(Methane, Sex, mean)
#2
boxplot(Methane ~ Method,
        col = c('green','yellow'),
        xlab = "Estimation method",
        ylab = "Estimated daily methane production")

#3
#4
#method
stripchart(Methane ~ Method, col=c('purple','orange'),
           xlab = "Estimation method",
           ylab = "Estimated daily methane production",
           pch=19, vertical=TRUE,
           ylim=c(50,100),
           method='jitter')
#sex
stripchart(Methane ~ Sex, col=c('green','red'),
           xlab = "Sex of the Cattle",
           ylab = "Estimated daily methane production",
           pch=19, vertical=TRUE,
           ylim=c(50,100),
           method='jitter')
#5
#6 
t.test(Methane ~ Method)


# -----------------Question 1-----------------
#Is there a significant difference in mean BMI between participants who stayed in Australia
#for less than 12 months, 1-5 years, 6-10 years, 11-20 years and > 20 years?
# -------------------------------------------

table(data$Time)
#1. Produce a plot to explore the variables
boxplot(BMI~Time,main="Boxplot between BI vs Time", col=c("red", "green", "blue", "yellow", "magenta"))

# 2.Check the flow chart for an appropriate statistical test
# • The median of BMI for  6-10 years of Time Frame to be the lowest.
# • The Time frame of  1-5 year , 6 -10 and less than 12 months have a similar median of BMI.
# • The Time frame of 11-20 years appears to be the maximum median of BMI.


# 3. Write down null and alternative hypotheses
# H0: The average BMI is same for all the 5 Time-Frame Residence i.e.
# H0: μ(less then 12 months) = μ(1-5 years) = μ(6-10 years) = μ(11-20 years) =μ(> 20 years)

# Ha: The average BMI is different for at least 2 Time-Frame Residence

# 4. Check assumptions/condition
# 4.1. Independence
# 4.2. Normality of the response
# variable for each group
# 4.3. All groups have the same
# standard deviation.
qqnorm(BMI, col=c("yellow","blue", "red","green","magenta"), pch=19)

aggregate(BMI, list(Time_frame=Time), sd) # obtain sd by Time-frame
tapply(BMI, Time, mean) # mean by Time-Frame
table(Time)
6.73829/3.537642
8.3/4.5
#  Observations are independent within each field as well as between all fields
#  All groups are not very skewed (side-by-side box plot). There is no outlier. The
# assumption of normal distribution is valid.
# # 
# 1.9047<2
# All populations are assumed to have
# the same standard deviation.

f-value
196.701/42.018

0.351/0.057
class(data)
data %>%  filter(!row_number() %in% c(1, 2,3,4,5))
d1 <- data[1:51,]
# 5. Run the analysis
Model <- lm(BMI ~ Time, data = d1)


d <-1.465 -1.056 *6
d



17/51

51*42/127

anova(Model)
# As p-value = 0.017 < 0.05, H0 is rejected
# There is a significant difference in mean
# BMI between participants who stayed in Australia
# for less than 12 months, 1-5 years, 6-10 years, 11-20 years and > 20 years.
# --------------------------------------------
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
influenza <- read.csv('OK.csv')
attach(influenza)

ages <- summary(age)
titre <- summary(fold_change_titre)
s <- rbind(ages,titre)
rownames(s) = c("ages", "Titre")
#print(s)

o <- table(sex, vaccination_status )

rownames(o) = c("female", "male")
colnames(o) =c("not-vaccinated", "vacinated")
write.table(s, file="sec2a_1.csv", sep=',',row.names = TRUE,)
write.table(o, file="sec2a_2.csv", sep=',', )

sec2a_2 <- read.csv("sec2a_2.csv")
View(sec2a_2)
sec2a_1 <- read.csv("sec2a_1.csv")
View(sec2a_1)

#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>

# remove all existing variables in the working environment
rm(list = ls())
# reading data
df <- read.csv('test.csv')
df$Time <- factor(df$Time)
attach(df)
summary(df)
# -----------------Question 1-----------------
#Is there a significant difference in mean BMI between participants who stayed in Australia
#for less than 12 months, 1-5 years, 6-10 years, 11-20 years and > 20 years?
# -------------------------------------------

table(df$Time)
#1. Produce a plot to explore the variables
boxplot(BMI~Time,main="boxplot", col=c("red", "green", "blue", "yellow", "magenta"))

# 2.Check the flow chart for an appropriate statistical test
# • The median of BMI for  6-10 years of Time Frame to be the lowest.
# • The Time frame of  1-5 year , 6 -10 and less than 12 months have a similar median of BMI.
# • The Time frame of 11-20 years appears to be the maximum median of BMI.


# 3. Write down null and alternative hypotheses
# H0: The average BMI is same for all the 5 Time-Frame Residence i.e.
# H0: μ(less then 12 months) = μ(1-5 years) = μ(6-10 years) = μ(11-20 years) =μ(> 20 years)

# Ha: The average BMI is different for at least 2 Time-Frame Residence

# 4. Check assumptions/condition
# 4.1. Independence
# 4.2. Normality of the response
# variable for each group
# 4.3. All groups have the same
# standard deviation.
qqnorm(BMI, col=c("yellow","blue", "red","green","magenta"), pch=19)

aggregate(BMI, list(Time_frame=Time), sd) # obtain sd by Time-frame
tapply(BMI, Time, mean) # mean by Time-Frame

6.73829/3.537642

#  Observations are independent within each field as well as between all fields
#  All groups are not very skewed (side-by-side box plot). There is no outlier. The
# assumption of normal distribution is valid.
# # 
# 1.9047<2
# All populations are assumed to have
# the same standard deviation.


# 5. Run the analysis
Model <- lm(BMI ~ Time, data = df)

anova(Model)
# As p-value = 0.017 < 0.05, H0 is rejected
# There is a significant difference in mean
# BMI between participants who stayed in Australia
# for less than 12 months, 1-5 years, 6-10 years, 11-20 years and > 20 years.
# --------------------------------------------
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>
#===============================================================================================================><><><><>

# remove all existing variables in the working environment
rm(list = ls())
# reading data
df <- read.csv('test.csv')
# df$Time <- factor(df$Time)
attach(df)
summary(df)
# -----------------Question 3-----------------
# Do the data provide evidence to suggest that length of time in Australia (Time) correlated
# with whether participants having hypertension?
# -------------------------------------------

# ● Produce a plot to explore the variables in the question
# ● Check the flow chart for an appropriate statistical test
# ● Write down null and alternative hypotheses
# ● Check assumptions/condition
# ● Run the analysis
# ● Write up the results

# contingency table
count <-table(df$Time, df$Hyp)
count
mosaicplot(count,main = "Hypertension Mosaic Plot",
           xlab = "Time",
           ylab = "Hypertension",
           las = 1,
           color = "skyblue",
           border = "chocolate",
           )

barplot(count, xlab = "Time",ylab = "values",  legend=TRUE, beside=TRUE, main='Bar chart', col=rainbow(20),density=c(30,90))
barplot(count,xlab = "Time",ylab = "values",  legend=TRUE, beside=TRUE, main='Bar chart', col="yellow",density=c(30,90))
# library(ggplot2)
# # Stacked + percent
# ggplot(df, aes(fill=Hyp, y=value, x=specie)) + 
#   geom_bar(position="fill", stat="identity")
# 3. Write down null and alternative hypotheses

# H0: length of time in Australia (Time) and participants having hypertension are independent.HyperTension doesnot vary by 
# length of time in Australia (Time)
# H0: β1 = 0
# 
# Ha: length of time in Australia (Time) and participants having hypertension are dependent.HyperTension  vary by 
# length of time in Australia (Time)
# Ha: β1 ≠ 0


# The test statistic is calculated as
#***formula** 
# where k is the number of cells, R is the number of rows, and C is the
# number of columns.
# The p-value is the area under the χ2df 
# distribution curve to the RIGHT of the calculated test statistic.

count
yg20<- c(5, 5)
y1_5 <- c(10, 2)
y11_20<- c(10,2)
y6_10<- c(11, 4)
lt12m<- c(10,1)


# ..........
tab <- table(df$Hyp ,df$Time )
chisq.test(x = tab)

fisher.test(count)
# expected count = (row total)*(column total)/table total
# X**2
# df = (R-1)*(C-1)= 1*4 = 4
pchisq(q=4.68, df = 4, lower.tail = FALSE )

#create density curve
curve(dchisq(x, df = 4), from = 0, to = 15,
      main = 'Chi-Square Distribution (df = 4)',
      ylab = 'Density',
      lwd = 2)
x_vector <- seq(4.68, 15)
p_vector <-dchisq(x_vector, df = 4)
polygon(c(x_vector, rev(x_vector)), 
        c(p_vector, rep(0, length(p_vector))),
        col = adjustcolor('red', alpha=0.3), 
        border = NA)


# p(X**2df=4 > 1.3121) = 86% or any thing 
p-value>0.05  we dont reject H0

#  Conclusion
# Simple linear regression was performed to investigate the relationship between the Systolic BLood pressure
#  and a person Age . There was a significant weak linear relationship between Age
# and Systolic (R2 = 0.0525). The fitted equation is: systolic = 117.6597 + 0.2410 * age
# 
# When a person age is increased by 1 year, we’d expect the mean
# Systolic blood pressure  would
# increases and decreases between [0.51, -0.0281]

# --------------------------------------------


