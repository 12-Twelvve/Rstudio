# remove all existing variables in the working environment
rm(list = ls())
# reading data
data<- read.csv('STAT100_A2_data_IshantNeupaneNeupane.csv')
attach(data)
summary(data)
data$Sample.Number <- factor(data$Sample.Number)
Sample.Number
#mean(data$Sample.Number, na.rm=1)
# lapply(data$Sample.Number, mean, na.rm = TRUE)
# sapply(Sample.Number, mean, na.rm = TRUE)
# colMeans(Sample.Number, na.rm = TRUE)
sd(data$FlipperLength)

# -----------------Question 1-----------------
# 1. Is there evidence that one species of penguins is more or less prevalent in the Anvers
# region?
tb<-table(data$Island, data$Species)
tb
t.test(data$Island, data$Species, paired = TRUE)
mosaicplot(tb,main = "Hypertension Mosaic Plot",
           xlab = "Island",
           ylab = "Species",
           las = 1,
           color = "yellow"
)
barplot(tb, xlab = "Island",  legend=TRUE, beside=TRUE, main='Bar chart', col=c('yellow', 'pink','orange'))

chisq.test(x = tb,simulate.p.value = TRUE)
curve(dchisq(x, df = 4), from = 0, to = 15,
      main = 'Chi-Square Distribution (df = 4)',
      ylab = 'Density',
      lwd = 2)
x_vec <- seq(64.691, 15)
p_vec <-dchisq(x_vec, df = 4)
polygon(c(x_vec, rev(x_vec)), 
        c(p_vec, rep(0, length(p_vec))),
        col = adjustcolor('blue', alpha=0.9), 
        border = NA)
pchisq(q=64.691, df = 4, lower.tail = FALSE )
# -----------------Question 2-----------------
#I2. Do males have significantly larger culmen length than females?
#boxplot
boxplot(CulmenLength~Sex, xlab="Sex", ylab="Culmen Length" , main="Boxplot between Culmen length vs Sex", col=c("green", "yellow"))
qqnorm(CulmenLength, col=c("yellow","blue", "red","green","magenta"), pch=19)
aggregate(CulmenLength, list(sex=Sex), sd)
tapply(CulmenLength, Sex, mean)
table(Sex)

Model <- lm(CulmenLength ~ Sex, data = data)
anova(Model)

# -----------------Question 3-----------------
# Is there a significant linear relationship between culmen length and flipper length for
#Chinstrap penguins?

df  <- subset(data, Species == 'Chinstrap')
attach(df)
plot( df$CulmenLength, df$FlipperLength,
      main = "scatterplot of Culmen Length against Flipper Length of Chinstrap penguins", 
      xlab = "Culmen length", 
      ylab = "Flipper lenght", 
      pch=19,
      col="red" )
summary(df)
length(df$Sample.Number)
abline(lm(df$FlipperLength ~ df$CulmenLength , data=df),  col="green")


model <- lm(df$CulmenLength~df$FlipperLength)
summary(model)
confint(model)
plot(model, which=1)
plot(model, which=2, col='red')
# -------------------------------------------
# ● Produce a plot to explore the variables in the question
# ● Check the flow chart for an appropriate statistical test
# ● Write down null and alternative hypotheses
# ● Check assumptions/condition
# ● Run the analysis
# ● Write up the results

# Produce a scatter plot 
plot( data$Age, data$Systolic,
      main = "scatterplot of Systolic Blood pressure against Age", 
      xlab = "Age", 
      ylab = "Systolic", 
      pch=19,
      col="red" )
# outcomes
# There appears a Slightly Positive linear
# relationship between Systolic blood pressure  and Age.
# • When a Person gets older ,
# the level of Systolic blood pressure also slightly increased.


# Add fit lines
abline(lm(data$Systolic~data$Age), col="blue") # regression line (y~x)
# lines(lowess(Age,Systolic), col="blue") # lowess line (x,y)

# 2.Check the flow chart for an appropriate statistical test


# 3. Write down null and alternative hypotheses

# H0: There is no linear relationship between Systolic blood pressure and Age
# H0: β1 = 0
# 
# Ha: There is a linear relationship between Systolic blood pressure and Age
# Ha: β1 ≠ 0

# simple linear Regression
model <- lm(Systolic~Age)
summary(model)
# from the observation
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 117.6597     6.3851  18.427   <2e-16 ***
#   df$Age        0.2410     0.1344   1.793   0.0782 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.88 on 58 degrees of freedom
# Multiple R-squared:  0.0525,	Adjusted R-squared:  0.03616 
# F-statistic: 3.213 on 1 and 58 DF,  p-value: 0.07825

# the fitted equation is:
systolic = 117.6597 + 0.2410 * age
# tHE P-VALue for the slope is 0.0782.
# concluded that tThere is a linear relationship between Systolic blood pressure and Age
# When the age increases by 1 year,
# we’d expect the mean number of increases by 0.241
# The strength of the relationship between these variables is weak, R2 = 0.036.
confint(model)
# The 97.5% CI for the slope of the regression line is (-0.0281, -0.510).
# when a person age increases by 1 year we'd expect the Systolic blood pressure would
# increases and decreases between [0.51, -0.0281]

# 4. Check assumptions/condition

plot(data$Systolic ~data$Age, pch=19, col="blue")
# 4.1 Independent observations
# From the study description, the observations are independent between sites.
# (i.e. data observed in one site does not affect the data from the other sites).
# 4.2 Linearity
abline(model, size=4, col="red") # check linearity
# 2. Linearity
# There is no obvious curvature in the
# scatterplot, so the assumption of linearity
# is valid.
plot(model, which=1) # residuals vs fitted value
# 4.3 Residuals have a constant variance
# Residuals appear to randomly scatter
# below and above the horizontal line 0
# (the red line). However, the residuals
# seem to spread more on the middle than on the left and  right.
# This suggests that the assumption of
# constant variance is not valid.
# The observations 58, 59 and 28 could be outliers.
plot(model, which=2) # QQ plot
# 4.4. Residuals follow a normal distribution, N(0,1)
# From the Q-Q plot, the residuals appear to form
# a straight line, despite some deviations at the tails and head.
# This suggests that the normality assumption of the
# residuals is met.
# The observation numbers 58 has a standardised
# residual of -1.5 and number 59 has a standardised residual of 4 which are an obvious outlier.
# The observation numbers 28 is also
# potential outliers.

# 5. Conclusion
# Simple linear regression was performed to investigate the relationship between the Systolic BLood pressure
#  and a person Age . There was a significant weak linear relationship between Age
# and Systolic (R2 = 0.0525). The fitted equation is: systolic = 117.6597 + 0.2410 * age
# 
# When a person age is increased by 1 year, we’d expect the mean
# Systolic blood pressure  would
# increases and decreases between [0.51, -0.0281]

# --------------------------------------------
