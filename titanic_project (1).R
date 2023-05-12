library(data.table) # the library that had fread function
library(dplyr)
library(statsr)
library(ggplot2)

## Details of our data: ##
# "Survived" = survived or not, "Pclass" = passenger's class
# "SibSp" = no. of siblings/spouses, "Parch" = no. of parents/children
# "Fare" = value of ticket
# "Embarked" = denotes the embarkation spot, C = Cherbourg, Q = Queenstown, S = Southampton
titanic <- fread("C:\\Users\\Loreen\\Desktop\\train.csv")

# Question 1:
summary(titanic)
# the summary function enables u to summarize all columns of data set, 
# while summarize() you have to specify which data u want to work on


#Question 2

# Qauntitative: It is the method used to generate numerical data by using 
# a lot of techniques such as logical,statistical and mathematical
# techniques and It is expressed using graphs and numbers
# Qalitativw: A method for understanding human behaviour and personalities better
# It is generally expressed using words

# ----------------------------------------
# variabel   | Qualitative| Quantitative|
# ----------------------------------------
# PassengerId                   1      
# Survived                      1
# Pclass                        1
# Name              1
# Sex               1
# Age                           1
# SibSp                         1
# Parch                         1
# Ticket            1
# Fare                          1
# Cabin             1
# Embarked          1


# Question 3:
# if we wanted to count NA values in multiple columns we use colSums, 
# but since we want for only one col we use sum()
NA_count <- sum(is.na(titanic$Age))
NA_count
# ii) it is normal to have few missing data entries with a great population, also most data sets in the real world contain missing data
# iv) it is needed to make transformations and engineering to the data with missing fields, for more accurate/correct analysis
# so yes the missing values would affect our analysis so that we won't be able to perform our calculations.


# Question 4:
titanic_df <- data.frame(na.omit(titanic))


# Question 5, 6:
# age follows normal distribution
ggplot(data = titanic_df, aes(x=Age)) + geom_histogram(binwidth = 5, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Age histogram") 

# Fare follows chi squared distribution, as it is not symmetric 
ggplot(data = titanic_df, aes(x=Fare)) +geom_histogram(binwidth = 15, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
   labs(title = "Fare histogram")


# Question 7:
age_mean <- mean(titanic_df$Age)
age_sd <- sd(titanic_df$Age)
age_mean
age_sd



# Question 8:
sample_8 <- sample_n(titanic_df, size = 50) 
sample_8 %>% summarise(x_bar8= mean(Age), sd_8 = sd(Age))


# Question 9:
sample_means50 <- titanic_df %>%
  rep_sample_n(size = 50, reps = 50, replace = TRUE) %>%
  summarise(x_bar50 = mean(Age))

ggplot(data = sample_means50, aes(x=x_bar50)) +  geom_histogram(binwidth = 1, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
   labs(title = "Sample of size=50 and n=50")  
# by taking mean of all the 50 x_bars of sample
# we can estimate that the pop_Age mean is around 30


# Question 10:
sample_means100 <- titanic_df %>%
  rep_sample_n(size = 50, reps = 100, replace = TRUE) %>%
  summarise(x_bar100 = mean(Age))

ggplot(data = sample_means100, aes(x=x_bar100)) +geom_histogram(binwidth = 5, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
   labs(title = "Sample of size=50 and n=100")
# by taking mean of all the 100 x_bars of sample
# we can estimate that the pop_Age mean is around 30


# Question 11:
sample_means1000 <- titanic_df %>%
  rep_sample_n(size = 50, reps = 1000, replace = TRUE) %>%
  summarise(x_bar1000 = mean(Age))

ggplot(data = sample_means1000, aes(x=x_bar1000)) + geom_histogram(binwidth = 1, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Sample of size=50 and n=1000")


# Question 12:
# we can notice that as the number of samples increase, the sample mean get closer to the population parameter-mew


# Question 13:
sample_means_s20 <- titanic_df %>%
  rep_sample_n(size = 20, reps = 1500, replace = TRUE) %>%
  summarise(x_bar_s20 = mean(Age))

ggplot(data = sample_means_s20, aes(x=x_bar_s20)) + geom_histogram(binwidth = 1, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Sample of size=20 and n=1500")


# Question 14:
sample_means_s100 <- titanic_df %>%
  rep_sample_n(size = 100, reps = 1500, replace = TRUE) %>%
  summarise(x_bar_s100 = mean(Age))

ggplot(data = sample_means_s100, aes(x=x_bar_s100)) + geom_histogram(binwidth = 1, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Sample of size=100 and n=1500")


# Question 15:
sample_means_s200 <- titanic_df %>%
  rep_sample_n(size = 200, reps = 1500, replace = TRUE) %>%
  summarise(x_bar_s200 = mean(Age))

ggplot(data = sample_means_s200, aes(x=x_bar_s200)) + geom_histogram(binwidth = 1, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Sample of size=200 and n=1500")


# Question 16:
# as the number of samples increase, the samples' sd will decrease
# as from the formula of sample sd, it is inversely proportional to sqrt(n)


# Question 17:
sample_U1500 <- titanic_df %>%
  rep_sample_n(size = 2, reps = 1500, replace = TRUE) %>%
  summarise(s_2 = var(Age))

ggplot(data = sample_U1500, aes(x=s_2)) + geom_histogram(binwidth = 50, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "variance Sample of size=2 and n=1500")
# variance doesn't follow normal distribution.either it is chi or F


# Question 18:
sample_U1500_2 <- titanic_df %>%
  rep_sample_n(size = 50, reps = 1500, replace = TRUE) %>%
  summarise(s_50 = var(Age))

ggplot(data = sample_U1500_2, aes(x=s_50)) + + geom_histogram(binwidth = 10, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Sample of size= 50 and n=1500")

# 18.i) normal distribution
# 18.ii) this sampling distribution could be used as estimate for the population, since it's a good estimate with good sample size and number of samples


#Question 19
n_19 = 50
df = rnorm(n_19 , mean=mean(titanic_df$Age) , sd=sd(titanic_df$Age))

#MME
mean_est_19  = sum(df)/n_19
var_est_19 = ((sum(df^2)/n_19) - ((mean_est_19)^2))

mean_est_19
var_est_19
sqrt(var_est_19)

bias_MME_19 = mean(titanic_df$Age) - mean_est_19
bias_MME_19

#MLE
NLL_19 = function(pars,data){
  mu=pars[1]
  sigma=pars[2]
  NLL_19=-sum(dnorm(x=data , mean=mu , sd=sigma ,log=TRUE))
}

mle_19 = optim(par=c(mu=0.2,sigm=1.5),fn=NLL_19 , data=df ,
               control=list(parscale=c(mu=0.2,sigm=1.5)))

mle_19$par

bias_MLE_19=mean(titanic_df$Age)- mle_19$par[1]
bias_MLE_19

#Question 20
n20=200
df = rnorm(n20 , mean=mean(titanic_df$Age) , sd=sd(titanic_df$Age))

#MME
mean_est_20=sum(df)/n20
var_est_20=(sum(df^2)/n20 - (mean_est_20)^2)

mean_est_20
var_est_20
sqrt(var_est_20)

bias_MME_20=mean(titanic_df$Age)-mean_est_20
bias_MME_20

#MLE
NLL_20=function(pars,data){
  mu=pars[1]
  sigma=pars[2]
  NLL_20=-sum(dnorm(x=data , mean=mu , sd=sigma ,log=TRUE))
}

mle_20=optim(par=c(mu=0.1,sigm=0.5), fn=NLL_20 , data=df ,
             control=list(parscale=c(mu=0.1,sigm=0.5)))

mle_20$par

bias_MLE_20=mean(titanic_df$Age)- mle_20$par[1]
bias_MLE_20


# Question 21:
age_separate <- titanic_df %>% summarise(sex=titanic_df$Sex, age=titanic_df$Age)
age_male <- age_separate$age[age_separate$sex == "male"]
age_female <- age_separate$age[age_separate$sex == "female"]

# here we had to convert our data to data frame to be able to take samples and plot it
age_male_df <- data.frame(age_male)
sample_male <- age_male_df %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  summarise(males = mean(age_male))

ggplot(data = sample_male, aes(x=males)) + +geom_histogram(binwidth = 1, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Representation for Males ages")


age_female_df <- data.frame(age_female)
sample_female <- age_female_df %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  summarise(females = mean(age_female))

ggplot(data = sample_female, aes(x=females)) + + geom_histogram(binwidth = 1, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Representation of Females ages")


samplediff_means15000 <- data.frame(sample_male - sample_female)
colnames(samplediff_means15000) <- c('replicate','gender_diffrence')

ggplot(data = samplediff_means15000, aes(x=gender_diffrence)) + geom_histogram(binwidth = 1, aes(y= ..density..))+ 
  geom_density(fill = "#77bd89", color = "000000", alpha= 0.6) +
  labs(title = "Difference between Male and Female ages")


# Question 22:
survive_separate <- titanic_df %>% summarise(sex=titanic_df$Sex, survived=titanic_df$Survived)
Survived_male <- survive_separate$survived[survive_separate$sex == "male"]
Survived_female <- survive_separate$survived[survive_separate$sex == "female"]

Survived_male_df <- data.frame(Survived_male)
sample_male_survived <- Survived_male_df %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  summarise(survived_males = mean(Survived_male))

mew1 <- mean(sample_male_survived$survived_males)
mew1

Survived_female_df <- data.frame(Survived_female)
sample_female_survived <- Survived_female_df %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  summarise(survived_females = mean(Survived_female))

mew2 <- mean(sample_female_survived$survived_females)
mew2

difference_between_samples <- mew1 - mew2
differ
#ggplot(data = sample_male_survived, aes(x=Survived_male, fill=Survived_male)) + geom_histogram(binwidth = 1)


# Question 23:
sample <- sample_n(titanic_df, size=10)
#step 1 calculate sample mean 
sample_mean <- mean(sample$Age)
sample_mean
#step 2 Calculate the standard error of the mean
standard_error <- sd(sample$Age)/sqrt(length(sample))
standard_error

#Step 3: Find the score that corresponds to the confidence level
confidence_level <- 0.95 #95%
alpha <- 1 - confidence_level 

DoF <- length(sample) - 1
t_score <- qt(p=alpha/2, df=DoF,lower.tail=F)
t_score
#step 4. Calculate the margin of error and construct the confidence interval
margin_errort <- t_score * standard_error
L <- sample_mean - margin_errort
U <- sample_mean + margin_errort
print(c(L,U))


# Question 24:
sample2 <- sample_n(titanic_df, size=50)
#step 1 calculate sample mean 
sample2_mean <- mean(sample2$Age)
sample2_mean
#step 2 Calculate the standard error of the mean
standard_error2 <- sd(sample2$Age)/sqrt(length(sample2))
standard_error2

confidence_level <- 0.95 #95%
alpha <- 1 - confidence_level 
Z <- 1-(alpha/2)
Z
z_score <- qnorm(Z)
z_score

margin_errorz = z_score * standard_error
L = sample_mean - margin_errorz
U = sample_mean + margin_errorz
print(c(L,U))


# Question 25:
sample_25 <- sample_n(titanic_df, size = 200)
mean_age_25 <- mean(sample_25$Age)
var_age_25 <- var(sample_25$Age)
mean_age_25
var_age_25

newAge_df_25 <- data.frame(newAge= sample_25$Age*5)
mean_newAge_25 <- mean(newAge_df_25$newAge)
var_newAge_25 <- var(newAge_df_25$newAge)
mean_newAge_25
var_newAge_25
# mean became *5 while the variance is now *(5^2)

# Question 26:
sample_26 <- sample_n(titanic_df, size = 200)
mean_age_26 <- mean(sample_26$Age)
var_age_26 <- var(sample_26$Age)
mean_age_26
var_age_26

newAge_df_26 <- data.frame(newAge= sample_26$Age+5)
mean_newAge_26 <- mean(newAge_df_26$newAge)
var_newAge_26 <- var(newAge_df_26$newAge)
mean_newAge_26
var_newAge_26
# mean became +5 while the variance didn't change


