####################
#####Data Type#####
##################

library(datasets)

# Quantitaive Data
# Check whether it is discrete or continuous
AirPassengers
BJsales
co2
LakeHuron
attitude['rating']

#Displaying Quantitative Data
stripchart(AirPassengers, xlab = 'airpassanger')
hist(AirPassengers)
aplpack::stem.leaf(AirPassengers,depth=F)

#Displaying Qualitative Data
table(state.division)
barplot(table(state.division),cex.names = 0.8)
dotchart(as.vector(table(state.region)),labels = names(table(state.region)))

state.region

############################
#####Prob. Distribution#####
###########################

# Binomial Distribution
# simulate 10 coin flips, each with a 50% chance of coming up 1 ("heads").
rbinom(10,1,0.5)

# Use the rbinom() function to simulate 100 separate occurrences of 
# flipping 10 coins, where each coin has a 50% chance of coming up heads.
rbinom(100,10,0.5)

# If you flip 10 coins each with a 50% probability of coming up heads, 
# what is the probability exactly 5 of them are heads?
dbinom(5,10,0.5)

# Confirm your answer using the rbinom() function 
# by creating a simulation of 10,000 trials. 
# Put this all on one line by wrapping the mean() function around the rbinom() function.
mean(rbinom(10000,10,0.5)==5)

# Calculate the probability that at least five coins are heads
1-pbinom(4,10,0.5)

# Confirm your answer with a simulation of 10,000 trials
mean(rbinom(10000,10,0.5)>=5)

# Try now with 100, 1000, 10,000, and 100,000 trials
mean(rbinom(100,10,0.5)>=5)
mean(rbinom(1000,10,0.5)>=5)
mean(rbinom(10000,10,0.5)>=5)
mean(rbinom(100000,10,0.5)>=5)

# Simulate calculation of expected value
mean(rbinom(10000,10,0.4))
mean(rbinom(10000,10,0.3))
mean(rbinom(10000,10,0.2))

# Simulate calculation of variance
var(rbinom(10000,10,0.4))
var(rbinom(10000,10,0.3))
var(rbinom(10000,10,0.2))

# Normal Distribution
# Simulation of normal approximation 
hist(rbinom(10000,10,0.5),col='grey')
hist(rbinom(10000,10000,0.5), col='grey')
hist(rnorm(10000,mean(rbinom(10000,10000,0.5)),var(rbinom(10000,10000,0.5))),
     main="Normal Approximation of Binomial Dist")

# Z-Score
# Find P(85 <= X <= 115)
miu = 100
var = 15
X1 = 85
X2 = 115
Z1 = (X1 - miu)/var
Z2 = (X2 - miu)/var

pnorm(Z2,0,1) - pnorm(Z1,0,1)
pnorm(Z2) - pnorm(Z1)
pnorm(X2,miu,var) - pnorm(X1,miu,var)

# What is P(X <= 115)
pnorm(115,miu,var)

# What is P(X >= 120)
1-pnorm(120,miu,var)

# What is X value where the probability is 99% Hints: use qnorm()
qnorm(0.99,miu,var)

# What is X value where the probability is 84%?
qnorm(0.84,miu,var)  

# Women dataset
head(women)
mean = mean(women$height)
vari = var(women$height)

# What is P(X <=  70)
pnorm(70,mean,vari)

# What is P(X >=  85)
1- pnorm(85,mean,vari)

# What is X value where the probability is 90% Hints: use qnorm()
qnorm(0.90,mean,vari)

# What is X value where the probability is 75%
qnorm(0.75,mean,vari)

############################
#####Hypothesis Testing#####
###########################

# One-Sample t-test
# We claim that the average height for American women aged 30-39 is greater than 60. 
# Assume the data is normally distributed.
# H0: the average height for American women aged 30-39 is less than 60
# H1: the average height for American women aged 30-39 is greater than 60

test <-  t.test(women$height, alternative = 'greater', mu=60, conf.level = 0.95)
test$p.value

# Two-Sample t-test
# The Effect of Vitamin C on Tooth Growth in Guinea Pigs. 
# Each animal received one of three dose levels of vitamin C, orange juice (OJ) or ascorbic (VC). 
# We claim there is difference in average tooth length between Guinea Pigs taking OJ and Guinea Pigs taking VC. 
# H0: there is no difference in average tooth length between Guinea Pigs taking OJ and Guinea Pigs taking VC
# H1: there is difference in average tooth length between Guinea Pigs taking OJ and Guinea Pigs taking VC

OJ = ToothGrowth$len[ToothGrowth$supp == 'OJ']
VC = ToothGrowth$len[ToothGrowth$supp == 'VC']

# test the mean difference between two samples
test_2 = t.test(OJ,VC)
test_2$p.value

# Paired_Sample t-test
# Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

# test the mean difference between two paired samples
test_3 = t.test(before,after,paired=T)
test_3$p.value
