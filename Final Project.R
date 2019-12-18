#Load data
dat <- read.csv('UFCData.csv')
names(dat)


dat$weight_dif <- dat$R


library(MASS)
#install.packages('GGally')
library(GGally)

#install.packages('data.table')

#install.packages('corrplot')
library(corrplot)
## Data Exploration
#In this section, we will mutate and plot the data in order to understand relationships within the data.

head(dat)
nrow(dat)

## Column mutation
#converting Blue and red on winner into  to Win /Loss /Draw /No Contest for each fighter

#convert Blue and red on winner into  to Win /Loss /Draw /No Contest for each fighter
result1<-rep("a", 3592)
result2<-rep("a", 3592)
for (i in c(1:3592)) {
  if (dat$Winner[i]=="Red") {
    result1[i]<-"L"
    result2[i]<-"W"
  } else if (dat$Winner[i]=="Blue") {
    result1[i]<-"W"
    result2[i]<-"L"
  } else if (dat$Winner[i]=="draw") {
    result1[i]<-"D"
    result2[i]<-"D"
  } else {
    result1[i]<-"NC"
    result2[i]<-"NC"
  }
}
#assinging the muatated variables
dat$Blue_result <- result1
dat$Red_result <- result2

#viewing data in a table
table(dat$Winner)
table(dat$no_of_rounds)
table(dat$R_age)
table(dat$B_age)

#First off, let's get a broad look at how several of the columns within this dataset look in relationship to each other.

ggpairs(data=dat, columns=c("R_age", "B_age", "R_Weight_lbs", "B_Weight_lbs", "Winner"))

## Testing for corelation
#To see if some of the variables are correlated to each other we run the corelation plots, which produced the results below:

totalcor<-cor(dat[,c(4:5,7:23,27)])
par(xpd=TRUE)
corrplot(totalcor, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 90, mar = c(1,1,.5,.5))



##Testing whether the variables are normally distributed
#The variables in our data are not normally distributed.

ggplot(data=dat, aes(no_of_rounds)) + geom_bar(aes(fill=Winner))


#Now, in order to get a feel for how the wins are distributed, we can use a facet grid to see wins for each color by the number of rounds each fight took.

ggplot(data=dat, aes(no_of_rounds)) + geom_bar(aes(fill=Blue_result)) + facet_grid(~Winner)

#So this plot shows the number of times blue and red each one, with the number of rounds that fight went on the x-axis. The first graph is the number of times Blue won, with most occurring in the third round, and the second shows the number of times red won, again, with the majority ocurring in the third round.

dat$weight_dif <- dat$R_Weight_lbs - dat$B_Weight_lbs
dat$height_dif <- dat$R_Height_cms - dat$B_Height_cms
qqnorm(dat$R_Weight_lbs)
qqnorm(dat$B_Weight_lbs)

#examining the weights
ggplot(data=dat, aes(B_Weight_lbs)) + geom_histogram(binwidth = 20)
ggplot(data=dat, aes(R_Weight_lbs)) + geom_histogram(binwidth = 20)

#Now, let's see how the weight difference in each fight is distrubuted.

ggplot(data=dat, aes(x=weight_dif)) + geom_density(adjust=4)

#Moving on, lets take a look at age for both Red and Blue fighters

ggplot(data=dat, aes(R_age)) + geom_histogram(binwidth=1)
ggplot(data=dat, aes(B_age)) + geom_histogram(binwidth=1)

## Linear Regression Analyis
#Null Hypothesis: No difference in the number of head strikes attempted by red and blue between winners.

## Building a  Linear Regression Model

fit<-lm(R_avg_HEAD_att ~ B_avg_HEAD_att, data = dat)
summary(fit)

## Plotting the regression

plot(lm(R_avg_HEAD_att ~ B_avg_HEAD_att, data = dat))

## Building a Linear Regression Model
#Null Hypothesis: No difference in the number of head strikes landed by red and blue between winners.

fit<-lm(R_avg_HEAD_landed ~ B_avg_HEAD_landed, data = dat)
summary(fit)

## Plotting the regression model

plot(lm(R_avg_HEAD_landed ~ B_avg_HEAD_landed, data = dat))


## Logistic Regresion
#First we need to create a category for our winner: 1 if red, 0 if blue
dat$numeric_winner <- as.numeric(dat$Winner == "Red")

logit1 <- glm(numeric_winner~R_Height_cms + B_Height_cms + R_age + B_age + R_Weight_lbs + B_Weight_lbs + weight_dif + height_dif + B_avg_BODY_att + R_avg_BODY_att + B_avg_BODY_landed + R_avg_BODY_landed + B_avg_HEAD_att + R_avg_HEAD_att + B_avg_HEAD_landed + R_avg_HEAD_landed,data=dat, family="binomial")
summary(logit1)
#PLOTTING lOGISTIC REGRESSION
ggplot(dat, aes(x=R_age, y=numeric_winner)) + geom_jitter(height=0.1,width=0.5,size = .5) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + xlab("Red Age") + 
  ylab(" ") + theme(axis.text=element_text(size=12),axis.title=element_text(size=16,face="bold"))
#Second plot
ggplot(dat, aes(x=B_age, y=numeric_winner)) + geom_jitter(height=0.1,width=0.5,size = .5) + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + xlab("Blue Age") + 
  ylab(" ") + theme(axis.text=element_text(size=12),axis.title=element_text(size=16,face="bold"))

ggplot(dat,aes(x=R_age,y=R_Height_cms))+ geom_point(aes(color=factor(Red_result)))


#Now perhaps the age of either one is not very interesting to consider, so let's instead take a look at the age difference, specifically the age of red less the age of blue:

dat$age_diff <- dat$R_age-dat$B_age
ggplot(dat, aes(x=age_diff, y=numeric_winner)) + geom_jitter(height=0.1,width=0.5,size = .5) + 
     geom_smooth(method = "glm", method.args = list(family = "binomial")) + xlab("Age Difference") + 
     ylab(" ") + theme(axis.text=element_text(size=12),axis.title=element_text(size=16,face="bold"))

#Let's try to trim our regression a little bit and put our predictive variables in terms of comparative difference:

dat$avg_diff_body_landed <-dat$R_avg_BODY_landed-dat$B_avg_BODY_landed
dat$avg_diff_head_landed <-dat$R_avg_HEAD_landed-dat$B_avg_HEAD_landed
logit2 <- glm(numeric_winner~height_dif+weight_dif+age_diff + avg_diff_body_landed + avg_diff_head_landed,data=dat, family="binomial")
summary(logit2)

#### Predicting Winner Using Decision Tree
#we decided to test our Logistic regression  using a Decision Tree to predict the winner

## Step 1:Split data in train and test data
#We decided to split the data using 0.7 ratio and divide it into two data sets which are training and testing set.

#install.packages("caTools")
library(caTools)
#install.packages("rpart")
library(rpart)
set.seed(2447)
#splitting the data 
split <- sample.split(dat, SplitRatio = 0.7)
split
#dividing the data into trainig and testing subsets
train <- subset(dat, split=="TRUE")
test <- subset(dat, split=="FALSE")
str(train)
str(test)
# Step 2:Train model with logistics regression using glm function
#In this we built our model inform of logistic regression and used the train subset as our data.

dmodel <- rpart(numeric_winner~., data=train, method="class")
dmodel
summary(dmodel)

#To visualize how our model is predicting we decided to plot our model
#From our results we can see the we have split errors hence our decison tree need pruning 
printcp(dmodel)
plotcp(dmodel)

plot(dmodel,uniform=TRUE,branch=0.6,margin=0.1)

# Step 3:Predict test data based on trained model 
#we used our trained model to predict the winner on the test data.

test$numeric_winner_predicted <-predict(dmodel, newdata=test, type="class")
table(test$numeric_winner,test$numeric_winner_predicted)


#install.packages("caret")
library(caret)


#install.packages("e1071")
library(e1071)

# Step 4: Evauate Model Accuracy using Confusion matrix
#we can use confusion matrix to evalute the accuracy of our model
confusionMatrix(table(test$numeric_winner,test$numeric_winner_predicted))


##install.packages('rpart.plot')
library(rpart.plot)

#Prune the tree by setting the CP parameter as =  cpmin

decision_tree_pruned = prune(dmodel, cp = cpmin)
rpart.plot(decision_tree_pruned)
printcp(decision_tree_pruned)
plotcp(decision_tree_pruned)


# Predict test data based on trained model

test$numeric_winner_predicted <-predict(decision_tree_pruned, newdata=test, type="class")
table(test$numeric_winner,test$numeric_winner_predicted)

# Evaluate Model Accuracy using Confusion matrix

confusionMatrix(table(test$numeric_winner,test$numeric_winner_predicted))

