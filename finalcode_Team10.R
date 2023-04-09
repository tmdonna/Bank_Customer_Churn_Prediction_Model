#Data Cleaning
library(leaps)
bank <- read.csv('Churn_Modelling.csv')
bank
bank1 <- bank[,4:14]
str(bank1)
head(bank1)

#Data Cleaning
Geography <- bank1$Geography
tempGeo <- data.frame(model.matrix(~Geography-1))
newdat <- cbind(bank1, tempGeo)
str(newdat)
newdat$Gender <- as.factor(newdat$Gender)
newdat$Gender <- as.numeric(newdat$Gender) - 1
newdat$Male <- newdat$Gender
str(newdat)
dat<-newdat[,c(1,4,5,6,7,8,9,10,11,12,13,15)]
str(dat)

#Observing Data
exit.count <- table(dat$Exited)
exit.count
exit.perc <- table(dat$Exited)/nrow(dat)
exit.perc

#Training and Test data
Exit <- subset(dat, dat$Exited == 1)
Stay <- subset(dat, dat$Exited == 0)
set.seed(112233)
train.Exit <- sample(1:nrow(Exit),1018)
train.Stay <- sample(1:nrow(Stay),1018)
#Combining Training Data
dat.train <- rbind(Exit[train.Exit,],Stay[train.Stay,])
table(dat.train$Exited)
#Removing Data and Making Testing Data Set while Keeping Proportion
newStay <- Stay[-train.Stay,]
test.Stay <- newStay[sample(1:nrow(newStay),3983),]
dat.test <- rbind(Exit[-train.Exit,],test.Stay)
table(dat.test$Exited)
exit.perc <- table(dat.test$Exited)/nrow(dat.test)
exit.perc

#Obtaining Best Logistic Regression and Using Step 
reg1 <- glm(formula = Exited ~., data=dat.train, family='binomial')
summary(reg1)
best_model <- step(reg1, direction = "both")
summary(best_model)
print(exp(coef(best_model)))

#Model Performance on Training Data
yhat.train <- predict(best_model, dat.train, 
                      type = "response")
yhat.train.plus.act <- cbind(yhat.train, 
                             dat.train$Exited)

yhat.train.class <- ifelse(yhat.train > 0.5, 1, 0)
yhat.train.class[1:20] 
tab.lr1.train <- table(dat.train$Exited, 
                       yhat.train.class, 
                       dnn = c("Actual","Predicted"))
tab.lr1.train

lr1.train.err <- (tab.lr1.train[1,2] + 
                    tab.lr1.train[2,1])/sum(tab.lr1.train)
lr1.train.err 
#We see almost 30% error in the training data

#Model performance on Test Dataset
yhat.test <- predict(best_model, dat.test, 
                     type = "response") 

yhat.test.class <- ifelse(yhat.test > 0.5, 1, 0)
yhat.test.class[1:20]

tab.lr1.test <- table(dat.test$Exited, 
                      yhat.test.class, 
                      dnn = c("Actual","Predicted"))
tab.lr1.test
logerr1<-mean(yhat.test.class != dat.test$Exited) 
logerr1
#Slightly better in the Testing Dataset, 28.37%

#Finding the Best Cutoff
overall_err <- 1:999
class1_err <- 1:999
class0_err <- 1:999
for (i in 1:999){
  val <- i/1000
  yhat.test.class <- ifelse(yhat.test > val, 1, 0)
  overall_err[i] <- mean(dat.test$Exited 
                         != yhat.test.class)
  class1_err[i] <- mean(dat.test$Exited[1:128] 
                        != yhat.test.class[1:128])
  class0_err[i] <- mean(dat.test$Exited[129:1250] 
                        != yhat.test.class[129:1250])
}
class1_err[1:10]
class0_err[1:10]
xrange <- 1:999/1000
plot(xrange, class0_err, xlab = "Cutoff Value", 
     ylab = "Error Rate", col = "Red", type = "b")
points(xrange, class1_err, xlab = "Cutoff Value", 
       col = "blue")
#We see that graph intersects around at 0.6, so let us try that value

#Model Performance on Training Data
yhat.train <- predict(best_model, dat.train, 
                      type = "response")
yhat.train.plus.act <- cbind(yhat.train, 
                             dat.train$Exited)

yhat.train.class <- ifelse(yhat.train > 0.6, 1, 0)
yhat.train.class[1:20] 
tab.lr1.train <- table(dat.train$Exited, 
                       yhat.train.class, 
                       dnn = c("Actual","Predicted"))
tab.lr1.train

lr1.train.err <- (tab.lr1.train[1,2] + 
                    tab.lr1.train[2,1])/sum(tab.lr1.train)
lr1.train.err 
#Error increased in training data set, 31%

#Model performance on Test Dataset
yhat.test <- predict(best_model, dat.test, 
                     type = "response") 

yhat.test.class <- ifelse(yhat.test > 0.6, 1, 0)
yhat.test.class[1:20]

tab.lr1.test <- table(dat.test$Exited, 
                      yhat.test.class, 
                      dnn = c("Actual","Predicted"))
tab.lr1.test
logerr2<-mean(yhat.test.class != dat.test$Exited) 
logerr2
#Model performs way better at testing dataset. -> 22.67%

#Lets try adding Interaction Terms to the best model
reg2 <- glm(formula = Exited ~ CreditScore + Age + Tenure
            +Balance + IsActiveMember + GeographyGermany + Male
            + Balance * Tenure + Balance * CreditScore 
            + IsActiveMember * Age + CreditScore * Age
            , data=dat.train, family='binomial')
summary(reg2)
#Testing Set Error
yhat.test3 <- predict(reg2, dat.test, 
                     type = "response") 

yhat.test.class3 <- ifelse(yhat.test3 > 0.6, 1, 0)
yhat.test.class3[1:20]

tab.lr1.test3 <- table(dat.test$Exited, 
                      yhat.test.class3, 
                      dnn = c("Actual","Predicted"))
tab.lr1.test3
logerr3<-mean(yhat.test.class3 != dat.test$Exited) 
logerr3 
#21.55% with 0.6 cutoff, 27% with 0.5 cutoff


#kNN
library(class)
dat.train
dat.train.x <- dat.train[,-9]
dat.train.y <- dat.train[,9]
dat.test.x <- dat.test[,-9]
dat.test.y <- dat.test[,9]

#Scaling the Continuous Variable
dat.train.x
dat.train.x1<-dat.train.x[,c(1,2,3,4,5,8)]
dat.train.x1<-scale(dat.train.x1)
dat.train.x1
dat.train.x <-cbind(dat.train.x1,dat.train.x[,-c(1,2,3,4,5,8)])
dat.train.x

dat.test.x1<-dat.test.x[,c(1,2,3,4,5,8)]
dat.test.x1<-scale(dat.test.x1)
dat.test.x1
dat.test.x <-cbind(dat.test.x1,dat.test.x[,-c(1,2,3,4,5,8)])
dat.test.x

#Try k = 1
out1 <- knn(dat.train.x, dat.test.x, dat.train.y, k=1)
tab.knn1 <- table(dat.test.y, out1,
                  dnn = c("Actual", "Predicted"))
tab.knn1
knn1.err <- mean(dat.test.y != out1)
knn1.err

#Try k = 5
out5 <- knn(dat.train.x, dat.test.x, dat.train.y, k=5)
tab.knn5 <- table(dat.test.y, out5,
                  dnn = c("Actual", "Predicted"))
tab.knn5
knn5.err <- mean(dat.test.y != out5)
knn5.err

#Try k = 13
out13 <- knn(dat.train.x, dat.test.x, dat.train.y, k=13)
tab.knn13 <- table(dat.test.y, out13,
                   dnn = c("Actual", "Predicted"))
tab.knn13
knn13.err <- mean(dat.test.y != out13)
knn13.err

#We see that k is higher -> smaller error but still worse than logistic
#regression

#Finding Optimal k
knn.err <- 1:50
xrange <- 1:50
for (j in 1:99) {
  if (j %% 2 != 0) {
    xrange[(j+1)/2] <- j
    out <- knn(dat.train.x, dat.test.x, 
               dat.train.y, j)
    knn.err[(j+1)/2] <- mean(out != dat.test.y)
  }
}
xrange
knn.err
plot(xrange, knn.err, xlab = "Value of K (K odd)",
     ylab = "Error from KNN") 

#From the graph, let us try k=19
out19 <- knn(dat.train.x, dat.test.x, dat.train.y, k=19)
tab.knn19 <- table(dat.test.y, out19,
                   dnn = c("Actual", "Predicted"))
tab.knn19
knn19.err <- mean(dat.test.y != out19)
knn19.err
#Error is now at 26% with kNN = 19 and scaled data!


#Using LDA
library(MASS)
table(dat.train$Exited)
lda.fit <- lda(Exited ~ ., data = dat.train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, dat.test)
lda.pred$posterior
lda.pred$class
lda.test.class <- lda.pred$class
tab.lda <- table(dat.test$Exited, lda.test.class,
                 dnn = c("Actual", "Predicted"))

tab.lda
err.lda <- mean(dat.test$Exited != lda.test.class)
err.lda
#Error is 28.17%

#Using QDA
qda.fit <- qda(Exited ~ ., data = dat.train)
names(qda.fit)
qda.pred <- predict(qda.fit, dat.test)
qda.test.class <- qda.pred$class
tab.qda <- table(dat.test$Exited, qda.test.class,
                 dnn = c("Actual", "Predicted"))
tab.qda
err.qda <- mean(dat.test$Exited != qda.test.class)
err.qda
#Error is 26.1%


#Naive Bayes
library(e1071)
nb.fit <- naiveBayes(Exited ~ ., data = dat.train)
nb.fit
nb.class <- predict(nb.fit, newdata = dat.test)
nb.class
tab.nb <- table(dat.test$Exited, nb.class,
                dnn = c("Actual", "Predicted"))
tab.nb
err.nb <- mean(dat.test$Exited != nb.class)
err.nb
#Error is 26.65%

#Classification Tree
library(tree)
dat.train[,9] <- as.factor(dat.train[,9])
dat.test[,9] <- as.factor(dat.test[,9])
#Building the Model
tree1 <- tree(Exited~., data = dat.train)
summary(tree1)
#Plot
plot(tree1)
text(tree1, pretty = 0)
#Model Performance on Training
tree.pred.tr <- predict(tree1, dat.train, type = "class")
table(dat.train$Exited, tree.pred.tr,
      dnn = c("Actual", "Predicted"))
mean(dat.train$Exited != tree.pred.tr)
#Model Performance on Testing
tree.pred.tst <- predict(tree1, dat.test, type = "class")
table(dat.test$Exited, tree.pred.tst,
      dnn = c("Actual", "Predicted"))
treeerr<-mean(dat.test$Exited != tree.pred.tst)
treeerr

#Random Forest 
#Train a Random Forest Model
library(randomForest)
rf <- randomForest(Exited ~.,dat=dat.train, ntree = 1000, 
                   importance = TRUE)

rf
#Model Performance with Testing data
yhat.rf <- predict(rf, dat.test)
tab.rf <- table(dat.test$Exited, yhat.rf)
tab.rf
err.rf <- mean(dat.test$Exited != yhat.rf)
err.rf

importance(rf)
varImpPlot(rf, main = "Variable Importance Plot")

#SUMMARY
sumstats.tab <- as.data.frame(matrix(nrow = 1, ncol = 10))
sumstats.tab[1,1] <- logerr1
sumstats.tab[1,2] <- logerr2
sumstats.tab[1,3] <- logerr3
sumstats.tab[1,4] <- knn1.err
sumstats.tab[1,5] <- knn5.err
sumstats.tab[1,6] <- knn13.err
sumstats.tab[1,7] <- knn19.err
sumstats.tab[1,8] <- err.lda
sumstats.tab[1,9] <- err.qda
sumstats.tab[1,10] <- err.nb
sumstats.tab[1,11] <- treeerr
sumstats.tab[1,12] <- err.rf
colnames(sumstats.tab) <- c("Logistic Reg 0.5 Cutoff", "Logistic Reg 0.6 Cutoff",
                            'Logistic Reg with Interaction Term 0.6 Cutoff',
                            "kNN - 1 ", "kNN - 5",'kNN - 13','kNN - 19',
                            'LDA','QDA','Naive Bayes','Tree Classification',
                            "Random Forest")
rownames(sumstats.tab) <-  "Error"
sumstats.tab
#Best model is Tree Classification

# Conclusion for Later:
# We see that Logistic Regression is improved by increasing cutoff to 0.6.
# We also added interaction terms and saw that the model improved slightly.
# However, out of all the models we observe, the Tree Classification performed
# the best. 
#   1) We can talk about the variables in the Logistic Regression such as:
#      Germans increases the odds of customers churning, etc.
#   2) We see that in the Classification Tree, it is first split by age which 
#      means that it is the most important variable (google to get more technical 
#      details on what it means to be split first in classification tree)
#
# Also see if Age is significant or not in logistic regression


#Presentation: Logistic Regression - Best Model 0.5 Cutoff -> Best Model 0.6 Cutoff,
# -> Interaction Terms, Classification Tree, (Random Forest)

#Data Visualizations
install.packages("ggplot2")
install.packages("tidyverse")

library(ggplot2)
library(tidyverse)

# Visualization 1
# Create a frequency dataframe of geography column
geo_count <- as.data.frame(table(bank1$Geography))
colnames(geo_count) <- c("region", "frequency")
geo_count

# Load geodata and join geodata with geo_count
mapdata <- map_data("world")
mapdata <- left_join(mapdata, geo_count, by = "region")
# Filter only France, Germany, and Spain
mapdata_filtered <- filter(mapdata, region == "France" | 
                             region == "Germany" | 
                             region == "Spain")

# View the resulting data frame
map1 <- ggplot(mapdata_filtered, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = frequency), color = "gray26") +
  ggtitle("Total Number of Customers") +
  scale_fill_gradient(name = "Total", low = "cyan", high = "deepskyblue") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map1

# Visualization 2
# Get frequency of Stay and Exit for each country
france_filtered <- filter(bank1, Geography == "France")
france_count <- as.data.frame(table(france_filtered$Exited))
france_count
germany_filtered <- filter(bank1, Geography == "Germany")
germany_count <- as.data.frame(table(germany_filtered$Exited))
germany_count
spain_filtered <- filter(bank1, Geography == "Spain")
spain_count <- as.data.frame(table(spain_filtered$Exited))
spain_count

# Combine all Stay frequencies into one table
no_exit_count <- data.frame(region = c("France", "Germany", "Spain"),
                            frequency = c(france_count[1,2], 
                                          germany_count[1,2], 
                                          spain_count[1,2]))
# Combine all Exit frequencies into one table
exit_count <- data.frame(region = c("France", "Germany", "Spain"),
                         frequency = c(france_count[2,2], 
                                       germany_count[2,2], 
                                       spain_count[2,2]))

#------------------------Exit=0 proportion heatmap------------------------
# Load geodata and join geodata with Stay table above
mapdata <- map_data("world")
mapdata <- left_join(mapdata, no_exit_count, by = "region")
# Filter only France, Germany, and Spain
mapdata_no_exit_filtered <- filter(mapdata, region == "France" | 
                                     region == "Germany" | 
                                     region == "Spain")
# Creat map visualization
map2 <- ggplot(mapdata_no_exit_filtered, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = frequency), color = "gray26") +
  ggtitle("Total Customers Staying") +
  scale_fill_gradient(name = "Total", low = "cyan", high = "deepskyblue", limits = c(0, 4500)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

#------------------------Exit=1 proportion heatmap------------------------
# Load geodata and join geodata with Exit table above
mapdata <- map_data("world")
mapdata <- left_join(mapdata, exit_count, by = "region")
# Filter only France, Germany, and Spain
mapdata_exit_filtered <- filter(mapdata, region == "France" | 
                                  region == "Germany" | 
                                  region == "Spain")
# Creat map visualization
map3 <- ggplot(mapdata_exit_filtered, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = frequency), color = "gray26") +
  ggtitle("Total Customers Exiting") +
  scale_fill_gradient(name = "Total", low = "cyan", high = "deepskyblue", limits = c(0, 4500)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())

map3
# Visualization 3
#------------------------Exit=0 proportion bar plot------------------------
stay_barplot <- ggplot(no_exit_count, aes(x = region, y = frequency)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  ylim(0, 4500) +
  labs(title = "Total Customers Staying", x = "Countries", y = "Total")

#------------------------Exit=1 proportion bar plot------------------------
exit_barplot <- ggplot(exit_count, aes(x = region, y = frequency)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  ylim(0, 4500) +
  labs(title = "Total Customers Exiting", x = "Countries", y = "Total")

# Show Visualization 2 and 3 (Heatmap and Bar plot)
library(gridExtra)
grid.arrange(map2, map3, stay_barplot, exit_barplot, ncol = 2, nrow = 2)

# Visualization 4 (Correlation Plot)
library(corrplot)
#visualize correlation matrix
corrplot(cor(dat))

# Visualization 5 (Boxplot)
# Version 1
boxplot(Balance~Exited, data = dat, horizontal = T, 
        xlab = "Credit Card Average", ylab = "Accept Offer")
# Version 2
ggplot(dat, aes(x = Exited, y = Balance, fill = factor(Exited))) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_jitter() +
  labs(x = "Exited", y = "Balance", fill = "Exited") +
  scale_fill_manual(values = c("cyan", "deepskyblue")) +
  theme_classic() +
  coord_flip()
