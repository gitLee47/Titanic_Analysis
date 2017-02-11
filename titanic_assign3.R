install.packages("ggplot2", "reshape2", "plyr", "languageR",
                 "lme4", "psych")

setwd("D:/MS/DA/assign 3")

train <- read.csv("D:/MS/DA/assign 3/train.csv")version

View(train)

str(train)

table(train$Survived)

prop.table(table(train$Survived))

table(train$Sex, train$Survived)

prop.table(table(train$Sex, train$Survived),1)

train$Child <- 0
train$Child[train$Age < 18] <- 1
table(train$Child)

#number of kids and adults based on gender
aggregate(Survived ~ Child + Sex, data=train, FUN=length)

#number of people who survived, since 1 - survived and 0 - died, it will sum and find only survived
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

#proportion of adults and chilren who died based on gender
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x){sum(x)/length(x)})

#seeing what most passengers paid
hist(train$Fare)

#checking relation  based on fares and class
train$FareBins <- '30+'
train$FareBins[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$FareBins[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$FareBins[train$Fare < 10] <- '<10'

#aggregating on class and creating a proportion
aggregate(Survived ~ FareBins+ Pclass + Sex, data=train, FUN=function(x){sum(x)/length(x)})


library(ggplot2)



ggplot(train, aes(Sex, Embarked, color= Survived, size=FareBins)) + geom_point() + facet_wrap(~Pclass)

ggplot(train, aes(Sex, Survived, color = Survived, size=Age)) + geom_point() + facet_wrap(~Pclass)



# females look on average yourger than males
ggplot(train, aes(x = Sex, y = Age)) + stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot()

# 3rd class represents more than half of the total population (55.11%)
ggplot(train, aes(x=Pclass)) + geom_bar()

# interestingly 1st class population is considerably older than the rest. This might be a
# potential confounder!
ggplot(train, aes(x = Pclass, y = Age)) + stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() + facet_grid(~Pclass)



#Survival by Passenger Class and Gender
barchart <- ggplot(train, aes(as.factor(Pclass), fill=as.factor(Survived)))+geom_bar() + facet_grid(~Sex)

barchart+xlab("Passenger Class")+ylab("Number of Passengers")+ggtitle("Survival by Passenger Class and Number")+scale_fill_discrete(name = "", labels = c("Died", "Survived"))



train$Fsize <- train$SibSp + train$Parch + 1

hist(train$Fsize)

histFamily <- ggplot(train, aes(x =Fsize, fill=as.factor(Survived))) + geom_histogram(bins=30)

histFamily+xlab("Family Size")+ylab("Number of Passengers")+ggtitle("Survival rate vs Family Size distribution")+scale_fill_discrete(name = "", labels = c("Died", "Survived"))


#Data Cleaning for Embarked
which(train$Embarked == "")
train_clean = subset(train, PassengerId != 62 & PassengerId != 830)

#Violin and Facet both included
voilinEmb <- ggplot(train_clean, aes(x=Sex, y=Age, fill=as.factor(Survived))) + geom_violin(trim = FALSE) + facet_grid(~Embarked)
voilinEmb+scale_fill_discrete(name = "", labels = c("Died", "Survived"))
