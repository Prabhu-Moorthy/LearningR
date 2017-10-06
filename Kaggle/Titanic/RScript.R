training_set = read.csv("train.csv")
test_set = read.csv("test.csv")

test_set$Survived = 0

submit = data.frame(PassengerId = test_set$PassengerId, Survived = test_set$Survived)

write.csv(submit, "allaredead.csv",row.names = FALSE)

summary(training_set$Sex)

prop.table(table(training_set$Sex,training_set$Survived),1)

table(training_set$Sex,training_set$Survived)

test_set$Survived = 0

test_set$Survived[test_set$Sex == "female"] = 1

table(test_set$Sex,test_set$Survived)

summary(training_set$Age)

training_set$Age = ifelse(is.na(training_set$Age), mean(training_set$Age,na.rm = TRUE),training_set$Age)

training_set$Child = 0
training_set$Child[training_set$Age < 18] = 1

aggregate(Survived ~ Child + Sex,training_set, sum)

aggregate(Survived ~ Child + Sex, training_set, length)

aggregate(Survived ~ Child + Sex, training_set, function(x){sum(x)/length(x)})

str(training_set$Fare)

training_set$Fare2 = "30+"
training_set$Fare2[training_set$Fare < 30 & training_set$Fare >=20] = "20-30"
training_set$Fare2[training_set$Fare < 20 & training_set$Fare >=10] = "10-20"
training_set$Fare2[training_set$Fare < 10] = "<10"

str(training_set$Fare2)

table(training_set$Fare2)

aggregate(Survived ~ Fare2 + Pclass + Sex, training_set, function(x){sum(x)/length(x)})

library(rpart)

classifier = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, training_set, method = "class")

plot(classifier)
text(classifier)

y_pred = predict(classifier,training_set,type = "class")

classifier = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                   training_set,
                   control = rpart.control(minsplit = 1,cp = 0),
                   method = "class")

plot(classifier)
text(classifier)

y_pred = predict(classifier,test_set,type = "class")

cm = table(training_set$Survived,y_pred)
cm

submit = data.frame(PassengerId = test_set$PassengerId,Survived = y_pred)
write.csv(submit,"decision_tree.csv",row.names = FALSE)

training_set$Name[1]
test_set$Survived <- NA
combi <- rbind(training_set,test_set)

combi$Name = as.character(combi$Name)

combi$Name[1]
class(combi$Name[1])

strsplit(combi$Name[1],"[.,]")[[1]][2]

combi$Title = sapply(combi$Name,function(x){strsplit(x[1],"[.,]")[[1]][2]})
str(combi$Title)

combi$Title = sub(" ","",combi$Title)

table(combi$Title)

combi$Title[combi$Title %in% c("Mme","Mlle")] = "Mlle"
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title = factor(combi$Title)

combi$FamilySize = combi$SibSp + combi$Parch + 1

summary(training_set)

age_predict = rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                    training_set[!is.na(training_set$Age),],
                    method = "anova")

training_set$Age[is.na(training_set$Age)] = predict(age_predict,training_set[is.na(training_set$Age),])

summary(training_set)


which(training_set$Embarked == "")

training_set$Embarked[c(62,830)] = "S"

summary(training_set)
training_set$Embarked <- factor(training_set$Embarked)

install.packages("randomForest")
library(randomForest)

classifier = randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                          training_set,
                          ntree = 3000)

y_pred = predict(classifier,training_set)

cm = table(training_set$Survived,y_pred)
cm


install.packages("party")
library(party)
set.seed(234)

classifier = cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data = training_set, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

y_pred = predict(classifier, test_set, OOB=TRUE, type = "response")

cm = table(training_set$Survived,y_pred)
cm

submit = data.frame(PassengerId = test_set$PassengerId,Survived = y_pred)
write.csv(submit,"final.csv",row.names = FALSE)
