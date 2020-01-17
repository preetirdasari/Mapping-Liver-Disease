scatterplotMatrix(~Age+TB+DB+Proteins+Alkphos|Selector, data=Liver, smooth=FALSE, regLine=FALSE,ellipse=FALSE,
                   main="Scatterplot Matrix of Liver Data")

scatterplotMatrix(~AdjClose+Open+LogRange+Volume+Lag1r+Lag2r|Risk, data=praret, smooth=FALSE, regLine=FALSE,
                  ellipse=FALSE,col=carPalette()[-2],main="Scatterplot Matrix of PRA Stock Data")
#Liver Data#

#To evaluate performance 
set.seed(1)
train = Liver %>%
  sample_n(437)
test = Liver %>%
  setdiff(train)

#Classification Tree#
tree = rpart(Selector~.-Selector, train)
summary(tree)
print(tree)
plot <- rpart.plot(tree, tweak = 1, fallen.leaves = FALSE, box.col = c("royalblue", "seagreen4"))

#Pruning#

a=rep(1,583)
a[Liver[,7]=="No"]=0
a[572:583]
newliver = cbind(Liver,a)

length(newliver$TB)
length(newliver$a)

set.seed(1)
train.boost = newliver %>%
  sample_n(437)
test.boost = newliver %>%
  setdiff(train.boost)

tree.preds = predict(tree, test, type="class")
table(tree.preds, test$Selector)


liver.tree2 = rpart(a~Age+TB+DB+Alkphos+Proteins, train.boost)
bestcp.liver = liver.tree2$cptable[which.min(liver.tree2$cptable[,"xerror"]),"CP"]
pruned.liver = prune(liver.tree2, cp=bestcp.liver)
prune.liver = predict(pruned.liver, test.boost)
table(prune.liver, test.boost$a)
rpart.plot(pruned.liver, fallen.leaves = FALSE)
print(prune.liver)
summary(prune.liver)

#Boosting#

set.seed(1)
boost = gbm(a~Age+TB+DB+Alkphos+Proteins, data = train.boost, distribution = "bernoulli", n.trees = 5000, 
                   interaction.depth = 5)
summary(boost)


par(mfrow=c("a", "Alkphos", "Age"))
plot(boost,i="Alkphos")
plot(boost,i="Age")
plot(boost, i="TB")

yhat_boost = predict(boost, newdata = test.boost, n.trees = 5000)
mean((yhat_boost-test.boost$a)^2)

boost2 = gbm(a~Age+TB+DB+Alkphos+Proteins, data = train.boost, distribution = "bernoulli", n.trees = 5000,
             interaction.depth = 5, shrinkage = 0.01, verbose = F)
summary(boost2)
plot(boost2, i="Alkphos")
yhat_boost2 = predict(boost2, newdata = test.boost, n.trees = 5000)
mean((yhat_boost2-test.boost$a)^2)

#Logistic Regression
glm.fit = glm(Selector~.-Gender, data=Liver, family=binomial)
contrasts(Liver$Selector)
summary(glm.fit)
glmprobs = predict(glm.fit, type="response")
sample(glmprobs)
glm.forecast = Liver$Selector
glm.forecast[glmprobs>.5]="Yes"
glm.forecast[glmprobs<.5]="No"
summary(glm.forecast)

confusion.matrix = table(glm.forecast, Liver$Selector)
confusion.matrix
(confusion.matrix[1,2]+confusion.matrix[2,1])/sum(confusion.matrix)


  
#Log Range Data#
praret <- PRAret[-c(1:3),]

#To evaluate performance 
set.seed(1)
pra.train = praret %>%
  sample_n(2284)
pra.test = praret %>%
  setdiff(pra.train)

#Classification Tree#
pra.tree = rpart(Risk~LogRange+AdjClose+Range+Volume+Close+High+Low+Open, pra.train)
summary(pra.tree)
print(pra.tree)
pra.plot <- rpart.plot(pra.tree, tweak = 1.4, fallen.leaves = TRUE, box.col = c("steelblue3", "slateblue2"))

#Pruning#

names(praret)
pra.pred = predict(pra.tree, pra.test, type="class")
table(pra.pred, pra.test$Risk)

r=rep(1,3046)
r[praret[,13]=="LowRisk"]=0
newpra = cbind(praret,r)

set.seed(1)
pra.train2 = newpra %>%
  sample_n(2284)
pra.test2 = newpra %>%
  setdiff(pra.train2)

pra.tree2 = rpart(r~LogRange+AdjClose+Range+Volume+Close+High+Low+Open, pra.train2)
bestcp2 = pra.tree2$cptable[which.min(pra.tree2$cptable[,"xerror"]),"CP"]
pruned = prune(pra.tree2, cp=bestcp2)
prune.pred = predict(pruned, pra.test2)
table(prune.pred, pra.test2$r)
rpart.plot(pruned, fallen.leaves = FALSE)

#Boosting#


set.seed(1)
pra.boost = gbm(r~LogRange+AdjClose+Range+Volume+Close+High+Low+Open, pra.train2, distribution = "bernoulli", n.trees = 5000, 
            interaction.depth = 5)
summary(pra.boost)

par(mfrow=c("LogRange", "Volume", "AdjClose"))
plot(pra.boost,i="LogRange")
plot(pra.boost2, i="LogRange")
plot(pra.boost,i="Volume")


yhat_boost2 = predict(pra.boost, newdata = pra.train2, n.trees = 5000)
mean((yhat_boost2-pra.train2$r)^2)

pra.boost2 = gbm(r~LogRange+AdjClose+Range+Volume+Close+High+Low+Open, pra.train2, distribution = "bernoulli", n.trees = 5000, 
                 interaction.depth = 5, shrinkage = 0.1, verbose = F)
summary(pra.boost2)
plot(pra.boost2, i="LogRange")
yhat_boost3 = predict(pra.boost2, newdata = pra.test2, n.trees = 5000)
mean((yhat_boost3-pra.test2$r)^2)

#Logistic Regression#

glm2 = glm(r~LogRange+AdjClose+Range+Volume+Close+High+Low+Open, data=newpra, family=binomial)
summary(glm2)
probs = predict(glm2, type="response")
sample(probs)

forecast = newpra$r
forecast[probs>.5]="High"
forecast[probs<.5]="Low"
summary(forecast)

matrix = table(forecast, newpra$r)
matrix
