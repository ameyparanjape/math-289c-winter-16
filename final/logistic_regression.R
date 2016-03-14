# Divide train into thirds
train.one = train[,1:40]

logit <- glm(train.one$target ~ . - train.one$ID, data = train, family = "binomial", na.action = "na.omit")
