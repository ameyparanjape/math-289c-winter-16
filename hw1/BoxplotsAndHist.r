# Code used to generate boxplots and histograms
# Data used is babies23.txt (switch string for other data sets)
# There are 2 subsections of histograms, depending on which you want to use.
# NOTE: Import babies23.txt as the dataset 

################################################################################
# Boxplot Creation Section
################################################################################

# create colors and labels vectors to help visualize groups
colors <- c("salmon", "cadetblue1", "aquamarine", "thistle1")
labels <- c("Never Smoked", "Currently Smokes", "Until Pregnancy", "Smoked Once, Not Now")

# Create a group of data that does not include smoke == 9
data.answered <- babies23[babies23$smoke != 9,]

# Boxplot creation (One comparing all groups that answered)
boxplot(data.answered$bwt~data.answered$smoke, main="Birth Weight of Baby in Ounces for Mothers of Various Smoking Status", las=2, col=colors, xlab="Mother's Smoking Status", ylab="Baby's Birth Weight in Ounces", xaxt="n")
# Create labels for x-axis tic marks
axis(1, at=1:4, labels=labels)

# Create a group of data that only compares mothers that never smoked to smokers
data.narrow <- babies23[babies23$smoke != 9 & babies23$smoke != 3 & babies23$smoke !=2,]

# Boxplot creation (One comparing only never smoked to current smokers)
boxplot(data.narrow$bwt~data.narrow$smoke, main="Birth Weight of Baby in Ounces of NonSmoking vs Smoking Mothers", las=2, col=colors, xlab="Mother's Smoking Status", ylab="Baby's Birth Weight in Ounces", xaxt="n")
# Create labels for x-axis tic marks
axis(1, at=1:4, labels=labels)

################################################################################
# Histogram Creation Below
################################################################################

# Group the non smokers
data.nonsmoke <- babies23[babies23$smoke == 0,]

# Group the current smokers
data.current <- babies23[babies23$smoke == 1,]

# Group the smokers that quit right before preg
data.quitters <- babies23[babies23$smoke == 2,]

# Group the smokers that quit awhile ago
data.oldsmoker <- babies23[babies23$smoke == 3,]

# Create titles for the groups
title.current <- "Birth Weight of Babies of Smoking Mothers"
title.nonsmoke <- "Birth Weight of Babies of Nonsmoking Mothers"
title.quitters <- "Birth Weight of Babies of Mothers that Quit Before Pregnancy"
title.oldsmoker <- "Birth Weight of Babies of Mothers that Smoked in the Past"

  #####################
  # ggplot subsection #
  #####################

# Hist creation for mothers that don't smoke
hist.nonsmoke <- ggplot(data=data.nonsmoke, aes(wt))
hist.nonsmoke + geom_histogram() + ggtitle("Birth Weight for Babies of Nonsmoker Mothers") + labs(x="Birth Weight in Ounces", y="Quantity of Births")

# Hist creation for mothers that do smoke
hist.smoke <- ggplot(data=data.smoke, aes(wt))
hist.smoke + geom_histogram() + ggtitle("Birth Weight for Babies of Smoker Mothers") + labs(x="Birth Weight in Ounces", y="Quantity of Births")

# Hist creation for mothers that quit before preg
hist.quitters <- ggplot(data=data.quitters, aes(wt))
hist.quitters + geom_histogram() + ggtitle("Birth Weight for Babies of Mothers that Quit Before Preg") + labs(x="Birth Weight in Ounces", y="Quantity of Births")

# Hist creation for mothers that quit awhile before preg
hist.old <- ggplot(data=data.oldsmoker, aes(wt))
hist.old + geom_histogram() + ggtitle("Birth Weight For Babies of Mothers that Used to Smoke") + labs(x="Birth Weight in Ounces", y="Quantity of Births")

  #########################################################
  # Built-in R hist using Freedman-Diaconis rule for bins #
  #########################################################

# Hist creation for mothers that don't smoke
hist(x=data.nonsmoke$bwt,breaks="FD", xlab="Birth Weight in Ounces", ylab="Quantity of Births", main=title.nonsmoke, col="salmon", labels=TRUE)

# Hist creation for mothers that do smoke
hist(x=data.current$bwt,breaks="FD", xlab="Birth Weight in Ounces", ylab="Quantity of Births", main=title.current, col="cadetblue1", labels=TRUE)

# Hist creation for mothers that quit before pregnancy
hist(x=data.quitters$bwt,breaks="FD", xlab="Birth Weight in Ounces", ylab="Quantity of Births", main=title.quitters, col="aquamarine", labels=TRUE)

# Hist creation for mothers that quit awhile before pregnancy
hist(x=data.oldsmoker$bwt,breaks="FD", xlab="Birth Weight in Ounces", ylab="Quantity of Births", main=title.oldsmoker, col="thistle1", labels=TRUE)
