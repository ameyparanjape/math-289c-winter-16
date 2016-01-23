# Code used to generate boxplots and histograms
# Data used is babies23.txt (switch string for other data sets)
# ggplot2 required for histograms

################################################################################
# Boxplot Creation Section
################################################################################

# create colors and labels vectors to help visualize groups
colors <- c("red", "blue", "green", "purple")
labels <- c("Never", "Smokes Now", "Until Pregnancy", "Smoked Once, Not Now")

# Create a group of data that does not include smoke == 9
data.answered <- babies23[babies23$smoke != 9,]

# Boxplot creation (One comparing all groups that answered) (Labels not used)
boxplot(data.answered$wt~data.answered$smoke, main="Birth Weight in Ounces vs
        Mother's Smoking Status", las=2, par(mar = c(12,5,4,2)+ 0.1), col=colors,
        xlab="Mother's Smoking Status", ylab="Baby's Birth Weight in Ounces")

# Create a group of data that only compares mothers that never smoked to smokers
data.narrow <- babies23[babies23$smoke != 9 & babies23$smoke != 3 & babies23$smoke !=2,]

# Boxplot creation (One comparing only never smoked to current smokers)
boxplot(data.narrow$wt~data.narrow$smoke,
        main="Birth Weight in Ounces vs Mother's Smoking Status", las=2,
        par(mar = c(12,5,4,2)+ 0.1), col=colors, xlab="Mother's Smoking Status",
        ylab="Baby's Birth Weight in Ounces")

################################################################################
# Histogram Creation Below
################################################################################
# Grouping data
# Group the non smokers
data.nonsmoke <- babies23[babies23$smoke == 0,]

# Group the current smokers
data.current <- babies23[babies23$smoke == 1,]

# Group the smokers that quit right before preg
data.quitters <- babies23[babies23$smoke == 2,]

# Group the smokers that quit awhile ago
data.oldsmoker <- babies23[babies23$smoke == 3,]

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
