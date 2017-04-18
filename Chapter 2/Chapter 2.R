# Reading in the data
College=read.csv("College.csv")

# Converting the first column into row names
rownames(College)=College[,1]
College=College[,-1]

# Producing a matrix of pairwise plots, saved to Pairs.png
pairs(College)
dev.copy(png,'Pairs.png', width = 40, height = 40, units = 'cm', res = 300); dev.off()

# Creating boxplots of Outstate vs Private
plot(Outstate~Private,College)
dev.copy(png,'Boxplot.png', width = 10, height = 10, units = 'cm', res = 300); dev.off()

# Define Elite category
Elite = rep("No",nrow(College))
Elite[College$Top10perc>50]="Yes"
Elite=as.factor(Elite)
College=data.frame(College,Elite)

# Boxplot of Outstate vs Elite
plot(Outstate~Elite,College)
dev.copy(png,'Boxplot2.png', width = 10, height = 10, units = 'cm', res = 300); dev.off()

# Making histograms
par(mfrow=c(2,2))
hist(College$Enroll,100,xlab="Enroll",main="Histogram of Enroll")
hist(College$Apps,100,xlab="Apps",main="Histogram of Apps")
hist(College$Accept,100,xlab="Acept",main="Histogram of Accept")
hist(College$PhD,100,xlab="PhD",main="PhD")
dev.copy(png,'Histograms.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Fitting a linear model for the graduation rate
fit_lin = lm(Grad.Rate~.,College)
summary(fit_lin)

# Many variables do not seem significantly predictive
fit_lin = update(fit_lin,~.-Accept-Enroll-Top10perc-F.Undergrad-Books-PhD-Terminal-S.F.Ratio-EliteYes)
summary(fit_lin)
