# Reading in the data
College=read.csv("../Data/College.csv")

# Converting the first column into row names
rownames(College)=College[,1]
College=College[,-1]

# Producing a matrix of pairwise plots, saved to Pairs.png
pairs(College)
dev.copy(png,'./Output/Ex8_Pairs.png', width = 40, height = 40, units = 'cm', res = 300); dev.off()

# Creating boxplots of Outstate vs Private
plot(Outstate~Private,College)
dev.copy(png,'./Output/Ex8_Boxplot.png', width = 10, height = 10, units = 'cm', res = 300); dev.off()

# Define Elite category
Elite = rep("No",nrow(College))
Elite[College$Top10perc>50]="Yes"
Elite=as.factor(Elite)
College=data.frame(College,Elite)

# Boxplot of Outstate vs Elite
plot(Outstate~Elite,College)
dev.copy(png,'./Output/Ex8_Boxplot2.png', width = 10, height = 10, units = 'cm', res = 300); dev.off()

# Making histograms
par(mfrow=c(2,2))
hist(College$Enroll,50,xlab="Enroll",main="Histogram of Enroll")
hist(College$Apps,50,xlab="Apps",main="Histogram of Apps")
hist(College$Accept,50,xlab="Acept",main="Histogram of Accept")
hist(College$PhD,50,xlab="PhD",main="PhD")
dev.copy(png,'./Output/Ex8_Histograms.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

