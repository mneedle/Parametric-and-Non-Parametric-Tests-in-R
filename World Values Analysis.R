library(psych)
library(lawstat)
library(stats)
library(agricolae)
library(plyr)
library(car)
library(onewaytests)
library(gplots)
library(readxl)
library(afex)
require(lsmeans)
library(lsmeans)
require(multcomp)
data(obk.long)
library(data.table)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
library(polycor)
library(MASS)
library(rcompanion)


### Create scales
# Load data
data <- read_excel("WV2015A.xlsx")

# Build scales
resistance_to_tech <- c(222,223,227,70)
resistance_to_tech_scale <- data[resistance_to_tech]
resistance_to_tech_scale[resistance_to_tech_scale<0] <- NA
rt_1 <- 11-resistance_to_tech_scale[1]
rt_1_z <- (rt_1 - mean(as.matrix(rt_1), na.rm=TRUE))/sd(as.matrix(rt_1), na.rm=TRUE)
rt_2 <- 11-resistance_to_tech_scale[2]
rt_2_z <- (rt_2 - mean(as.matrix(rt_2), na.rm=TRUE))/sd(as.matrix(rt_2), na.rm=TRUE)
rt_3 <- 11-resistance_to_tech_scale[3]
rt_3_z <- (rt_3 - mean(as.matrix(rt_3), na.rm=TRUE))/sd(as.matrix(rt_3), na.rm=TRUE)
rt_4 <- resistance_to_tech_scale[4]
rt_4_z <- (rt_4 - mean(as.matrix(rt_4), na.rm=TRUE))/sd(as.matrix(rt_4), na.rm=TRUE)
resistance_to_tech_z_df <- cbind(rt_1_z,rt_2_z,rt_3_z,rt_4_z)
alpha(as.matrix(resistance_to_tech_z_df[1:4])) #raw_alpha= .75
resistance_to_tech_z_df$non_na <- 4 - rowSums(is.na(resistance_to_tech_z_df))
resistance_to_tech_z_df$sum <- rowSums(resistance_to_tech_z_df[1:4], na.rm=TRUE)
resistance_to_tech_z_scale <- resistance_to_tech_z_df$sum/resistance_to_tech_z_df$non_na
hist(resistance_to_tech_z_scale, main="Resistance to Technology Scale", ylab="Frequency", xlab="Z-Score") # histogram

optimism<- c(57,24,108,11)
optimism_scale <- data[optimism]
optimism_scale[optimism_scale<0] <- NA
o_1 <- optimism_scale[1]
o_1_z <- (o_1 - mean(as.matrix(o_1), na.rm=TRUE))/sd(as.matrix(o_1), na.rm=TRUE)
o_2 <- 3-optimism_scale[2]
o_2_z <- (o_2 - mean(as.matrix(o_2), na.rm=TRUE))/sd(as.matrix(o_2), na.rm=TRUE)
o_3 <- 5-optimism_scale[3]
o_3_z <- (o_3 - mean(as.matrix(o_3), na.rm=TRUE))/sd(as.matrix(o_3), na.rm=TRUE)
o_4 <- 5-optimism_scale[4]
o_4_z <- (o_4 - mean(as.matrix(o_4), na.rm=TRUE))/sd(as.matrix(o_4), na.rm=TRUE)
optimism_z_df <- cbind(o_1_z,o_2_z,o_3_z,o_4_z)
alpha(as.matrix(optimism_z_df[1:4])) #raw_alpha= .35
optimism_z_df$non_na <- 4 - rowSums(is.na(optimism_z_df))
optimism_z_df$sum <- rowSums(optimism_z_df[1:4], na.rm=TRUE)
optimism_z_scale <- optimism_z_df$sum/optimism_z_df$non_na
hist(optimism_z_scale, main="Optimism Scale", ylab="Frequency", xlab="Z-Score") # histogram

ageism <- c(182,184,185,187,188)
ageism_scale <- data[ageism]
ageism_scale[ageism_scale<0] <- NA
ag_1 <- 5-ageism_scale[1]
ag_1_z <- (ag_1 - mean(as.matrix(ag_1), na.rm=TRUE))/sd(as.matrix(ag_1), na.rm=TRUE)
ag_2 <- 11-ageism_scale[2]
ag_2_z <- (ag_2 - mean(as.matrix(ag_2), na.rm=TRUE))/sd(as.matrix(ag_2), na.rm=TRUE)
ag_3 <- 5- ageism_scale[3]
ag_3_z <- (ag_3 - mean(as.matrix(ag_3), na.rm=TRUE))/sd(as.matrix(ag_3), na.rm=TRUE)
ag_4 <- 5-ageism_scale[4]
ag_4_z <- (ag_4 - mean(as.matrix(ag_4), na.rm=TRUE))/sd(as.matrix(ag_4), na.rm=TRUE)
ag_5 <- 5-ageism_scale[5]
ag_5_z <- (ag_5 - mean(as.matrix(ag_5), na.rm=TRUE))/sd(as.matrix(ag_5), na.rm=TRUE)
ageism_z_df <- cbind(ag_1_z,ag_2_z,ag_3_z,ag_4_z,ag_5_z)
alpha(as.matrix(ageism_z_df[1:5])) #raw_alpha= .33
ageism_z_df$non_na <- 5 - rowSums(is.na(ageism_z_df))
ageism_z_df$sum <- rowSums(ageism_z_df[1:5], na.rm=TRUE)
ageism_z_scale <- ageism_z_df$sum/ageism_z_df$non_na
hist(ageism_z_scale, main="Ageism Scale", ylab="Frequency", xlab="Z-Score") # histogram

# Build other variables
dep_variable <- data[180]
dep_variable[dep_variable<0] <- NA

additional_variables <- cbind(data[297], data[28], data[302])
additional_variables[additional_variables<0] <- NA
additional_variables[additional_variables==8] <- NA

variables <- cbind(dep_variable, resistance_to_tech_z_scale, optimism_z_scale, 
                   ageism_z_scale, additional_variables)

#Histogram of Employment Status
plotNormalHistogram(data.frame(variables$V229), density= 15, col= "black",
                    main="Employment Status", 
                    ylab="Frequency", 
                    xlab= "",
                    xaxt = "n")
axis(1, at=1:7, las= 2, labels=c("Full-Time", "Part-Time", "Self-\nEmployed", "Retired", "Housewife", "Student", "Unemployed"), par(cex.axis=0.79))

#Histogram of Labor Union Membership
plotNormalHistogram(data.frame(variables$V28), density= 15, col= "black",
                    main="Labor Union Membership", 
                    ylab="Frequency", 
                    xlab= "",
                    xaxt = "n")
axis(1, at=0:2, las= 2, labels=c("Non-Member","Inactive Member","Active Member"), par(cex.axis=0.79))

#Histogram of Acceptance of Young Bosses (dependent variable)
plotNormalHistogram(data.frame(dep_variable), density= 15, col= "black",
                    main="Is a 30-year old boss acceptable?", 
                    ylab="Frequency",
                    xlab = "Response")



### Inferential statistical tests
# Two independent samples t-test for whether they are supervising someone
managers= as.matrix(subset(variables[1], variables[7]==1))
non_managers= as.matrix(subset(variables[1], variables[7]==2))
t.test(managers,non_managers, var.equal = FALSE, conf.level = 0.95)
boxplot(V160~V234, data= variables, col = "blue", main= "Acceptance of Young Bosses \nby Managers/Non-Managers",ylab="Acceptance of Young Bosses",names= c("Managers", "Non-Managers"))
# no difference between managers and non-managers in terms of Acceptance of Young Bosses (p = 0.213)


# ANOVA, HSD to excel, level descriptions, levenes and welch tests, and means plot for employment status
employment_anova=aov(V160~V229, data=variables)
summary(employment_anova) #p= 0.00111

employment_comparisons= data.frame(TukeyHSD(aov(variables$V160~as.factor(variables$V229)))$`as.factor(variables$V229)`)
setDT(employment_comparisons, keep.rownames = TRUE)[]

tapply(variables$V160,variables$V229,mean, na.rm=TRUE) # means 
tapply(variables$V160,variables$V229,sd, na.rm=TRUE) # standard deviations

leveneTest(variables$V160, variables$V229, center=mean) # significant levene's test
welch.test(V160~V229, data=variables) # significant welch's t-test (p = 4.12e-18)

plotmeans(V160 ~ V229, 
          data = variables,
          main="Acceptance of Young Bosses \nby Employment Status",
          ylab="Acceptance of Young Bosses",
          n.label = FALSE,
          xlab= "",
          xaxt = "n")
axis(1, at=1:7, las= 2, labels=c("Full-Time", "Part-Time", "Self-\nEmployed", "Retired", "Housewife", "Student", "Unemployed"), par(cex.axis=0.79))
# Acceptance of Young Bosses is significnatly different based on Employment Status (p=4.12e-18). 
# More specifically, Full-Time employees are significantly more accepting than Retired (p=0.049) and Unemployed (p=0.009) people.


# ANOVA, HSD, level descriptions, levenes and welch tests, and means plot for labor union membership
union_anova=aov(V160~V28, data=variables) 
summary(union_anova) #p< 2e-16

TukeyHSD(aov(variables$V160~as.factor(variables$V28))) #Significant differences: 0&1, 0&2

leveneTest(variables$V160, variables$V28, center=mean) # significant levene's test
welch.test(V160~V28, data=variables) # significant welch's t-test (p = 1.39e-37)

plotmeans(V160 ~ V28, 
          data = variables,
          main="Acceptance of Young Bosses \nby Labor Union Membership", 
          ylab="Labor Union Membership",
          n.label = FALSE,
          xlab= "",
          xaxt = "n")
axis(1, at=1:3, las= 2, labels=c("Non-Member","Inactive Member","Active Member"), par(cex.axis=0.79))
# Acceptance of Young Bosses is significnatly different based on Labor Union Membership (p=1.39e-37). 
# More specifically, non-members are more accepting that both inactive (p<1e-7) and active (e<1e-7) members.


# Two-way ANOVA and HSD to excel
COPY<-data.frame(variables)
COPY$V28 = COPY$V28 +1
COPY$V229<-factor(COPY$V229, c(1,2,3,4,5,6,7), labels=c("Full-Time", "Part-Time", "Self-Employed", "Retired", "Housewife", "Student", "Unemployed"))
COPY$V28<-factor(COPY$V28, levels= c(1:3), labels= c("Non-Member", "Inactive Member", "Active Member"))
new_two_way= aov(V160 ~ V229 + V28 + V229:V28, data= COPY)
model= lm(V160 ~ V229 + V28 + V229:V28, data= COPY)
newest_anova= Anova(model, type="III")


summary(new_two_way)
leveneTest(V160 ~ V229*V28, data= COPY) #significant levene's test
mult_comp= data.frame(TukeyHSD(new_two_way)$`V229:V28`)
setDT(mult_comp, keep.rownames = TRUE)[]


# Means plot
means= as.matrix(model.tables(new_two_way, type="means", cterms= c("V229:V28"))$tables$`V229:V28`) # All groups have a mean
xdata <- c(1:7)
y1 <- matrix(means[,1])
y2 <- matrix(means[,2])
y3 <- matrix(means[,3])

par(mar=c(5.1, 4.1, 4.1, 9.1), xpd=TRUE)
plot(xdata, y1, type="o", col="blue", pch="o", lty=1, ylim=c(3,7),ylab="", main="Interaction Plot", yaxt= "n", xlab= "", xaxt = "n")
mtext("Employment Status", side=1, line= 5, cex.lab=1,las=1)
mtext("Acceptance of Young Bosses", side=2, line= 2.5, cex.lab=1)
axis(1, at=1:7, las= 2, labels=c("Full-Time", "Part-Time", "Self-\nEmployed", "Retired", "Housewife", "Student", "Unemployed"), par(cex.axis=0.79))
axis(2, las= 2)

points(xdata, y2, col="red", pch="*", lty=2)
lines(xdata, y2, col="red")

points(xdata, y3, col="dark red",pch="+")
lines(xdata, y3, col="dark red", lty=3)

legend("topright", inset=c(-0.49,0),title= "Labor Union Membership", legend=c("Non-Member","Inactive Member","Active Member"), col=c("blue","red","black"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=1, cex = 0.85)
# There is a significant interaction between Employment Status and Labor Union Membership in terms of Acceptance of Young Bosses (p=2.84e-15).
# More specifically, all non-members of labor union are significantly different than inactive labor union members (except self-employed people),
# while the differences between non-members and active labor union members changes based on employment status.


# Correlation Matrix
corr_matrix= rquery.cormat(variables, type="flatten", graph=FALSE)$r  
# Acceptance of Young Bosses is significantly correlated with Resistance to Technology (r = -0.35,	p < 1e-5),
# Ageism (r = -0.25, p < 1e-5), Optimism	(r = 0.058, p <1e-5). Resistance to Technology is significantly
# correlated with Optimism (r = -0.15, p < 1e-5) and Ageism	(r = 0.28, p< 1e-5). Ageism and Optism are significantly
# correlated (r = -0.14, p < 1e-5).


# Scatterplots
plot(COPY$resistance_to_tech_z_scale, COPY$V160, main="Resistant to Technology Scatterplot",
     xlab="Resistance to Technology (Z-Score)", ylab="Acceptance of Young Bosses")
abline(lm(COPY$V160~COPY$resistance_to_tech_z_scale), col="red", lwd= 3)

plot(COPY$optimism_z_scale, COPY$V160, main="Optimism Scatterplot",
     xlab="Optimism (Z-Score)", ylab="Acceptance of Young Bosses")
abline(lm(COPY$V160~COPY$optimism_z_scale), col="red", lwd= 3)

plot(COPY$ageism_z_scale, COPY$V160, main="Ageism Scatterplot",
     xlab="Ageism (Z-Score)", ylab="Acceptance of Young Bosses")
abline(lm(COPY$V160~COPY$ageism_z_scale), col="red", lwd= 3)

# Binary
poly_ML <- polyserial(variables$V234, variables$V160, ML= TRUE, std.err= TRUE) #r= 0.01796
std.err_ML <- sqrt(poly_ML$var[1, 1])
p_value_ML <- 2 * pnorm(-abs(poly_ML$rho / std.err_ML))
p_value_ML #p= 0.15703

plot(COPY$V234, COPY$V160, 
     main="Manager/Non-Manager Scatterplot", 
     ylab="Acceptance of Young Bosses",
     n.label = FALSE,
     xlim= c(0.5,2.5),
     xlab= "",
     xaxt = "n")
axis(1, at=1:2, labels= c("Managers", "Non-Managers"), par(cex.axis=1))
abline(lm(COPY$V160~COPY$V234), col="red", lwd= 3)


##MULTIPLE REGRESSION##
#Overall (both managers and non-managers)
multireg=lm(V160~resistance_to_tech_z_scale+optimism_z_scale+ageism_z_scale+V234, data=COPY)
summary(multireg)
plotNormalHistogram(resid(multireg), # check assumption of linearity
                    main = "Histogram of Overall Multiple Regression Residuals",
                    xlab="Residuals",
                    col="cyan",
                    border="#000000")
# Resistance to technology, ageism, and managerial status (but not optimism) significantly predict Acceptance of Young Bosses.


#Split data
managers_df= subset(COPY, COPY$V234==1)
non_managers_df= subset(COPY, COPY$V234==2)

# Multiple regression with only managers
managers_reg=lm(V160~resistance_to_tech_z_scale+optimism_z_scale+ageism_z_scale, data=managers_df)
summary(managers_reg)
plotNormalHistogram(resid(managers_reg), # check assumption of linearity
                    main = "Histogram of Multiple Regression Residuals\nfor Managers",
                    xlab="Residuals",
                    col="cyan",
                    border="#000000")
# Resistance to technology and ageism (but not optimism) significantly predict Acceptance of Young Bosses.


# Multiple regression with only non-managers
non_managers_reg=lm(V160~resistance_to_tech_z_scale+optimism_z_scale+ageism_z_scale, data=non_managers_df)
summary(non_managers_reg)
plotNormalHistogram(resid(non_managers_reg), # check assumption of linearity
                    main = "Histogram of Multiple Regression Residuals\nfor Non-Managers",
                    xlab="Residuals",
                    col="cyan",
                    border="#000000")
# Resistance to technology and ageism (but not optimism) significantly predict Acceptance of Young Bosses


###CHI SQUARE###
chitable=table(COPY$V229,COPY$V28)
prop.table #cell
prop.table(chitable,1) #row
prop.table(chitable,2)#column
chisq.test(COPY$V28,COPY$V229)
# Across Employment Statuses, people are more likely to not be Labor Union Members than to be Active or Inactive Members
# Across Labor Union Membership types, people are most likely to be Full-Time employees followed by Housewives and the other employment statuses


###MANN WHITNEY###
wilcox.test(V160~V234, data=COPY, exact = FALSE)
table(COPY$V160,COPY$V234)
boxplot(V160~V234, data= variables, col="cyan", barcol="#FFFFFF", main ="Median Acceptance of Young Bosses\nby Manager/Non-Manager",ylab="Acceptance of Young Bosses",names= c("Managers", "Non-Managers"))
# no significant difference in Acceptance of Young Bosses between managers and non-managers.


##KRUSKAL WALLACE##
kruskal.test(COPY$V160~COPY$V28)
boxplot(V160~V28, data= variables, col="cyan", barcol="#FFFFFF", main ="Median Acceptance of Young Bosses\nby Labor Union Membership",ylab="Acceptance of Young Bosses", xlab= "Labor Union Membership", xaxt = "n")
axis(1, at=1:3, labels= c("Non-Member", "Inactive Member", "Active Member"))
# significant differences in Acceptance of Young Bosses based on Labor Union Membership
