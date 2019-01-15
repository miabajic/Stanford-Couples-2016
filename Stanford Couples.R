getwd()

setwd("/Users/miabajic/assignment-3-miabajic")

x = readRDS("hcmst.rds")

#1
head(x)
summary(x$relationship_quality)
summary(x$income)
summary(x$gender_attraction)

#2
summary(x$religion)
summary(x$race)
summary(x$gender)
summary(x$age)
homosexualcouple = x$partner_same_sex
t.test(homosexualcouple)
#Frequency of Religions barchart
barchart(x$religion, main = "Frequency of Religions", xlab = "Frequency")
#Frequency of Races barchart 
barchart(x$race, main = "Frequency of Races", xlab = "Frequency")
plot(x$income, main = "Distribution of Income", xlab = "Income Brackets", ylab = "Frequency")
#Gender Representation Among Income Brackets plot
plot(x$income, x$gender, main = "Gender Representation Among Income Brackets", xlab = "Income Brackets", ylab = "Gender")

#3
length(x$id)
table(x$breakup)
table(x$got_married)
#unmarried couples
unmarried = subset(x, !married)
#cross-tabulate unmarried couples who got married and breakup
ctab = table(unmarried$got_married, unmarried$breakup)
mb = subset(unmarried, got_married & breakup == "yes")
addmargins(ctab)

#4
subjectage = x$age
partnerage = x$partner_age
totalage = subjectage + partnerage
breakup = subset(x, breakup=="T")
boxplot(totalage ~ breakup, x, main = "Breakup vs Combined Age", xlab = "Breakup", ylab = "Combined Age (Years)")
relage = x$years_relationship
boxplot(relage ~ breakup, x, main = "Breakup vs Age of Relationship", xlab = "Breakup", ylab = "Age of Relationship (Years)")

#5
median(x$age)
median(x$partner_age, na.rm=T)

#heterosexual couples
h = subset(x, !x$partner_same_sex)
#heterosexual couple age difference
h$agediff = h$age - h$partner_age
#heterosexual couple females
females = h$gender == "female"

h$m_f_age = h$agediff
#make negative age differences female
h$m_f_age[females] = -h$agediff[females]
#Histogram of Age Difference 
hist(h$m_f_age[abs(h$m_f_age) <=20], main = "Histogram of Age Difference", xlab = "Age Difference")
median(h$m_f_age, na.rm=T)

#6
#subset same religion
samereligion = subset(x, religion == partner_religion)
#subset different religion
diffreligion = subset(x, religion != partner_religion)
table(samereligion$breakup)
table(diffreligion$breakup)
plot(samereligion$breakup)

#subset same years of education
sameedu = subset(x, years_edu == partner_years_edu)
#subset different years of education
diffedu = subset(x, years_edu != partner_years_edu)
table(sameedu$breakup)
table(diffedu$breakup)
#subset same political party
samepolitic = subset(x, politic == partner_politic)
diffpolitic = subset(x, politic != partner_politic)
table(samepolitic$breakup)
table(diffpolitic$breakup)
table(x$same_hometown, x$breakup)

mosaicplot(x$breakup ~ x$same_hometown, main = "Breakup vs Same Hometown", xlab = "Breakup", ylab = "Same Hometown")

#7
#set bigger margins
par(mar = c(5, 6, 4, 2))
tab = table(x$relationship_quality, x$breakup)
tab = t(tab)
#change label names
ylabels = colnames(tab)
colnames(tab) = rep(NA, ncol(tab))

xlabels = rownames(tab)
rownames(tab) = rep(NA, nrow(tab))
#Spineplot of Breakup vs Relationship Quality
spineplot(tab, main = "Breakup vs Relationship Quality", xlab = "Breakup")
#change label locations
axis(2, c(0, .05, .15, .5, .8), ylabels, las = 2, tick = FALSE)
axis(1, c(.1, .3, .7), xlabels, tick = FALSE, cex.axis = 0.7)
title(ylab = "Relationship Quality", line = 5)

#Relationship Quality vs Breakup barplot
barplot(x$relationship_quality, x$breakup, main = "Relationship Quality vs Breakup", las = 3)

#Relationship Quality vs Number of Kids density plot
densityplot(~ children, x, main = "Relationship Quality vs Number of Kids", groups = relationship_quality, auto.key = T)

#Breakup vs Parental Approval mosaic plot 
plot(x$breakup, x$parental_approval, main = "Breakup vs Parental Approval")


#8
#make margins bigger
par(mar = c(5, 12, 4, 2))
levels(x$met_at)
#create is old variable
x$isold = x$age > 50
tab = table(x$met_at, x$isold)
#Where Couples Met barplot
barplot(t(tab), beside = T, horiz = TRUE, las = 2, main = "Where Couples Met", xlab = "Frequency")
#create legend
Legends = c("Old", "Young")
legend("topright", legend = Legends, fill = c("grey", "black"))

# For personal use, decided not to use in report
old = subset(x, age > 50 & met_at != "other")
young = subset(x, age < 50 & met_at != "other")
barchart(old$met_at, main = "Old Couples Meet", xlab = "Frequency")
barchart(young$met_at, main = "Young Couples Meet", xlab = "Frequency")
# Percentage of couples that met online for young and old
table(young$met_online)
table(old$met_online)

#9
#set samegenderattraction as a variable
x$samegenderattraction = x$gender_attraction 
#change the levels of this variable to simplify
levels(x$samegenderattraction) = c("Opposite", "Opposite", NA, "Same", "Same")
#create variable for same gender couple
x$samegendercouple = x$partner_gender == x$gender
#Create TRUE or FALSE for variable 
x$samegendercouple = factor(x$samegendercouple, labels = c("Opposite Gender Couple", "Same Gender Couple"))
#Cross-tabulate same gender attraction, same gender couples, and breakup
t = table(x$samegenderattraction, x$samegendercouple, x$breakup)
#Change from frequencies to proportions
pro = prop.table(t, c(1, 2))
#Create side by side barchart matrix
barchart(pro, stack = F, auto.key = list(title = "Breakup", cex = 0.5), ylab = "Preferred Gender", xlab = "Frequency")

#10
#Part a
#Were participants who were dating the gender they were less attracted to more likely to be the initiator of the breakup?
plot(d$breakup, d$breakup_who, main = "Breakup vs Breakup Initiator for Not Matched")
plot(s$breakup, s$breakup_who, main = "Breakup vs Breakup Initiator for Matched")

#Part b
# Is there a relationship between relationship quality and years living together?
#Relationship Quality vs Years Since Meeting density plot
densityplot(~ years_met, x, main = "Relationship Quality vs Years Since Meeting", groups = relationship_quality, auto.key = T)

#Part c
# Is there a relationship between relationship quality and income?

levels(x$income) = c("Low Income", "Low Income", "Low Income", "Low Income", "Low Income", "Middle Income", "Middle Income", "Middle Income", "Middle Income", "Middle Income", "Middle Income", "Middle Income", "High Income", "High Income", "High Income", "High Income", "High Income", "High Income", "High Income")
plot(x$income, x$relationship_quality, main = "Income vs Relationship Quality", xlab = "Income Level", ylab = "Relationship Quality")