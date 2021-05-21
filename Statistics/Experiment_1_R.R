#This script explores some hypothesis that have been established after plotting the data within excel
#these analysis are not complete as statistics were first conducted within SPSS 

ACC2_analysis <- read.csv("~/Semester 6/Bachelor Thesis/acc2/Statistics/ACC2_analysis.csv", sep=";")
View(ACC2_analysis)

#recode the responses so that higher values relate to higher animacy ratings
library(car)
ACC2_analysis$recodedresponse <- recode(ACC2_analysis$response, '1=7;2=6;3=5;4=4;5=3;6=2;7=1')

#some simple descriptive statistics
library(psych)
describeBy(ACC2_analysis$recodedresponse,ACC2_analysis$Animacytype)

hist(ACC2_analysis$response) #is not normally distributed, however this was not expected and is ok since sample size is above 30

#check homogenity of variance assumptions
leveneTest(ACC2_analysis$response ~ ACC2_analysis$Animacytype)

#log transform reaction time as it is not normally distributed and heavlily right skewed
ACC2_analysis$logRT <- log(ACC2_analysis$RT)

#check spearman correlation between the reaction time and the response
cor.test(ACC2_analysis$logRT,ACC2_analysis$response, method = "spearman",exact = FALSE)

#create subsets of the data for further exploration
animal <- subset(ACC2_analysis, TypeNum == 1)
human <- subset(ACC2_analysis, TypeNum == 2)
object <- subset(ACC2_analysis, TypeNum == 3)
plant <- subset(ACC2_analysis, TypeNum == 4)
robot <- subset(ACC2_analysis, TypeNum == 5)
word <- subset(ACC2_analysis, TypeNum == 6)

#check wether wordlength correlates with reaction time
cor.test(word$logRT,word$wordlength,method="spearman",exact =FALSE)

#check the same for the response rating
cor.test(word$response,word$wordlength,method="spearman",exact =FALSE)

#subsetting animate and inanimate words
animate_words <- ACC2_analysis[ACC2_analysis$text == "cute"|ACC2_analysis$text == "cool"|ACC2_analysis$text == "handsome"|ACC2_analysis$text == "wonderful"|ACC2_analysis$text == "loud"|ACC2_analysis$text == "old"|ACC2_analysis$text == "great"|ACC2_analysis$text == "impressive"|ACC2_analysis$text == "extraordinairy"|ACC2_analysis$text == "basic"|ACC2_analysis$text == "awesome"|ACC2_analysis$text == "ugly",]
inanimate_words <- ACC2_analysis[ACC2_analysis$text == "huge"|ACC2_analysis$text == "sweet"|ACC2_analysis$text == "fair"|ACC2_analysis$text == "ordinary"|ACC2_analysis$text == "small"|ACC2_analysis$text == "ideal"|ACC2_analysis$text == "plain"|ACC2_analysis$text == "radient"|ACC2_analysis$text == "odd"|ACC2_analysis$text == "modern"|ACC2_analysis$text == "okay"|ACC2_analysis$text == "solid",]

#performing a Wilcoxon signed-rank test between the 
#word categories to make sure they are really differnt form each other
wilcox.test(animate_words$response,inanimate_words$response)
 
#calculating their median to report the finding
median(animate_words$recodedresponse)
median(inanimate_words$recodedresponse)

#check wether the homogenity of variance of only robot responses is violated
leveneTest(robots.only$response,robots.only$TypeNum) 

robots.only$RT <- as.numeric(robots.only$RT)
robots.only$logRT <- log(robots.only$RT)

leveneTest(robots.only$logRT,robots.only$TypeNum)

#since it is not we can do an ANOVA
raov <- aov(robots.only$response ~ robots.only$TypeNum)
summary(raov)

rrtaov <- aov(robots.only$logRT ~ robots.only$TypeNum) #not significant
summary(rrtaov)



#check if wordlength correlates with the reaction time #not significant
words <- ACC2_analysis[2321:2778,]

cor.test(words$logRT, words$wordlength)

cor.test(words$response, words$wordlength)

mean(words$response) 

median(words$response)

#take words around mean as median is the highest number
words_animate <- subset(words, response < 5.67)
words_inanimate <- subset(words,response > 5.67)

#check wether there are significant differences between these two groups
t.test(words_animate$response, words_inanimate$response)

#significant for reaction times (0.5)
t.test(words_animate$logRT, words_inanimate$logRT)

#mean of responses for words significantly differs from pictures (pictures: 3.137872, words: 5.674672)
t.test(pictures$response,words$response)

#What can we learn from participants response? 
c <- c(mean(human$response),mean(object$response),mean(plant$response),mean(robots$response),mean(animal$response))
#(animals < humans < plants < robots < objects)

c <- c(sd(human$response),sd(object$response),sd(plant$response),sd(robots$response),sd(animal$response))
#(animals < humans < plants < robots < objects)

#What can we learn from reaction time?
c <- c(mean(human$logRT),mean(object$logRT),mean(plant$logRT),mean(robots$logRT),mean(animal$logRT))
match(max(c),c) #participants took the longest to rate humans
match(min(c),c) #and the shortest to rate objects
#objects < plants < animals < robots < humans

c <- c(sd(human$zRT),sd(object$logRT),sd(plant$logRT),sd(robots$logRT),sd(animal$logRT))
match(min(c),c) #humans
match(max(c),c) #animals -> this could be because of the animacy hierarchy(insects,reptiles,mamels)
#animals < robots < plants < humans < objects


#check whether the length of the experiment correlated with participants responses (insignificant)
cor.test(ACC2_stats$response, ACC2_stats$eventindex,method="spearman")

#however, length of the experiment correlates with the reaction time 
cor.test(ACC2_stats$logRT, ACC2_stats$eventindex)



#this is also the case for the cleaned data set (where words around the mean and with high sd have been excluded)

#t-test between the chosen words (animate-inanimate, significant!)
ACC_cleaned_words <- read.csv2("~/Semester 6/Bachelor Thesis/acc2/Statistics/ACC_cleaned_words.csv")
View(ACC_cleaned_words)

#since RT is skewed, calculate the z scores
ACC_cleaned_words$RTlog <- log(ACC_cleaned_words$Mittelwert.von.duration)

animate_words <- ACC_cleaned_words[1:12,]
inanimate_words <- ACC_cleaned_words[13:24,]

#Animacy rating (significant)
t.test(animate_words$Mittelwert.von.response,inanimate_words$Mittelwert.von.response)

#Reaction time
t.test(animate_words$zRT,inanimate_words$zRT)


