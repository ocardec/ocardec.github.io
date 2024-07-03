#============================================================#
# Classification of a Cardiotocography Data Set              #
# Using the Conditional Inference Tree                       #
#============================================================#
# install.packages("rmarkdown")
# library(rmarkdown)

# Pre-requisites 
#1. Required packages
library("tidyverse")
library("party")
library("partykit")
# library("graphics")
#install.packages("devtools")
# library(devtools)
# library("hrbrthemes")
# library("caret")

#2. Load csv and assess
#setwd("../Documents/GitHub/portfolio/conditional_inference_tree_CTREE/cardiotocography.csv")
ctg<-read.csv(file="cardiotocography.csv", head=TRUE, sep=",", as.is=FALSE)
str(ctg)  # Content
summary(ctg)  # Stats

head(ctg, 10)
summary(ctg$LB)
summary(ctg$NSP)

#4. Pre-processing
# Any missing values?
colSums(is.na(ctg))

# LB distribution
hist((ctg$LB), main="Histogram of FHR Baseline",
     xlab="(beats per minute)", border="darkblue",col ="darkgrey", labels = T)

# LB stats
t.test(ctg$LB)
m<-mean(ctg$LB)
std<-sd(ctg$LB)
upr=m+std
lwr=m-std
lbdf <- data.frame(ctg,my_x = 0 + rnorm(length(ctg$LB),
        mean=m, sd=std),my_y = 0 + rnorm(length(ctg$LB), mean=m, sd=std))

# LB Variation
print(pltlb <- ggplot(lbdf, xlab=FALSE, aes(x=(my_x), y=my_y)) + 
        geom_line(col="grey51",linemitre=1) +
        geom_smooth(method=lm , color="blue", lty=3, fill="light blue", se=T) +
        labs(x=NULL, y="BPM", title="FHR LB Variation\nIn Relation To The Mean")+
        theme_ipsum())

# What is 2-standard deviations from the mean?
upr2=m+(std*2)
lwr2=m-(std*2)

# Plot LB distribution boundaries 
plot.new()
plot(ctg$LB, type="l", col="grey51", ylab="LB", main="1 & 2 Standard Deviations")
abline(h = m, col = "blue")
abline(h = upr, col = "orange", lty=2)
abline(h = lwr, col = "orange", lty=2)
abline(h = upr2, col = "red", lty=2)
abline(h = lwr2, col = "red", lty=2)
text(-65,134, "mean:133.30", col = "blue", adj = c(0, -.1))
text(-65,upr, round(upr, 2), col = "black", adj = c(0, -.1))
text(-65,lwr, round(lwr, 2), col = "black", adj = c(0, -.1))
text(-65,upr2, round(upr2, 2), col = "black", adj = c(0, -.1))
text(-65,lwr2, round(lwr2, 2), col = "black", adj = c(0, -.1))

# LB Observations higher than 2-s.d.
 lba<-(sum(ctg$LB >152.99)) #39
# LB Observations lower than 2-s.d.
 lbb<-(sum(ctg$LB <113.62)) #44
lba+lbb #=83 obs outside of 2-s.d.
sum(between(ctg$LB, 113.62, 152.99))/nrow(ctg) # of obs within 2-s.d.

# b. Exclude non-original measurements, rename targeted values
ctg[12:22] <- NULL
ctg$NSP<-as.numeric(ctg$NSP)
ctg$NSP<-factor(ctg$NSP, levels= 1:3, labels = c("Normal","Suspect", "Pathologic"))
# ctg$CLASS<-as.numeric(ctg$CLASS)
# ctg$CLASS<-factor(ctg$CLASS, levels=1:10, labels= c('A', 'B', 'C', 'D', 'SH', 'AD', 'DE', 'LD', 'FS', 'SP'))


# Visualization of original NSP
plot(ctg$NSP, main="Original NSP Distribution",
     xlab="Fetal State Classification", 
     ylab="Frequency", col=c(3, 7, 2))
text(ctg$NSP, labels=as.character(tabulate(ctg$NSP)), adj=3, pos=3)

# tabulate NSP values and proportions
tabulate(ctg$NSP)
round(prop1<-(proportions(tabulate(ctg[,12])))*100,2)

summary(ctg)

# Distributions preview
ctg[,1:12] %>% 
  gather() %>%                             
  ggplot(aes(value)) +
  theme_light() + labs( title="FHR Measurement Distributions")+
  theme(axis.text.x = element_text(angle=90)) +                 
  facet_wrap(~ key, scales = "free", shrink = TRUE) +  
  geom_bar(mapping = aes(value), 
           color="darkblue", fill="lightgrey")

# Final data subset to be modeled
summary(ctg)

#5. Split the data into a training and test set
set.seed(1234)
ind <- sample(2, nrow(ctg), replace = T, prob = c(0.70, 0.30))
train.data <- ctg[ind == 1, ]
test.data <- ctg[ind == 2, ]

#6. Run the method on a training data
myFormula<-NSP~.
model <- ctree(myFormula, data = train.data)

#7. output the tree structure
print(model)

#8. visualize the tree
plot(model, main="Cardiotocography Data\n Conditional Inference Tree\n'Extended'",
      type="simple",ep_args = list(justmin = 8), drop_terminal = F, 
     gp = gpar(fontsize = 9), margins = c(4,4, 4, 4))

plot(model, type="extended", ep_args = list(justmin =8), drop_terminal=F, tnex=1.5, 
     gp=gpar(fontsize = 8, col="dark blue"),
     inner_panel = node_inner(model, fill=c("light grey","cyan"), pval=T), 
     terminal_panel=node_barplot(model, fill=c(3,7,2), beside=T, ymax=1, rot = 75, 
     just = c(.95,.5), ylines=T, widths = 2, gap=0.05, reverse=F, id=T), 
     margins = c(5,3, 4, 3),
     main ="Cardiotocography Data\n Conditional Inference Tree\n'Extended'")

#9. Confusion matrix
table(predict(model), train.data$NSP, dnn=c("PREDICTED", "ACTUAL"))
# predicted classification accuracy with training data
sum(predict(model) == train.data$NSP)/length(train.data$NSP)
prop.table(table(predict(model), train.data$NSP, dnn=c("PREDICTED", "ACTUAL")))

#10. Evaluate the model on a test data
model2 <- ctree(myFormula, data = test.data)
print(model2)

(model2[5])

plot(model2, main="Cardiotocography Data\n Simple Conditional Inference Tree\nby ocardec",
     type="simple",ep_args = list(justmin = 10), drop_terminal = F, gp = gpar(fontsize = 12))

plot(model2, ep_args = list(justmin = 10), type="extended", drop_terminal = T, 
     tnex=2, gp= gpar(fontsize = 8, col="dark blue"), 
     inner_panel = node_inner (model2, fill=c("lightgrey","yellow"), pval=TRUE, id=TRUE),
     terminal_panel=node_barplot(model2, col="black", fill=c(3,7,2, 0.3), beside=TRUE, 
     ymax=1, rot = 90, just = c("right", "top"), ylines=T, 
     widths=1, gap=0.1, reverse=FALSE, id=TRUE), margins = c(5, 3, 4, 2), 
     main="Cardiotocography Data\n Extended Conditional Inference Tree\nby ocardec")

# Confusion matrix and stats
testPred2 <- predict(model2, newdata = test.data, method="NSP")
confusionMatrix(testPred2, test.data$NSP)

#======================================================
# Workspace Clean up
rm(list=ls())
