# By: Elisabeth Meyers 01/17
#options(java.parameters = "-Xmx1024m")

library(ggplot2)
library(tidyr)
# load in the file setting empty values to NA
can.data = read.csv('001_2015GNRDataset_Child-Adult-Nutrition.csv', header=TRUE, na.strings=c("", NA))

# remove blank rows (trimmed down a lot)
can.data = can.data[rowSums(is.na(can.data))!= ncol(can.data),]
anemia.na = sum(is.na(can.data$WRAanaemia_NUMBER))
lbw.na = sum(is.na(can.data$LBW))

View(can.data)

Anaemia.rate = as.data.frame(can.data$WRAanaemia_RATE)
LBW = as.data.frame(can.data$LBW)

# Calculate the average and replace missing values
LBW.mean = colMeans(LBW, na.rm = TRUE)
LBW$`can.data$LBW`[is.na(LBW$`can.data$LBW`)] = LBW.mean

# Perform the same task for Anaemia rate
Anaemia.mean = colMeans(Anaemia.rate,na.rm=TRUE)
Anaemia.rate$`can.data$WRAanaemia_RATE`[is.na(Anaemia.rate$`can.data$WRAanaemia_RATE`)] = Anaemia.mean 

# combine into one data frame for side by side box plotting
anaemia.lbw.data = as.data.frame(c(Anaemia.rate,LBW))
colnames(anaemia.lbw.data)[colnames(anaemia.lbw.data)=="can.data.LBW"] = "Birth weight"
colnames(anaemia.lbw.data)[colnames(anaemia.lbw.data)=="can.data.WRAanaemia_RATE"] = "Anaemic women of reproductive age"
# this combines the values as one category, but keeps them as seperate variables
td = tidyr::gather(anaemia.lbw.data,key=can.data.WRAnaemia_RATE)
# this way, we can do a side by side box plot using the same scale, in this case 
# we can do this because the scale is percentage out of 100%. Both variables follow
# the same type of measure
names(td)=c("anaemia","lbw")

cat("The correlation coefficient is")
cor(anaemia.lbw.data$`Anaemic women of reproductive age`,anaemia.lbw.data$`Birth weight`)
cat("which suggests a strong linear relationship")

# perform a side-by-side box plot for visualizing the data
ggplot(data=td,aes(x=anaemia,y=lbw))+ylab("% percent of cases")+xlab("")+geom_boxplot(outlier.color=c("dodgerblue1"))

# perform a linear regression with anaemic mothers as input
fit =lm(LBW ~ WRAanaemia_RATE, data=can.data)
summary(fit)
# perform a scatter plot with linear regression line

ggplot(anaemia.lbw.data, aes(x=`Anaemic women of reproductive age`, y=`Birth weight`)) + geom_point(colour="dodgerblue1")+ geom_abline(intercept = 3.12618,slope = 0.25742)

ci.i = confint(fit, 'WRAanaemia_RATE', level=0.95)

test.mod = data.frame(Fitted = fitted(fit),
                       Residuals = resid(fit), Anaemia_Rate = fit$model$WRAanaemia_RATE)
ggplot(test.mod, aes(Fitted,Residuals,colour=Anaemia_Rate)) + geom_point() + stat_smooth(method = "lm", col = "red", linetype="dotted")

# make a prediction, given a country has a certain percentage of anaemic mothers,
# can we predict the percentage of babies born with low birth weight?
newCountry = NULL
newCountry$WRAanaemia_RATE = 30
cat("the predicted low birth weight percentage for the new country based on percentage of anaemic women is",lbw.prediction = predict(fit,newCountry))

predict(fit,data.frame(WRAanaemia_RATE = newCountry), level = 0.95, interval = "confidence")


# lets predict for the orignal null values
predict.bw = as.data.frame(can.data$WRAanaemia_RATE)
predict.bw[,2] = as.data.frame(can.data$LBW)

for(i in 1:length(predict.bw$`can.data$WRAanaemia_RATE`)) {
  if(is.na(predict.bw[i,2])){
    cat("true ")
    #predict.bw[i,2]
    cat(predict(fit,predict.bw[i,1]))
  }
}
