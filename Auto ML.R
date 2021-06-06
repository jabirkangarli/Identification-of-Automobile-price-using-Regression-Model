# Installing packages and importing libraries

if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(dplyr,MASS,randomForest,
               RColorBrewer,ggplot2,caret,psych)
# importing data
df <- read.csv("Auto_data.csv")
head(df)

# Replacing ? with NAs
df$bore[df$bore == "?"] <- NA
df$stroke[df$stroke == "?"] <- NA
df$horsepower[df$horsepower == "?"] <- NA
df$peak.rpm[df$peak.rpm == "?"] <- NA
df$price[df$price == "?"] <- NA
df$normalized.losses[df$normalized.losses == "?"] <- NA
df$num.of.doors[df$num.of.doors == "?"]<- NA

# checking NA values  
colSums(is.na(df))

# Data Imputation


#df$x[is.na(df$x)]<-mean(df$x,na.rm=TRUE)
df$num.of.doors[is.na(df$num.of.doors)] <- "four"
# impute normalized.losses
df$normalized.losses<-sapply(df$normalized.losses, function(x) as.numeric(as.character(x)))
df$normalized.losses[is.na(df$normalized.losses)] <- mean(df$normalized.losses,na.rm=TRUE)

# impute bore

df$bore<-sapply(df$bore, function(x) as.numeric(as.character(x)))
df$bore[is.na(df$bore)] <- mean(df$bore,na.rm=TRUE)

# impute stroke

df$stroke<-sapply(df$stroke, function(x) as.numeric(as.character(x)))
df$stroke[is.na(df$stroke)] <- mean(df$stroke,na.rm=TRUE)

# imputing rpm

df$peak.rpm<-sapply(df$peak.rpm, function(x) as.numeric(as.character(x)))
df$peak.rpm[is.na(df$peak.rpm)] <- mean(df$peak.rpm,na.rm=TRUE)

# imputing hp

df$horsepower<-sapply(df$horsepower, function(x) as.numeric(as.character(x)))
df$horsepower[is.na(df$horsepower)] <- mean(df$horsepower,na.rm=TRUE)

# impute price

df$price<-sapply(df$price, function(x) as.numeric(as.character(x)))
df$price[is.na(df$price)] <- mean(df$price,na.rm=TRUE)

colSums(is.na(df)) #checking NA

# EDA all variable

describe(df)

# EDA

# Fuel type vs price
df %>%  
  ggplot(aes(x= fuel.type,y=price,fill=fuel.type)) + 
  geom_boxplot() +  
  xlab("Fuel Type") + 
  ylab("Price")+ 
  ggtitle("Fuel Type vs Price")

# Engine Type vs price

df %>% 
  ggplot(aes(x= engine.type,y=price,fill=engine.type)) +
  geom_boxplot() +  
  xlab("engine.type") + 
  ylab("Price")+ 
  ggtitle("engine.type vs Prime")

# Engine Size and Mean Price
ndf<-df %>% 
  group_by(as.factor(engine.size)) %>% 
  summarise(Mean_Price = mean(price))
colnames(ndf)<- c("engine.size", "Mean_Price" )
ndf %>% 
  ggplot(aes(x=engine.size ,y=Mean_Price,group = 1)) +
  geom_line() +  
  xlab("engine.size ") + 
  ylab("Mean Price")+ 
  ggtitle("Engine Size  vs Mean Price")+
  theme(axis.text.x = element_text(angle = 90))

# Peak RPM vs mean price
ndf<-df %>% 
  group_by(as.factor(round(peak.rpm))) %>%
  summarise(Mean_Price = mean(price))
colnames(ndf)<- c("peak.rpm", "Mean_Price" )
ndf %>%  
  ggplot(aes(x=peak.rpm ,y=Mean_Price,group = 1)) +
  geom_line() +  
  xlab("peak.rpm ") + 
  ylab("Mean Price")+ 
  ggtitle(" Peak RPM  vs Mean Price")+
  theme(axis.text.x = element_text(angle = 90))

# width and price

ndf<-df %>% 
  group_by(as.factor(round(width))) %>%
  summarise(Mean_Price = mean(price))
colnames(ndf)<- c("width", "Mean_Price" )

ndf %>% 
  ggplot(aes(x=width ,y=Mean_Price,group = 1)) + 
  geom_line() +  
  xlab("width") + 
  ylab("Mean Price")+ 
  ggtitle("width  vs Mean Price")+
  theme(axis.text.x = element_text(angle = 90))

# Engine location  vs price

df %>%  
  ggplot(aes(x= engine.location,y=price,fill=engine.location)) + 
  geom_boxplot() + 
  xlab("Engine Location") + 
  ylab("Price")+ 
  ggtitle("Engine Location vs Price")

# Data Preparation

df$normalized.losses<- log(df$normalized.losses)
df$wheel.base <- log(df$wheel.base)
df$length <- log(df$length)
df$width <- log(df$width)
df$height <- log(df$height)
df$curb.weight <- log(df$curb.weight)
df$engine.size <- log(df$engine.size)
df$bore <- log(df$bore)
df$stroke <- log(df$stroke)
df$compression.ratio <- log(df$compression.ratio)
df$horsepower <- log(df$horsepower)
df$peak.rpm <- log(df$peak.rpm)
df$city.mpg <- log(df$city.mpg)
df$highway.mpg <- log(df$highway.mpg)

df<- df%>%mutate_if(is.character, as.factor)

# Dummy variables 
car_dummy <- dummyVars(" ~ .", data = df, fullRank = T) 
df <- data.frame(predict(car_dummy, newdata = df))
# Splitting 
set.seed(123)
Index <- sample(1:nrow(df),round(nrow(df)*.7),replace = FALSE)
training <- df[Index,]
testing <- df[-Index,]

# Linear Model

lm_model <- lm(price~.,data = training)
summary(lm_model)

# Forward and backwards selection model

lm_model_both <- stepAIC(lm_model,direction = "both")

# Random Forest
randomforest_model <- randomForest(price~.,data = training,mtry = 3,ntree = 100)

model_predict_lm <- predict.lm(lm_model,newdata =testing)
model_predict_both<-predict.lm(lm_model_both,newdata =testing)
model_predict_randomForest <- predict(randomforest_model,newdata =testing)

# Model Evaluvaion 
paste0("Linear Regression's RMSE",": ",RMSE(model_predict_lm,testing$price)) # Linear Regression
paste0("Forward and Backwards selection model's RMSE:"," ",RMSE(model_predict_both,testing$price)) # Linear Regression with forward and backward selection model
paste0("RandomForest's RMSE :"," ",RMSE(model_predict_randomForest,testing$price)) # Random Forest












# 