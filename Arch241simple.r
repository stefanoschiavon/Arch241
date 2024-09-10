intro {
  x<-9 # write notes
  y is a very happy variable
  yy<-sqrt(x)
  log(x)
  x<-c(3,-2,4,2,0,6)
  x
  x[3]
  x[1:3]
  mean(x)
  sum(x)
  max(x)
  z<-x>2
  z
  v1=seq(0,100, by=0.5) #Variables are case sensitive
  V1='Ciao'
  v1;V1
  sd(v1)
  v2<-v1^2
  v3<-v1[v1>5];v3
  plot(v1,v2, type='p')
  help(plot)
  plot(v1,v2, type='l')
  plot(v1,v2, type='b')
  plot(sin(seq(0,8*pi,length=100)),type='l')
  summary(v1)
  plot(v1,v2, type='l',ylab='V2', xlab='V1',main='Arch 241', sub='First plot')
  cor(v1,v2)
  LinearModel<-lm(v2~v1); summary(LinearModel)
       }

load file and summary {
load("~/Dropbox/2020 Arch241/Arch241/Arch241simple.rda");attach(Arch241simple);
save("Arch241simple", file="~/Dropbox/2020 Arch241/Arch241/Arch241simple.rda")
  
Arch241simple [1:30,]
summary(Arch241simple) #stat summary
summary(Arch241simple$Height)
summary(Arch241simple$Age)

names(Arch241simple) #variable names

Arch241simpleMale<-subset(Arch241simple, Sex=="Male");summary(Arch241simpleMale) #subset
Arch241simpleOrg<-subset(Arch241simple, Organization.Name=="CCE");summary(Arch241simpleOrg) #subset

Arch241simpleMale1<-filter(Arch241simple, Sex=="Male");summary(Arch241simpleMale1) #usingtidyverse

Arch241simple$AgeBin30<-ifelse(Arch241simple$Age<=30,c("Less than 30"),c("More than 30"))
Arch241simple$AgeBin30<-as.factor(Arch241simple$AgeBin30)
summary(Arch241simple$AgeBin30)
summary(Arch241simple)

Arch241simple$BMI<-mutate(Arch241simple,BMI = Weight/(Height^2)) #using tidyverse
summary(Arch241simple$BMI)

by_Sex<-group_by(Arch241simple, Sex)
summarise(by_Sex, count = n(), age = mean(Age), weight=mean(Weight), height=mean(Height))


}

histogram density plot {
  library('ggplot2')
  library('splines')
  library('MASS')
  library('GGally')
  library('tidyverse') 
  qplot(Weight,data=Arch241simple, geom="histogram")
  qplot(Weight,data=Arch241simple, geom="histogram",binwidth=5)
  qplot(Weight,data=Arch241simple, geom="histogram",binwidth=10)
  qplot(Weight,data=Arch241simple,facets=Sex~., geom="histogram",binwidth=5)
  qplot(Weight,data=Arch241simple,facets=Organization.Name~., geom="histogram")
  qplot(Weight,data=Arch241simple,facets=AgeBin~., geom="histogram",binwidth=10)
  
  
  #getting the same but a bit more flexibility
  Arch141Hist<-ggplot(data=Arch241simple, aes(x=Weight))
  Arch141Hist+ geom_histogram(binwidth = 5)
  Arch141Hist+ geom_histogram(binwidth = 10,aes(fill = ..count..))
  Arch141Hist+ geom_histogram(binwidth = 5)+ facet_grid(Sex ~ .)
  Arch141Hist+ geom_histogram(binwidth = 5)+ facet_grid(Sex ~ AgeBin)
  
  Arch141Hist<-ggplot(data=Arch241simple, aes(x=Weight,fill=Sex))
  Arch141Hist+ geom_histogram(binwidth = 5,alpha=.5, position="identity")
  Arch141Hist+ geom_histogram(binwidth = 5,alpha=.5, position="stack")
  Arch141Hist+ geom_histogram(binwidth = 5,alpha=.5)
  summary(Arch241simple$Height)
  
  Arch141Hist<-ggplot(data=Arch241simple, aes(x=Weight))
  Arch141Hist+geom_density()
  Arch141Hist+geom_density() + xlim(60, 100)
  Arch141Hist+ geom_histogram(aes(y = ..density..)) + geom_density()
  
  Arch141HistAge<-ggplot(data=Arch241simple, aes(x=Age))
  Arch141HistAge+geom_density()
  
}

boxplot {
  qplot(Sex, Weight, data=Arch241simple, geom="boxplot") #qplot (factor, num, ...)
  qplot(Sex, Weight, data=Arch241simple, geom="boxplot") +ylab("Weight [kg]")
  qplot(Sex, Weight, data=Arch241simple, geom="boxplot") + facet_grid(AgeBin~.,)
  qplot(Sex, Weight, data=Arch241simple, geom="boxplot") + facet_grid(AgeBin~.,)+ theme_bw()
  
  #getting the same but a bit more flexibility
  Arch141Box<-ggplot(data=Arch241simple, aes(factor(Sex),Weight))
  Arch141Box+geom_boxplot()
  Arch141Box+geom_boxplot()+ coord_flip()
  Arch141Box+geom_boxplot(aes(fill=Sex))
  Arch141Box+geom_boxplot(aes(fill=factor(AgeBin)))+ theme_bw()+xlab("Sex")
  
  Arch141BoxH<-ggplot(data=Arch241simple, aes(factor(Sex),Height))
  Arch141BoxH+geom_boxplot(aes(fill=factor(AgeBin)))+ theme_bw()+xlab("Sex")
}

scatterplot {
  qplot(Height, Weight, data=Arch241simple)
  qplot(Height, Weight, data=Arch241simple, size=5)
  
  
  Arch141XY<- ggplot(data=Arch241simple, aes(x=Height, Weight))
  Arch141XY+geom_point()
  Arch141XY+geom_point(shape=1)
  Arch141XY+geom_point(shape=6)
  Arch141XY+geom_point(aes(colour = AgeBin))
  Arch141XY+geom_point(aes(colour = Sex,shape = AgeBin))
  Arch141XY+geom_point(aes(colour = Sex,shape = AgeBin), size=5)
  Arch141XY+geom_point(aes(size=Age))
  Arch141XY+geom_point(size=10)
  Arch141XY+geom_point(size=10,alpha = 1/5)
  
  
  ggpairs(Arch241simple, columns = c(2,3,4)) 
  ggpairs(Arch241simple, columns = c("Age","Sex","Weight")) #same as beforee
  ggpairs(Arch241simple, columns = 2:5)
  ggpairs(Arch241simple, columns = c("Age","Sex","Weight","Height"),aes(color=Sex))
  
  ggscatmat(Arch241simple, columns = 3:5, color = "Sex") #ggscatmat is similar to ggpairs but only works for purely numeric multivariate data
  
  
}

normal distribution {
shapiro.test(Arch241simple$Age) #no normally distributed p<0.05
shapiro.test(Arch241simple$Weight) #normally distributed p >0.07 W=1 and p>0.05 then it is normally distributed. 
shapiro.test(Arch241simple$Height) #almost normally distributed p=0.14
par(mfrow=c(1,3))
qqnorm(Arch241simple$Age, main='Age');qqnorm(Arch241simple$Weight,main='Weight');qqnorm(Arch241simple$Height,main='Height')
qqline(Arch241simple$Age);qqline(Arch241simple$Weight);qqline(Arch241simple$Height)

}

correlation{
cor(Arch241simple[,c("Height","Weight")], use="complete",method="pearson")
cor(Arch241simple[,c("Age","Height","Weight")], use="complete",method="spearman")
round(cor(Arch241simple[,c("Age","Height","Weight")], use="complete",method="spearman"),2)

#library(ellipse)
#corrTab<-cor(Arch241simple[,c("Age","Height","Weight")], use="complete",method="spearman")
#plotcorr(corrTab)
#colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
#plotcorr(corrTab, col=rgb(colorfun((corrTab+1)/2), maxColorValue=255))

ggcorr(Arch241simple[,c("Age","Height","Weight")])
ggcorr(Arch241simple[,c("Age","Height","Weight")], label = TRUE)

}

hypothesis testing {

  qplot(Sex, Weight, data=Arch241simple, geom="boxplot") +ylab("Weight [kg]")
t.test(Weight~Sex, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Arch241simple) # the diiference is statistically significant. confidence interval is strong. 
AnovaModel.1 <- aov(Weight ~ Sex, data=Arch241simple); summary(AnovaModel.1) # same results as before
AnovaModel.5 <- aov(Weight ~ Organization.Name, data=Arch241simple); summary(AnovaModel.5) #multi 
t.test(Weight~Organization.Name, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=Arch241simple) # to do to show the error that the t.test can manage only two levels, not many as Organization.Name

}

example of linear model {
  lmWeightHeight <- lm(Weight ~ Height, data=Arch241simple);summary(lmWeightHeight) # explain estimate, significance, adj R2. p value
  oldpar <- par(oma=c(0,0,0,0), mfrow=c(2,2));plot(lmWeightHeight);par(oldpar);round(coefficients(lmWeightHeight),0) 
  
  
  # predicted vs observed plot
  predictedWeight<-predict(lmWeightHeight)
  plot(predictedWeight,Weight)

  
  
  LinMod<-ggplot(Arch241simple, aes(y=Weight, x=Height))+geom_point(aes(color=Sex, size=4))+ theme_bw()+xlab("Height [m]")+ylab("Weight [kg]")
  LinMod
  LinMod+geom_smooth() # add loess function
  LinMod+geom_smooth(method=lm)
  
  LinMod2<-ggplot(Arch241simple, aes(y=Weight, x=Height, color=Sex))+geom_point(aes(size=4))+ theme_bw()+xlab("Height [m]")+ylab("Weight [kg]")
  LinMod2
  LinMod2+geom_smooth(method=lm)
  
  LinMod+stat_smooth()
  LinMod+stat_smooth(level=0.50)
  LinMod+stat_smooth(method='lm')
  LinMod+stat_smooth(method='lm', formula=y~ns(x,3)) # polynomial.
  LinMod+stat_smooth(method=rlm, formula=y~ns(x,2)) # rlm roboust linear regression
  
  
  

  # non linear model
  v1=seq(1,10, by=0.5); v2<-v1^2
  LinearModel<-lm(v2~v1); summary(LinearModel)
  oldpar <- par(oma=c(0,0,0,0), mfrow=c(1,1))
  plot(v1,v2, type='p')
  abline(lm(v2~v1))
  oldpar <- par(oma=c(0,0,0,0), mfrow=c(2,2));plot(LinearModel);par(oldpar);round(coefficients(LinearModel),0) 

# outlier
x1<-c(20,30,40,50,60,80,100,120,140,160,180)
y1<-c(24,28,34,36,39,80,48,51,59,64,72)
plot(x1,y1)
LinearModel<-lm(y1~x1); summary(LinearModel);oldpar <- par(oma=c(0,0,0,0), mfrow=c(2,2));plot(LinearModel);par(oldpar);round(coefficients(LinearModel),0) 

#  removed the outlier (80, 80) 
x1<-c(20,30,40,50,60,100,120,140,160,180)
y1<-c(24,28,34,36,39,48,51,59,64,72)
plot(x1,y1)
  LinearModel<-lm(y1~x1); summary(LinearModel);oldpar <- par(oma=c(0,0,0,0), mfrow=c(2,2));plot(LinearModel);par(oldpar);round(coefficients(LinearModel),0) 
  
  
}

example of multivariable regression {
LinearModelM <- lm(Weight ~ Height+Sex+Age, data=Arch241simple); summary(LinearModelM)
LinearModelM <- lm(Weight ~ Height+Sex, data=Arch241simple); summary(LinearModelM) # here I lost 0.04 of adj R2. Is this a lot? Depend



}

example of machine learning {
  library(caret) #short for Classification And REgression Training. Caret automatically tune the parameters of complex model using resampling
  set.seed(1) #starting point in the generation of a sequence of random numbers. By fixing it, the random number can be reproduced.
  
  inTrain<-createDataPartition(Arch241simple$Weight, # to create an index for partition of the dataset. It used a stratified random split of the data
                                      p=0.7, # percentage of data in the training dataset
                                      list = FALSE) 
  
  training<-Arch241simple[inTrain,];summary(training) 
  testing<-Arch241simple[-inTrain,];summary(testing)
  remove(inTrain)
  
  p<-ggplot(data=training, aes(x=Weight))+geom_density()+ xlim(40, 140)
  p<-p+geom_density(data=testing, aes(x=Weight),color="red");print(p)
  

  train_control<-trainControl(method = "repeatedcv",
                            number = 3,
                            repeats = 10,
                            search = "grid") # definition of training method  
  
  regressors<-c("Height","Sex","Age") 
  
  # simple linear model
  modellm<-train(x=training[regressors],
                 y=training$Weight, 
                 method="lm",
                 trControl = train_control)
  summary(modellm) #linear model
  
 #  modellmcs<-train(x=training[regressors], #to test in the future if I want to introduce center and scale
                 y=training$Weight, 
                 method="lm",
                 trControl = train_control,
                 preProcess = c("center","scale"))
  # summary(modellmcs) #linear model
  
  
  # random forest
  seeds <- as.vector(c(1:26), mode = "list"); seeds[[26]] <- 1 # RF needs a vector of 26 seeds. All of them can be fixed to an integer. In this case 1
  modelRF<-train(x=training[regressors],
                 y=training$Weight,
                 method="rf", # specify random forest
                 ntree=50,
                 tuneGrid=data.frame(mtry=3), # number of predictors
                 importance=TRUE,
                 na.action=na.omit,
                 trControl=trainControl(method="boot", seeds = seeds),
                 allowParallel=FALSE)
                 )
  print(modelRF$finalModel)
  round(importance(modelRF$finalModel),2)
  plot(modelRF)
  
  
  
  #RMSE on the trainging group
  round(modellm$results[c("RMSE","Rsquared","MAE")],2)
  round(modelRF$results[c("RMSE","Rsquared","MAE")],2)
  
  # RMSE models estimation on the testing dataset 
  predictlm<-predict(modellm,testing[regressors])  
  rmselm<-sqrt(mean((testing$Weight-predictlm)^2));rmselm
  
  predictRF<-predict(modelRF,testing[regressors])
  rmseRF<-sqrt(mean((testing$Weight-predictRF)^2));rmseRF
  

}

probability distribution {
  library(ggplot2)
  x<- runif(10000,min=2,max=5);hist(x) #The basic uniform generator in R is the function runif whose only required entry is the number of values to be generated. The other optional parameters are min and max, which characterize the bounds of the interval supporting the uniform. (The default is min=0 and max=1.)
  
  x<-rnorm(100000,mean=0, sd=1) #normal distribution. produces a N(0,1) sample of size 2500
  hist(x)
  x1<-rnorm(10000,mean = 2, sd=1);hist(x1)# change mean
  x2<-rnorm(10000,mean = 0, sd=1.5);hist(x2) 
  
  db<-data.frame(x,x1,x2)
  den<-ggplot(data=db)
  den+geom_density(aes(x=x),colour="black")+geom_density(aes(x=x1),colour="red")+geom_density(aes(x=x2),colour="green")
  
  
  x <-rgamma(30,2.5, 4.5) # produces 30 data points of gamma distribution with 2.5 shape and 4.5 scale.
  x <- rlnorm(500, 1, 0.6); hist(x) # produces 500 points with mean of 1, and standard deviation of 0.6
  
}

pseudo-random numbers {
  # "One thing that traditional computer systems aren't good at is coin flipping," "They're deterministic, which means that if you ask the same question you'll get the same answer every time. In fact, such machines are specifically and carefully programmed to eliminate randomness in results. They do this by following rules and relying on algorithms when they compute."- Steve Ward#
  # For most applications, a pseudo-random number is sufficient
  # Not all randomness is pseudo."If you go to an online poker site, for example, and you know the algorithm and seed, you can write a program that will predict the cards that are going to be dealt."
  x1<- runif(10)
  x2<- runif(10)
  x1-x2 #How to compare x1, x2?
  
  Use seed deterministic #setting the seed determines all the subsequent values produced by the random generator. Most of cases, do not need. But in settings where we need to reproduce the exact same sequence of random simulations, for example to compare two procedures or two speeds, setting a fixed value of the seed is reasonable.
  set.seed(14);x1<- runif(10) # to generate a random number
  set.seed(14);x2<- runif(10)
  x1-x2
}

random sampling { 
  #Suppose you are doing a thermal comfort study with the subjects randomly selected from a group. For simplicity that I only have five students and they are named Alice, Bob, Charlotte, Dan, Emily, Tom, Jerry,Chris. .
  group <- c("Alice", "Bob", "Charlotte", "Dan", "Emily", "Tom", "Jerry", "Chris")
  sample(x=group,size=5, replace=FALSE) # use function sample to get random sample
  sample(x=group,size=5, replace=TRUE) # if raplace is true, then it can use the original sample, otherwise it remove it.
  sample(x=group,size=20, replace=TRUE)

}

example of application: Draft risk model {
  Nsim <- 10000
  ta <-runif(Nsim,23,26); hist(ta) # uniform distribution
  va <- rlnorm(Nsim, log(15), log(2)); hist(va, prob=TRUE); lines(density(va)) # log distribution for air speed
  va<-va/100
  tu <- runif(Nsim, 0.4,0.6) # uniform distribution for turbulence intensity
  PD <- 3.143*(34-ta)*(va-0.05)^0.6223+0.3696*va*tu*(34-ta)*(va-0.05)^0.6223
  
  
  db<-data.frame(ta,va,tu,PD)
  p1<-ggplot(data=db)+geom_density(aes(x=ta),colour="black")
  p2<-ggplot(data=db)+geom_density(aes(x=va),colour="red")
  p3<-ggplot(data=db)+geom_density(aes(x=tu),colour="green")
  p4<-ggplot(data=db)+geom_density(aes(x=PD),colour="blue")
  multiplot(p1, p2, p3, p4, cols=2)
  
  #what if we increase the temperature
  ta <-runif(Nsim,26,28); hist(ta) # uniform distribution
  PD1 <- 3.143*(34-ta)*(va-0.05)^0.6223+0.3696*va*tu*(34-ta)*(va-0.05)^0.6223
  
  ggplot(data=db)+geom_density(aes(x=PD),colour="green")+geom_density(aes(x=PD1),colour="red")
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

example of application: thermal comfort{
  install.packages("comf")
  library(comf)
  library('ggplot2')
  
  
  pmvppd<-calcPMVPPD(ta=25, tr=25, vel=0.1, rh=50, clo = 0.5, met = 1)
  
  Nsim <- 10
  ta <-runif(Nsim,23,27)
  pmv<-calcPMV(ta=ta, tr=25, vel=0.1, rh=50, clo = 0.5, met = 1)
  pmv<-pmv(ta=ta, tr=25, vel=0.1, rh=50, clo = 0.5, met = 1)
  
  db<-c(ta,pmv)
  hist<-ggplot(data=db, aes(x=pmv))
  hist+geom_density()
  
  
  
} # this is not complete. To be refined in the future

Tydeverse {
  
  install.packages("tidyverse")
  library(tidyverse)
  
  #filter() Pick observations by their values
  Arch241simpleMale1<-filter(Arch241simple, Sex=="Male")
  summary(Arch241simpleMale1)
  #arrange() Reorder the rows - kind of sort
  arrange(Arch241simpleMale1,AgeBin)
  #select() Pick variables by their names
  #mutate() Create new variables with functions of existing variables
  mutate(Arch241simple,BMI = Weight/(Height^2))
  #summarise() Collapse many values down to a single summary
  #group_by() which changes the scope of each function from operating on the entire dataset to operating on it group-by-group
  
}




