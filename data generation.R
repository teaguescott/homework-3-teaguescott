#data generation

intercept=30
effect=-10

diamond=data.frame(price=diamonds$price,cut=diamonds$cut,carat=diamonds$carat)

write.csv(diamond,file="diamond.csv",row.names=F)

var=rep(c(1,0),times=100)

y=rnorm(length(var),mean=intercept+effect*var,sd=10)

plot(y~as.factor(var))

var_name=as.factor(rep(c("bubbles","fromdidy"),times=100))
#
contraception<-read.table("cuse.dat.txt",head=T)

contraception$Total<-contraception$notUsing+contraception$using
write.csv(contraception,file="contraception.csv",row.names=F)
#

summary(glm(cbind(contraception$using,contraception$notUsing)~contraception$education,family="binomial"))

#rpois

intercept=0.5
effect=1.2

var=rep(c(1,0),times=100)

x_variable=#interaction?
  
y=pois(length(var),prob=plogis(intercept+effect*var),size=1)

#choose 2 out of 3 of the distributions so far, find your own dataset
#and analyze

#plot with curve

#himmicanes

hurri<-read.csv("Hurricane Dataset.csv")
plot(hurri$alldeaths~hurri$Gender_MF)

summary(glm(alldeaths~ZMasFem,data=hurri),family="poisson")


