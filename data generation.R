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

intercept=1.1
effect=-1.2

var=rep(c(0,1),times=100)

fungus_gnats=rpois(length(var),lambda=exp(intercept+effect*var))


treatment=as.factor(rep(c("Pretreatment","Treated"),times=100))

plot(fungus_gnats~treatment)

fake_gnats=glm(fungus_gnats~treatment,family="poisson")

coef(fake_gnats)

#choose 2 out of 3 of the distributions so far, find your own dataset
#and analyze

#plot with curve

#himmicanes

hurri<-read.csv("Hurricane Dataset.csv")
plot(hurri$alldeaths~hurri$Gender_MF)

summary(glm(alldeaths~ZMasFem,data=hurri),family="poisson")


