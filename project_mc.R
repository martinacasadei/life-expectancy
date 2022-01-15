#Japan
library(demography)
library(devtools)
#Downloading of data from HMD
japan<- hmd.mx("JPN", "martina.casadei11@studio.unibo.it", "***", "Japan")
japan



#Plot of log death rates (1947-2019)
plot(japan, series=names(japan$rate)[1], year=1947:2019, main="Japan-Female, 1947-2019")
plot(japan, series=names(japan$rate)[2], year=1947:2019, main="Japan-Male, 1947-2019")
plot(japan, series=names(japan$rate)[3], year=1947:2019, main="Total (log) death rate-Japan 1947-2019")
legend("bottomright", inset = c(0,0), cex=(0.7), bty="n", legend=c("from","1947", "2019"),col=c("white","red","magenta3"), lty=1:1)
#Omitted in the project


#Death rates from lifetable
#1) 2019
japan_2019<- lifetable(japan, series = names(japan$rate)[3], years = 2019)
japan_2019
dx_2019 <-japan_2019$dx
#2) 1947
japan_1947<- lifetable(japan, series = names(japan$rate)[3], years = 1947)
dx_1947 <-japan_1947$dx
plot(dx_1947, type="l", col="darkolivegreen3", lwd=2, lty=1, main = "Deaths dx", sub="Japan 1947 vs 2019", xlab="age", ylab="dx")
#together
lines(dx_2019, col="slateblue3", lwd=2)
par(xpd=TRUE)
legend("bottomright", inset = c(0,-0.3), cex=(0.7), bty="n", legend=c("dx_1947", "dx_2019"),col=c("darkolivegreen3","slateblue3"), lty=1:1)



#Making age pyramids
library(pyramid)
#1)
#pyramid of year 1947 (first year in the dataset)
females<-japan$pop$female[,1]
males<-japan$pop$male[,1]
ages<-japan$age
data<-data.frame(males,females,ages)

pyramid(data,Llab="Males",Rlab="Females",Clab="",Laxis=seq(0,700000,len=5),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
        main="Age pyramid-Japan 1947")
#2)
#pyramid of year 2019 (last year in the dataset)
females<-japan$pop$female[,73]
males<-japan$pop$male[,73]
ages<-japan$age
data<-data.frame(males,females,ages)

pyramid(data,Llab="Males",Rlab="Females",Clab="",Laxis=seq(0,700000,len=5),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
        main="Age pyramid-Japan 2019")




#Plot life expectancy at different ages
e_0<-life.expectancy(japan, series = names(japan$rate)[3],  years = japan$year, type = c("period"))
plot(e_0, main="Life expectancy at birth", sub="Japan 1947-2019", col="deepskyblue2", xlab="years", lwd=2)
e_60 <- life.expectancy(japan, series = names(japan$rate)[3],  years = japan$year, type = c("period"), age=60)
e_60
e_80 <- life.expectancy(japan, series = names(japan$rate)[3],  years = japan$year, type = c("period"), age=80)
e_80
#Comparing life expectancy at 60 and 80 during time
plot(e_60, ylim=c(0,30), col="navyblue", xlab="years", ylab="expectancy", main = "Evolution of e60 and e80 during time", sub="Japan 1947-2019", lwd=2)
lines(e_80, col="palegreen3", lty=2, lwd=2)
par(xpd=TRUE)
legend("bottomright", inset = c(0,-0.30), cex=(0.7), bty="n", legend=c("e60", "e80"),col=c("navyblue","palegreen3"), lty=1:2, lwd=2)

#Sex differences in life expectancy
e0_females<-life.expectancy(japan, series = names(japan$rate)[1],  years = japan$year, type = c("period"))
e0_males<-life.expectancy(japan, series = names(japan$rate)[2],  years = japan$year, type = c("period"))
e0_males
e0_females
plot(e0_females, col="blueviolet", ylim=c(45,90), lwd=2, ylab="e0", xlab="years", main="Sex difference in life expectancy at birth", sub="Japan 1947-2019")
lines(e0_males, col="cyan3", lwd=2)
par(xpd=TRUE)
legend("bottomright", inset = c(-0.01,0), cex=(0.7), bty="n", legend=c("e0 for females", "e0 for males"), col=c("blueviolet","cyan3"), lty=1:1)
#Does sex difference in life expectancy change when reaching a specific age?
#1) at 60
e60_females <- life.expectancy(japan, series = names(japan$rate)[1],  years = japan$year, type = c("period"), age=60)
e60_males <- life.expectancy(japan, series = names(japan$rate)[2],  years = japan$year, type = c("period"), age=60)
e60_females
e60_males
plot(e60_females, col="blueviolet", ylim=c(12,30), lwd=2, ylab="e60", xlab="years", main="Sex difference in life expectancy at age 60", sub="Japan 1947-2019")
lines(e60_males, col="cyan3", lwd=2)
par(xpd=TRUE)
legend("bottomright", inset = c(0,0), cex=(0.7), bty="n", legend=c("e60 for females", "e60 for males"), col=c("blueviolet","cyan3"), lty=1:1)
#2) at 80
e80_females <- life.expectancy(japan, series = names(japan$rate)[1],  years = japan$year, type = c("period"), age=80)
e80_males <- life.expectancy(japan, series = names(japan$rate)[2],  years = japan$year, type = c("period"), age=80)
plot(e80_females, col="blueviolet", ylim=c(3,13), lwd=2, ylab="e80", xlab="years", main="Sex difference in life expectancy at age 80-Japan")
lines(e80_males, col="cyan3", lwd=2)
par(xpd=TRUE)
legend("bottomright", inset = c(-0.1,0), cex=(0.7), bty="n", legend=c("e80 for females", "e80 for males"), col=c("blueviolet","cyan3"), lty=1:1)
#graphs show that sex difference in life expectancy exists since the '50, but it seems to be more relevant around the last decades.
#in particular for what concern life expectancy at 60 and 80.




#Estimating Lee-Carter model
#1 step: model estimation for japan (3=total rate)
japan_LC<-lca(japan, series=names(japan$rate)[3], years=1980:2010)
plot(japan_LC) #it plots main effects ax, interaction bx and kt

#2 step: residuals analysis
RES=residuals(japan_LC, "residuals")
plot(rep(RES$y,length(RES$x)),(RES$z), xlab = "age", ylab = "residuals", main = "Residuals by ages from LC model")
plot(rep(RES$x,length(RES$y)),(RES$z), xlab = "years", ylab = "residuals",  main = "Residuals by years from LC model")

#3 step: forecasting life expectancy
e0_2010_2060<- life.expectancy(forecast(japan_LC),
                               type = c("period"))
#Joining past and forecasted life expectancy
plot(e0_2010_2060, main = "Forecasted Life Expectancy at birth-Japan", col="purple3", lwd=2, xlab="years", ylab="e0 2010-2060")
lines(e_0, col="palegreen3", lwd=2)
par(xpd=TRUE)
legend("bottomright", inset = c(-0.1,-0.35), cex=(0.7), bty="n", legend=c("forecasted e0", "past e0"), col=c("purple3","palegreen3"), lty=1:1)
#Selecting the period 2010-2020 to see if previsions matched actual e0
e0_2011_2019<- life.expectancy(forecast(japan_LC),
                               type = c("period"), years=2011:2019)
e0_bis<-life.expectancy(japan, series = names(japan$rate)[3],  years = 2011:2019, type = c("period"))

plot(e0_2011_2019, main = "Forecasted Life Expectancy at birth-Japan", col="purple3", lwd=2, xlab="years", ylab="e0 2011-2030")
lines(e0_bis, col="palegreen3", lwd=2)

#forecasting with c.i.
japan_LC_2<-lca(japan,adjust="e0",years=1980:2010)
f_japan<-forecast(japan_LC)
e0_ci<-e0(f_japan,PI=TRUE,nsim=200)
e0_ci
plot(e0_ci, col="deepskyblue2", lwd=2, main="C.I. for forecasted e0 from 2010 to 2060", sub="Japan 1980-2060", xlab="years", ylab="ages")





#Decomposition of life expectancy (PART 1)
#0 step:
#construction of life table
japan_lt_2000<- lifetable(japan, series = names(japan$rate)[3], years = 2000, ages = japan$age, max.age = min(110, max(japan$age)), type =
                            c("period"))
japan_lt_2019<-lifetable(japan, series = names(japan$rate)[3], years = 2019, ages = japan$age, max.age = min(110, max(japan$age)), type =
                           c("period"))
print(japan_lt_2000)
print(japan_lt_2019)

#1 step:
#decomoposing into direct and indirect & interaction effects
DE<-japan_lt_2000$lx/japan_lt_2000$lx[1]*(japan_lt_2019$Lx/japan_lt_2019$lx-japan_lt_2000$Lx/japan_lt_2000$lx)
IE<-japan_lt_2019$Tx[-1]/japan_lt_2000$lx[1]*((japan_lt_2000$lx[-length(japan_lt_2000$age)]/japan_lt_2019$lx[-length(japan_lt_2000$age)])-(japan_lt_2000$lx[-1]/japan_lt_2019$lx[-1]))

#2 step:
# adding a zero for the indirect effect at the last age
IE<-c(IE,0)

#3 step:
#adding the contributions for each age
C<-DE+IE

#4 step:
#results at each age
df <-data.frame(Age=japan_lt_2000$age,C=C)
head(df)
tail(df)
#results at age-classes
library(AMR)
age_grouped<-age_groups(japan_lt_2000$age, split_at = c(5, 10, 15, 20,25,30,35,40,45,50,55,60,65,70,75,80,85,90), na.rm = FALSE)
df_plot<-data.frame(Age=age_grouped,C=C)

#5 step:
#plotting the age-specific contributions to life expectancy change
library(ggplot2)
ggplot(data=df_plot, aes(x=Age, y=C)) +
  geom_bar(stat="identity")+
  xlab("age-class")+
  ylab("C")+
  geom_bar(stat="identity", color="lightsteelblue1", fill="lightsteelblue1", position = )+
  labs(title = "Age-specific contributions to life expectancy change", cex=2,
       subtitle = "Japan 2000-2019", cex =2, caption ="C=contributions for each age-class", cex=2)+
  theme_minimal()



#Decomposition of life expectancy (PART 2)
#0 step:
#construction of life table
japan_lt_1947<- lifetable(japan, series = names(japan$rate)[3], years = 1947, ages = japan$age, max.age = min(110, max(japan$age)), type =
                            c("period"))
japan_lt_2019<-lifetable(japan, series = names(japan$rate)[3], years = 2019, ages = japan$age, max.age = min(110, max(japan$age)), type =
                           c("period"))
print(japan_lt_1947)
print(japan_lt_2019)
#1 step:
#decomposing into direct and indirect & interaction effects
DE<-japan_lt_1947$lx/japan_lt_1947$lx[1]*(japan_lt_2019$Lx/japan_lt_2019$lx-japan_lt_1947$Lx/japan_lt_1947$lx)
IE<-japan_lt_2019$Tx[-1]/japan_lt_1947$lx[1]*((japan_lt_1947$lx[-length(japan_lt_1947$age)]/japan_lt_2019$lx[-length(japan_lt_1947$age)])-(japan_lt_1947$lx[-1]/japan_lt_2019$lx[-1]))
#2 step:
# adding a zero for the indirect effect at the last age
IE<-c(IE,0)
#3 step:
#adding the contributions for each age
C<-DE+IE
#4 step:
#results at each age
df <-data.frame(Age=japan_lt_2000$age,C=C)
head(df)
tail(df)
df
#results at age-classes
library(AMR)
age_grouped<-age_groups(japan_lt_2000$age, split_at = c(5, 10, 15, 20,25,30,35,40,45,50,55,60,65,70,75,80,85,90), na.rm = FALSE)
df_plot<-data.frame(Age=age_grouped,C=C)
#5 step:
#plotting the age-specific contributions to life expectancy change
library(ggplot2)
ggplot(data=df_plot, aes(x=Age, y=C)) +
  geom_bar(stat="identity")+
  xlab("age-class")+
  ylab("C")+
  geom_bar(stat="identity", color="mediumpurple1", fill="mediumpurple1", position = )+
  labs(title = "Age-specific contributions to life expectancy change", cex=2,
       subtitle = "Japan 1947-2019", cex =2, caption ="C=contributions for each age-class", cex=2)+
  theme_minimal()



