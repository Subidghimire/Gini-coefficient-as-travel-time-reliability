set.seed(12345)
meanlog <- runif(10000, min=1, max=2)
meanlog
set.seed(12345)
varlog <- runif(10000, min=0, max=1.5)
varlog
set.seed(12345)
x <- seq(1,10000,1)
x
means <- rep(0,10000)
std <- rep(0,10000)
coefv <- rep(0,10000)
perc_95 <- rep(0,10000)
giniall <- rep(0,10000)
Buffer <- rep(0,10000)
skew <- rep(0,10000)
widthm <- rep(0,10000)
corstgini <- rep(0,10000)
corginibuffer <- rep(0,10000)
corginiwidth <- rep(0,10000)
for (i in 1:10000)
{
  meanlog.smpl <- sample(meanlog,1,replace=T)
  meanlog.smpl
  varlog.smpl <- sample(varlog,1,replace = T)
  varlog.smpl
  mean <- exp(meanlog.smpl+varlog.smpl/2)
  mean
  std.dev <- sqrt((exp(varlog.smpl)-1)*exp(2*meanlog.smpl+varlog.smpl))
  std.dev
  cof.var <- sqrt(exp(varlog.smpl)-1)
  cof.var
  per_95 <- qlnorm(0.95,meanlog.smpl,sqrt(varlog.smpl), lower.tail = TRUE)
  per_95
  per_90 <- qlnorm(0.90,meanlog.smpl,sqrt(varlog.smpl), lower.tail = TRUE)
  per_50 <- qlnorm(0.5, meanlog.smpl, sqrt(varlog.smpl), lower.tail = TRUE)
  per_50
  per_10 <- qlnorm(0.1, meanlog.smpl, sqrt(varlog.smpl), lower.tail = TRUE)
  BTI <- ((per_95-per_50)/(per_50))
  sk <- ((per_90-per_10)/(per_50 - per_10))
  width <- ((per_90-per_10)/per_10)
  gini <- 2*pnorm((sqrt(varlog.smpl))/sqrt(2)) -1 
  #integrand <- function(x) {exp(-x*x)}
  #gini <- integrate(integrand, lower = 0, upper = std.dev/2)
  #gini$value
  #gini<- gini$value * 2/sqrt(3.1415)
  #gini
  std[i] <- std.dev
  coefv[i] <- cof.var
  Buffer[i] <- BTI
  skew[i] <- sk
  widthm[i] <- width
  giniall[i] <- gini
  corstgini<- cor(std.dev,gini)
  corginibuffer <- cor(gini,BTI)
  corginiwidth <- cor(gini, width)
}
giniall
Buffer
df <- data.frame(giniall, Buffer, widthm, skew, std, coefv, corstgini,corginibuffer,corginiwidth)
df
cor(df$giniall, df$std)
cor(df$giniall,df$coefv)
cor(df$giniall,df$Buffer)
cor(df$giniall,df$widthm)
cor(df$giniall,df$skew)
library("ggplot2")
ggscatter(df, x = "std", y = "giniall", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Buffer", ylab = "GINI Coefficient")
cor(giniall, std)
std
boxplot(df$corstgini)
boxplot(std)

df2<-df[!(df$giniall>=1),]
df2
length(df2$std)
df2$coefv
ggplot(df2, x = "coefv", y = "giniall", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "coeff of variation", ylab = "GINI Coefficient")
ggplot(df2, aes(x=coefv, y=giniall)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(df2, aes(x=Buffer, y=giniall)) + 
  geom_point()+
  geom_smooth(method=lm, se=TRUE)
ggplot(df2, aes(x=widthm, y=giniall)) + 
  geom_point()+
  geom_smooth(method=lm)
df2$widthm
cor(df2$Buffer, df2$giniall)
cor(df2$std, df2$giniall)
cor(df2$coefv,df2$giniall)
cor(df2$widthm,df2$giniall)
cor(df2$skew,df2$giniall)
x<-rlnorm(10000, meanlog = 0.5, sdlog = 0.8)
plot(x)
help(erf)


curve(dlnorm(x, 0, 0.25), from=0, to=5,
      main = 'Log-Normal Distributions',
      ylab = ' f(x)',
      lwd = 3,
      col = 'pink')
curve(dlnorm(x, 1, 1.5), from=0, to=5, lwd=3, col='green', add=TRUE)
curve(dlnorm(x, 0, 0.9), from=0, to=5, col='blue', add=TRUE)
curve(dlnorm(x, -2, 4), from=0, to=5, col='red', add=TRUE)
legend(3, 1.5, .6, .3, legend=c("Mean = 0, std.dev =0.25", "Mean=1, std.dev=1.5","Mean=0, std.dev=0.9", "Mean=-2, std.dev=4" ),
       col=c("pink", "green", "blue", "red"), lty=1, cex=0.8)

