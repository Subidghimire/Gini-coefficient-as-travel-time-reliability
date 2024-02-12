set.seed(12345)
library(stats)
k <- runif(10000, min=0.5, max=10)
k
set.seed(12345)
lambda <- runif(10000, min=0.5, max=10)
lambda
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
  k <- sample(k,replace=T)
  k
  lambda <- sample(lambda, replace = T)
  lambda
  mean <- lambda*gamma(1+(1/k))
  mean
  var <- (lambda^2)* (gamma(1+(2/k))-(gamma(1+1/k))^2)
  std.dev <- sqrt(var)
  std.dev
  cof.var <- mean/std.dev
  cof.var
  per_95 <- qweibull(0.95,k,lambda, lower.tail = TRUE)
  per_95
  per_90 <- qweibull(0.90,k,lambda, lower.tail = TRUE)
  per_50 <- qweibull(0.5, k, lambda, lower.tail = TRUE)
  per_50
  per_10 <- qweibull(0.1, k, lambda, lower.tail = TRUE)
  BTI <- ((per_95-per_50)/(per_50))
  sk <- ((per_90-per_10)/(per_50 - per_10))
  width <- ((per_90-per_10)/per_10)
  gini <- 1 - (2^(-1/k))
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
df <- data.frame(giniall, Buffer, widthm, skew, std, coefv)
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
###plotgraphs
curve(dweibull(x, shape=2, scale = 2), from=0, to=5,
      main = 'Weibull Distributions',
      ylab = ' f(x)',
      lwd = 2,
      col = 'pink')
curve(dweibull(x, shape=1.5, scale = 2), from=0, to=5, col='green', add=TRUE)
curve(dweibull(x, shape=2.5, scale = 2.5), from=0, to=5, col='blue', add=TRUE)
curve(dweibull(x, shape=1.5, scale = 3), from=0, to=5, col='red', add=TRUE)
legend(3.1, .4, .4, .3, legend=c("shape=2, scale=2", "shape=1.5, scale=2","shape=2.5, scale=2.5", "shape=1.5, scale=3" ),
       col=c("pink", "green", "blue", "red"), lty=1, cex=0.8)
