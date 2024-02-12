install.packages("extremefit")
library(extremefit)
set.seed(12345)
q <- runif(10000, 1, 5)
q
set.seed(12345)
a <- runif(10000, 2.5, 12)
a
set.seed(12345)
s<- runif(10000, min=2, max=5)
s
set.seed(12345)
x <- seq(1,10000,1)
x
means <- rep(0,10000)
std <- rep(0,10000)
coefv <- rep(0,10000)
perc_95 <- rep(0,10000)
giniall <- rep(0,10000)
Buffer <- rep(0,10000)
widthm <- rep(0,10000)
corstgini <- rep(0,10000)
corginibuffer <- rep(0,10000)
corginiwidth <- rep(0,10000)
skew <- rep(0,10000)
ku <- rep(0,10000)
cu <- rep(0,10000)
for (i in 1:10000)
{
  k <- sample(q, 1, replace=TRUE)
  k
  c <- sample(a, 1, replace = TRUE)
  c
  b <- sample(s, 1, replace = TRUE)
  b
  mean <- (gamma(k-(1/c))*gamma(1+(1/c)))/(gamma(k))
  mean
  std.dev <- sqrt(((gamma(k-(2/c)))*(gamma(1+(2/c)))*gamma(k))-((gamma(k-(1/c)))*
                    (gamma(k-(1/c)))*gamma(1+(1/c))*gamma(1+(1/c)))/(gamma(k)*gamma(k)))
  std.dev
  cof.var <- std.dev/mean
  cof.var
  per_95 <- qburr(0.95,c,k)
  per_95
  per_90 <- qburr(0.90,c,k)
  per_50 <- qburr(0.5,c,k)
  per_50
  per_10 <- qburr(0.1,c,k)
  BTI <- ((per_95-per_50)/(per_50))
  width <- ((per_90-per_10)/per_10)
  sk <- ((per_90-per_10)/(per_50 - per_10))
  #integrand <- function(x) {exp(-x*x)}
  #gini <- integrate(integrand, lower = 0, upper = std.dev/2)
  #gini$value
  gini<- 1-((gamma(k)*gamma(2*k-(1/c)))/(gamma(k-(1/c))*gamma(2*k)))
  gini
  #2*(pnorm(2.809073/sqrt(2),0,1))-1
  std[i] <- std.dev
  coefv[i] <- cof.var
  Buffer[i] <- BTI
  widthm[i] <- width
  giniall[i] <- gini
  ku[i] <- k
  cu[i] <- c
  corstgini<- cor(std.dev,gini)
  corginibuffer <- cor(gini,BTI)
  corginiwidth <- cor(gini, width)
  skew[i] <- sk
}
warnings()
giniall
Buffer

df <- data.frame(ku,cu, giniall, Buffer, widthm, std, coefv, skew)
df
cor(df$giniall, df$std)
cor(df$giniall,df$coefv)
cor(df$giniall,df$Buffer)
cor(df$giniall,df$widthm)
cor(df$giniall, df$skew)
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
df2 <- na.omit(df2)
#df2 <- na.omit(df2$coefv)
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
df2$Buffer
cor(df2$Buffer, df2$giniall)
cor(df2$std, df2$giniall)
cor(df2$coefv,df2$giniall)
cor(df2$widthm,df2$giniall)
cor(df2$skew,df2$giniall)
x<-rlnorm(10000, meanlog = 0.5, sdlog = 0.8)
plot(x)
help(erf)
qburr(0.95, 1, 1.5)
install.packages("extremefit")
library(extremefit)
gamma(0)


curve(dburr(x, 0.5, 2), from=0, to=5,
      main = 'Burr Distributions',
      ylab = ' f(x)',
      lwd = 2,
      col = 'pink')
curve(dburr(x, 1, 2), from=0, to=5, col='green', add=TRUE)
curve(dburr(x, 1.5, 3), from=0, to=5, col='blue', add=TRUE)
curve(dburr(x, 3, 1), from=0, to=5, col='red', add=TRUE)
legend(3.5, 2, .4, .3, legend=c("c=0.5, k=2", "c=1, k=2","c=1.5, k=3", "c=3, k=1" ),
       col=c("pink", "green", "blue", "red"), lty=1, cex=0.8)
