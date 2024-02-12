set.seed(12345)
alpha <- runif(10000, min=1, max=5)
alpha
set.seed(12345)
beta <- runif(10000, min=0, max=2)
beta
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
  alpha.smpl <- sample(alpha,replace=T)
  alpha.smpl
  beta.smpl <- sample(beta,replace = T)
  beta.smpl
  mean <- alpha.smpl/beta.smpl
  mean
  std.dev <- sqrt(alpha.smpl)/(beta.smpl)
  std.dev
  cof.var <- std.dev/mean
  cof.var
  per_95 <- qgamma(0.95,alpha.smpl,beta.smpl, lower.tail = TRUE)
  per_95
  per_90 <- qgamma(0.90,alpha.smpl,beta.smpl, lower.tail = TRUE)
  per_50 <- qgamma(0.5, alpha.smpl, beta.smpl, lower.tail = TRUE)
  per_50
  per_10 <- qgamma(0.1, alpha.smpl, beta.smpl, lower.tail = TRUE)
  BTI <- ((per_95-per_50)/(per_50))
  sk <- ((per_90-per_10)/(per_50 - per_10))
  width <- ((per_90-per_10)/per_10)
  gini <- 2*gamma((2*alpha.smpl+1)/2)*1/((alpha.smpl)*(gamma(alpha.smpl)*sqrt(22/7)))
  gini
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
df <- data.frame(giniall, Buffer, widthm, skew ,std, coefv, corstgini,corginibuffer,corginiwidth)
df
library("ggpubr")
ggscatter(df, x = "coefv", y = "giniall", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Buffer", ylab = "GINI Coefficient")
ggscatter(df, x = "Buffer", y = "giniall", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Buffer", ylab = "GINI Coefficient")

cor(df$giniall, df$std)
cor(df$giniall,df$Buffer)
cor(df$giniall,df$widthm)
cor(df$giniall, df$coefv)
cor(df$giniall, df$skew)
df$coefv
std
boxplot(df$corstgini)
boxplot(std)

df2<-df[!(df$giniall>=1),]
df2$giniall
length(df2$std)
cor(df2$giniall, df2$std)
cor(df2$giniall,df2$Buffer)
cor(df2$giniall,df2$widthm)
cor(df2$giniall, df2$coefv)
cor(df2$giniall, df2$skew)


ggscatter(df2, x = "std", y = "giniall", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "std", ylab = "GINI Coefficient")
cor(df2$std, df2$giniall)
ggplot(df2, aes(x=coefv, y=giniall)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(df2, aes(x=Buffer, y=giniall)) + 
  geom_point()+
  geom_smooth(method=lm, se=TRUE)
ggplot(df2, aes(x=widthm, y=giniall)) + 
  geom_point()+
  geom_smooth(method=lm)
df2$std
df2$Buffer


###plotgraphs
curve(dgamma(x, shape=1, scale = 2), from=0, to=5,
      main = 'Gamma Distributions',
      ylab = ' f(x)',
      lwd = 2,
      col = 'pink')
curve(dgamma(x, shape=1.5, scale = 2), from=0, to=5, col='green', add=TRUE)
curve(dgamma(x, shape=0.5, scale = 2.5), from=0, to=5, col='blue', add=TRUE)
curve(dgamma(x, shape=3, scale = 1), from=0, to=5, col='red', add=TRUE)
legend(3, .5, .4, .3, legend=c("shape=2, scale=2", "shape=1.5, scale=2","shape=2.5, scale=2.5", "shape=1.5, scale=3" ),
       col=c("pink", "green", "blue", "red"), lty=1, cex=0.8)

