library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

df = read.csv("US-DOM-IF.csv", sep=";", dec=",")
colnames(df) <- c("Journal","PercentUS", "PercentDOM","IFScore", "Country")
df$PercentDOM = df$PercentDOM*100
summary = summary(df)
summary
plot(df$PercentDOM ~ df$IFScore, data = df)
lmatt = lm(df$PercentDOM ~ df$IFScore, data = df)
summary(lmatt)

par(mfrow=c(2,2))
plot(lmatt)
par(mfrow=c(1,1))


lmattgraph<-ggplot(df, aes(x=df$IFScore, y=df$PercentDOM), )+
  geom_point()+
  ylim(-0.5,100)
lmattgraph


lmattgraph <- lmattgraph + geom_smooth(method="lm", col="black")


lmattgraph <- lmattgraph +
  stat_regline_equation(label.x = 3, label.y = 7)

lmattgraph


lmattgraph +
  theme_bw() +
  geom_point(aes(color=Country)) +
  labs(title = "IF-Score in relation to %-Domestic Contributions",
       x = "IF-Score",
       y = "%-Domestic Contributions")

