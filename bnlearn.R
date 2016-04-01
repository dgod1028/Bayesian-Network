## install.packages("bnlearn")
library(bnlearn)

bn.gs <- gs(data)
bn.gs

bn.hc <- hc(data,score ="aic")
bn.hc

par(mfrow = c(1,2))
plot(bn.gs, main = "Constraint-based algorithms")
plot(bn.hc, main = "Hill-Climbing",highlight = "幸福")


score(bn.hc,data,type="aic")

fitted = bn.fit(bn.hc,data,method="bayes")   ### method = bayes or mle
(Coef <- coefficients(fitted))

Coef
Coef$教育

#### Black list
banlist = data.frame(from=c("性別","子供"),to=c("子供","性別"))
banlist

bn.hc2 <- hc(data,score="aic",blacklist = banlist)
bn.hc2
par(mfrow = c(1,1))
plot(bn.hc2, main = "Hill-Climbing",highlight = "幸福")
graphviz.plot(bn.hc2)


#####





####　作業中

#---絵に上書き
XMax <- max(axTicks(1))
YMax <- max(axTicks(2))


par(mfrow = c(1,1))
plot(bn.hc, main = "Hill-Climbing",highlight = "幸福")
#切片
text(XMax*0.05, YMax*0.7, round(Coef$性別, digits=2), adj=0)
text(XMax*0.85, YMax*0.6, round(Coef$SBP, digits=2), adj=0)
text(XMax*0.35, YMax*0.9, round(Coef$FBS, digits=2), adj=0)
text(XMax*0.35, YMax*0.05, round(Coef$BMI[1], digits=2), adj=0)
#回帰係数
text(XMax*0.3, YMax*0.3, round(Coef$BMI[2], digits=3), adj=0)
text(XMax*0.65, YMax*0.3, round(Coef$BMI[3], digits=3), adj=0)
text(XMax*0.45, YMax*0.5, round(Coef$BMI[4], digits=3), adj=0)

## [1] "性別" "子供" "宗教" "教育" "幸福"
