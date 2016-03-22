## install.packages("deal")
## install.packages("Ecdat")
library(Ecdat)

data(Fair)
head(Fair)

data = Fair[,c(-3,-9,-7)]
data[] = lapply(data,as.factor)
colnames(data) = c("性別","年齢","子供","宗教","教育","幸福")

library(deal)
network.prior<- network(data)
prior.distribution <- jointprior(network.prior)
update <- learn(network.prior, data, prior.distribution)
network.post <- autosearch(getnetwork(update), data,prior.distribution,trace=FALSE)
plot(getnetwork(network.post),main="Bayesian Network",showban=TRUE)

#### Ban Listを入れる
ban <- rbind(cbind(1:6,1),cbind(1:6,2),cbind(1:6,3),cbind(6,1:6))
banlist(network.prior) = ban
update <- learn(network.prior, data, prior.distribution)
network.post <- autosearch(getnetwork(update), data,prior.distribution,trace=FALSE)
plot(getnetwork(network.post),main="Bayesian Network",showban=FALSE)
