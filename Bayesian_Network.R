## install.packages("deal")
## install.packages("Ecdat")
library(Ecdat)

data(Fair)
head(Fair)

data = Fair[,c(-2,-3,-9,-7)]
data[] = lapply(data,as.factor)

temp=data[,5]
data[,5] = data[,1]
data[,1] = temp


colnames(data) = c("幸福","子供","宗教","教育","性別")
head(data)
data = data[,c("幸福","教育","宗教","子供","性別")]

library(deal)
network.prior<- network(data)
prior.distribution <- jointprior(network.prior)
update <- learn(network.prior, data, prior.distribution)
network.post <- autosearch(getnetwork(update), data,prior.distribution,trace=FALSE)
plot(getnetwork(network.post),main="Bayesian Network",showban=TRUE)

ban <- rbind(cbind(1,1:5),cbind(1:5,4),cbind(1:5,5))
banlist(network.prior) = ban
update <- learn(network.prior, data, prior.distribution)
network.post <- autosearch(getnetwork(update), data,prior.distribution,trace=FALSE)
plot(getnetwork(network.post),main="Bayesian Network",showban=FALSE)


##逆推定

prob = localprob(getnetwork(network.post))


### P(幸福=5) 


Pro = function(localprob=prob,A=1:2,B=1:2,C=1:5,D=1:7,E=5){
PR=0
for(e in E){
  for(a in A) { 					 		## a = sex
	if(a == 1){sex = "male"} else{sex =female}

	for(b in B) {
		if(b ==1){kid = "yes"}else{kid="no"}	## b = kid
		for(c in C) {   				## c = syukyo
			rel = as.character(c)
			for(d in D){				## d = education
				if(d==1){edu="9"}
				if(d==2){edu="12"}
				if(d==3){edu="14"}
				if(d==4){edu="16"}
				if(d==5){edu=="17"}
				if(d==6){edu=="18"}
				if(d==7){edu=="20"}
				Temp = 
				  prob$幸福[e,edu,kid,sex] *
				  prob$教育[edu,rel,kid,sex] *
				  prob$宗教[rel,kid,sex] *
				  prob$子供[kid] *
				  prob$性別[sex] 
				PR = PR + as.vector(Temp)
				
				}
			}
		}

	}
}
	return(PR)
}

Pr = Pro()				###	P(幸福=5)
P_MAN = Pro(A=1)			###	P(幸福=5 |　性別 = 男) *P(性別 = 男)
P_Woman = Pro(A=2)		###	P(幸福=5 |　性別 = 女) *P(性別 = 女）

P_M = P_MAN / Pr			###	P(性別 = 男 | 幸福 = 5)
P_W = P_Woman / Pr		###	P(性別 = 女 | 幸福 = 5)

P_Kid = Pro(B=1)			###	P(幸福 = 5 | 子供 = yes ) * P(子供 = yes)
P_NoKid = Pro(B=2)		###	P(幸福 = 5 | 子供 = no )	 * P(子供 = no)

P_K = P_Kid / Pr			###	P(子供 = yes | 幸福 = 5)	
P_NK = P_NoKid / Pr		###	P(子供	= no  | 幸福 = 5)

P_M
P_W
P_K
P_NK
