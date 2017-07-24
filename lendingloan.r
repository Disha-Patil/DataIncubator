#read the csv files
loan14<-read.csv("2014loan.csv")
loan15<-read.csv("2015loan.csv")

#combine data of two years
loan.data<-rbind(loan14,loan15)

#median
loan.median<-median(loan.data[-which(is.na(loan.data[,'loan_amnt'])),'loan_amnt'])
cat("median loan amount=",loan.median,"\n")


#popular purpose
#which is the most popular purpose 
which(table(loan.data[,'purpose'])==max(table(loan.data[,'purpose'])))

#but we only need how popular it is so we need only value
fraction.required<-max(table(loan.data[,'purpose']))/nrow(loan.data)

#average rate of interest
each.purpose<-levels(loan.data[,'purpose'])
purpose.intrate<-lapply(1:length(each.purpose),function(i){loan.data[which(loan.data[,'purpose']==each.purpose[i]),'int_rate']})

purpose.intrate1<-sapply(1:length(purpose.intrate),function(i){as.numeric(purpose.intrate[[i]])})

mean.intrate<-sapply(1:length(purpose.intrate1),function(i){mean(purpose.intrate1[[i]])})

ratio.required<-min(mean.intrate)/max(mean.intrate)


#36 months term
df14<-as.data.frame(table(loan.data[1:nrow(loan14),'term']))
term14<-df14[2,2]

df15<-as.data.frame(table(loan.data[(nrow(loan14)+1):nrow(loan.data),'term']))
term15<-df15[2,2]

#fraction difference
difference<-abs((term14/nrow(loan14))-(term15/nrow(loan15)))



#loan status
nostatus<-which(loan.data[,'loan_status']=="")

loan.data[nostatus,'loan_status']<-"Default"

default.status<-which(loan.data[,'loan_status']=="Default")

date.diff <- 12 * as.numeric(as.yearmon(loan.data[default.status,'last_pymnt_d'], "%b-%Y") - as.yearmon(loan.data[default.status,'issue_d'], "%b-%Y"))
#replace NA by median
date.diff[which(is.na(date.diff))]<-median( date.diff,na.rm=T)

default.term<-loan.data[default.status,'term']

term.months<-as.numeric(substr(default.term,2,3))

term.months[which(is.na(term.months))]<-median(term.months,na.rm=T)

spent.time<-date.diff/term.months

sd.default<-sd(spent.time)


#pearson correlation coefficient

completed.term<-(which(loan.data[,'loan_status']=="Fully Paid"))
total.paid<-(loan.data[completed.term,'total_pymnt'])
funded<-(loan.data[completed.term,'funded_amnt'])

rate<-(total.paid-funded)/funded
int.rate<-loan.data[completed.term,'int_rate']

cor(rate,as.numeric(int.rate)/100,method="pearson")


#ratio of probability
each.purpose<-levels(loan.data[,'purpose'])
states<-levels(loan.data[,'addr_state'])

purpose.states<-list()
prob.ratio<-list()
loop=1
for(t in 2:length(each.purpose)){
	#B<-c();A<-c()
	purpose.states[[t]]<-loan.data[which(loan.data[,'purpose']==each.purpose[t]),'addr_state']
	totalinstate<-sum(table(purpose.states[[t]]))
	pairs<-as.data.frame(table(purpose.states[[t]])[which(table(purpose.states[[t]])>10)])
	if(nrow(pairs)==0){next}
	B<-pairs[,2]/totalinstate
	A<-length(which(loan.data[,'purpose']==each.purpose[t]))/nrow(loan.data)
	prob.ratio[[loop]]<-data.frame(pupose=each.purpose[t],state=pairs[,1],ratio=as.numeric(B)/as.numeric(A))
	loop=loop+1
}

max.ratio<-sapply(1:length(prob.ratio),function(i){max(prob.ratio[[i]][,3])})

prob.ratio[[which(max.ratio==max(max.ratio))]]

#linear fit

sub.grade<-levels(loan.data[,'sub_grade'])

subset.grade<-list()

for (s in 1:length(sub.grade)){

	subset.grade[[s]]<-loan.data[which(loan.data[,'sub_grade']==sub.grade[s]),c('int_rate','loan_status')]

}


