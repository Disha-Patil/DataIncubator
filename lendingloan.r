#read the 2014 and 2014 csv file containg issued loan data
loan14<-read.csv("2014loan.csv")
loan15<-read.csv("2015loan.csv")

#join the two files for complete analysis
loan.data<-rbind(loan14,loan15)

#calculate median of loan amount issued after filtering out the NA values
loan.median<-median(loan.data[-which(is.na(loan.data[,'loan_amnt'])),'loan_amnt'])

#print the median loan amount
cat("median loan amount = ",loan.median,"\n")

