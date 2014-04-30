library(Hmisc)
library(car)
library(MASS)
library(leaps) ##for 
loan_data<-read.csv("loansData.csv")
loan_data<-na.omit(loan_data)
print(nrow(loan_data))
#loan_data_Int<-data.frame(n=nrow(loan_data))
num_slice=10

AR=as.integer(cut2(loan_data$Amount.Requested,g=num_slice))
loan_data_Int<-data.frame(Amount.Requested.Int=AR)

loan_data_Int$Amount.Funded.By.Investors.Int<-as.integer(cut2(loan_data$Amount.Funded.By.Investors,g=num_slice))
loan_data_Int$Interest.Rate.Float<-as.numeric(sub("%","",loan_data$Interest.Rate))
loan_data_Int$Loan.Length.Int<-as.integer(sub("months","",loan_data$Loan.Length))/2
loan_data_Int$Loan.Purpose.Int<-as.integer(loan_data$Loan.Purpose)
#print(cut2(as.numeric(sub("%","",loan_data$Debt.To.Income.Ratio)),g=10))
loan_data_Int$Debt.To.Income.Ratio.Int<-as.integer(cut2(as.numeric(sub("%","",loan_data$Debt.To.Income.Ratio)),g=num_slice))

loan_data_Int$State.Int<-as.integer(loan_data$State)

loan_data_Int$Home.Ownership.Int<-as.integer(loan_data$Home.Ownership)
loan_data_Int$Monthly.Income.Int<-as.integer(cut2(loan_data$Monthly.Income,g=num_slice))

loan_data_Int$FICO.Range.Int<-as.integer(loan_data$FICO.Range)

loan_data_Int$Open.CREDIT.Lines.Int<-as.integer(cut2(loan_data$Open.CREDIT.Lines,g=num_slice))
loan_data_Int$Revolving.CREDIT.Balance.Int<-as.integer(cut2(loan_data$Revolving.CREDIT.Balance,g=num_slice))
loan_data_Int$Inquiries.in.the.Last.6.Months.Int<-loan_data$Inquiries.in.the.Last.6.Months
loan_data_Int$Employment.Length.Int<-as.integer(loan_data$Employment.Length)






print(names(loan_data_Int))


#forbidden_list=c('"Loan.Purpose"','"State"','"Home.Ownership"',
#                '"FICO.Range"','"Employment.Length"',
#                 '"Debt.To.Income.Ratio"','"Loan.Length"',
#                '"Interest.Rate"','"Interest.Rate.Float"')
#forbiddien_list_formula <- paste(paste0("names(loan_data)!=",forbidden_list) ,collapse=" & ")

#variables_list<-names(loan_data)[eval(parse(text=forbiddien_list_formula))]


#print(variables_list)

plot_names<-names(loan_data)[names(loan_data)!="Interest.Rate" & names(loan_data)!="FICO.Range"]
col_names<-names(loan_data_Int)[names(loan_data_Int)!="Interest.Rate.Float" & names(loan_data_Int)!="FICO.Range.Int"]

print(names(loan_data))
#print(length(plot_names))
#par(mfrow=c(3,4))
ytit<-"FICO Range"
xtit<-"Interest Rate"
fit3<-lm(Interest.Rate.Float ~ FICO.Range.Int  , data=loan_data_Int  )
for (i in 1:length(plot_names)){
#for (i in 1:1){
  pdf(paste(gsub("[.]","_",plot_names[i]),".pdf")) 
  plot(loan_data_Int$FICO.Range.Int,loan_data_Int$Interest.Rate.Float,col=loan_data_Int[,col_names[i]],main=gsub("[.]"," ",plot_names[i]),ylab=ytit,xlab=xtit)
  abline(fit3)
  #print(plot_names[i])
  dev.off()
  pdf(paste("HIST_",gsub("[.]","_",plot_names[i]),".pdf")) 
  plot(density(loan_data_Int[,col_names[i]]),main=gsub("[.]"," ",plot_names[i]))
  #print(plot_names[i])
  dev.off()
}



variables_list_Int<-names(loan_data_Int)[names(loan_data_Int)!="Interest.Rate.Float"]

fit_formula<-as.formula(paste("Interest.Rate.Float ~ ",paste(variables_list_Int,collapse="+")))
fit<-lm(fit_formula, data=loan_data_Int  )
#print(summary(fit))
#print(coefficients(fit)) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
#print(anova(fit)) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(fit)
#pairs(loan_data_Int)
step(fit,direction="backward")

fit2<-lm(Interest.Rate.Float ~ FICO.Range.Int + Loan.Length.Int , data=loan_data_Int  )
fit3<-lm(Interest.Rate.Float ~ FICO.Range.Int  , data=loan_data_Int  )

summary_fit1<-summary(fit)
summary_fit2<-summary(fit2)
summary_fit3<-summary(fit3)

#print(summary_fit2)
print(summary_fit1$sigma)
print(summary_fit2$sigma)
print(summary_fit3$sigma)

#stepwise_regression
initial_lm=lm(Interest.Rate.Float~FICO.Range.Int,data=loan_data_Int )
final_lm=lm(Interest.Rate.Float~.,data=loan_data_Int)
step(initial_lm,scope=list(lower=initial_lm,upper=final_lm),direction="forward")
##results
#Interest.Rate.Float ~ FICO.Range.Int + Loan.Length.Int + Amount.Funded.By.Investors.Int + 
#  Inquiries.in.the.Last.6.Months.Int + Open.CREDIT.Lines.Int + 
#  Loan.Purpose.Int + Revolving.CREDIT.Balance.Int + Amount.Requested.Int + 
#  Home.Ownership.Int
print("####################################")
best_lm<-lm(Interest.Rate.Float ~ FICO.Range.Int + Loan.Length.Int + Amount.Funded.By.Investors.Int + 
             Inquiries.in.the.Last.6.Months.Int + Open.CREDIT.Lines.Int + 
             Loan.Purpose.Int + Revolving.CREDIT.Balance.Int + Amount.Requested.Int + 
              Home.Ownership.Int,data=loan_data_Int)
######## stepwise_regression using AIC
#step_AIC<-stepAIC(fit,direction="both")
#step_AIC$anova #display_results

### got the same results
#Step:  AIC=3652.32
#Interest.Rate.Float ~ Amount.Requested.Int + Amount.Funded.By.Investors.Int + 
#  Loan.Length.Int + Loan.Purpose.Int + Home.Ownership.Int + 
#  FICO.Range.Int + Open.CREDIT.Lines.Int + Revolving.CREDIT.Balance.Int + 
#  Inquiries.in.the.Last.6.Months.Int
###############################################
leaps=regsubsets(fit_formula, data=loan_data_Int,nbest=10)
labels_list<-gsub("[.]"," ",names(loan_data))
pdf("leaps_r2.pdf")
##bty="n",yaxt="n",
plot(leaps,scale="r2",title="Random error variaton by adding different variables",labels=labels_list,ylab="Random Error")
#subsets(leaps,names=labels_list)
dev.off()
