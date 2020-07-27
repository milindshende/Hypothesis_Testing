############Q1 = 2 sample T Test ##################
Cutlet<-read.csv(file.choose())    # Cutlet.csv file
View(Cutlet)
attach(Cutlet)

#############Normality test###############

#Ho: No action, if Y1 & Y2 are normal
#Ha: Take action, if Y1 or  Y2 are not normal

shapiro.test(Unit.A)
# p-value = 0.32>0.05 so p high null fly => It follows normal distribution

shapiro.test(Unit.B)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution
# Both Y1&Y2 are found normal

#############Variance test###############
#Ho -> Variances are equal (Var of Y1 = Var of Y2)
#Ha -> Variances are not equal (Var of Y1 != Var of Y2)
?var.test

var.test(Unit.A,Unit.B)#variance test
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances

############2 sample T Test ##################
t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level =0.95,correct=TRUE )

t.test(InterestRateWaiver,StandardPromotion,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.02523 < 0.05 accept alternate Hypothesis 
# unequal means

?t.test
t.test(InterestRateWaiver,StandardPromotion,alternative = "greater",var.equal = T)

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (InterestRateWaiver-StandardPromotion) < 0
# Alternative Hypothesis -> (StandardPromotion - InterestRateWaiver) > 0
# p-value = 0.01211 < 0.05 => p low null go => accept alternate hypothesis
# InterestRateWaiver better promotion than StandardPromotion


############# Q2 = Anova (LAB TAT )##########
LabTAT<-read.csv(file.choose())   # LabTAT(unstacked).csv
View(LabTAT)
Stacked_LabTAT <- stack(LabTAT)
attach(Stacked_LabTAT)

#############Normality test###############
library(nortest)
ad.test(Stacked_LabTAT$values) 

############# Variance test ###############
install.packages("car")
library(car)
leveneTest(Stacked_LabTAT$values~Stacked_LabTAT$ind, data = Stacked_LabTAT)   #Test for equal Variance

################ One-way Anova ########
Anova_results <- aov(values~ind,data = Stacked_LabTAT)
summary(Anova_results)
# p-value = 0.104 > 0.05 accept null hypothesis 
# All Proportions all equal 

# Customer order form 
# Unstacked data 

cof<-read_excel(file.choose()) # customer_order(unstacked).xlsx
View(cof) # countries are in their own columns; so we need to stack the data 
stacked_cof<-stack(cof)
attach(stacked_cof)
View(stacked_cof)
table(stacked_cof$ind,stacked_cof$values)
chisq.test(table(stacked_cof$ind,stacked_cof$values))

############2 sample T Test ##################
t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level =0.95,correct=TRUE )
t.test(LabTAT$Laboratory.1,LabTAT$Laboratory.2,alternative="two.sided",conf.level=0.95,correct=TRUE)
#Ho->Null Hypothesis ->mu of Lab1=mu of Lab2
#H1->Alt Hypothesis->mu of Lab1 != mu of Lab2
#p-value=0.7663 > 0.05 ; P High Null Fly -> mu is equal of Lab1&Lab2
t.test(LabTAT$Laboratory.1,LabTAT$Laboratory.3,alternative="two.sided",conf.level=0.95,correct=TRUE)
#Ho->Null Hypothesis ->mu of Lab1=mu of Lab3
#H1->Alt Hypothesis->mu of Lab1 != mu of Lab3
#p-value< 0.05 ; P Low Null Go-> mu is not equal of Lab1&Lab3
t.test(LabTAT$Laboratory.1,LabTAT$Laboratory.3,alternative = "greater",var.equal = T)
#Ho->Null Hypothesis ->mu of Lab1 < mu of Lab3
#H1->Alt Hypothesis->mu of Lab1 > mu of Lab3
#p-value=1 means >0.05 ; P High Null Fly-> mu of Lab 1< mu o Lab 3
t.test(LabTAT$Laboratory.1,LabTAT$Laboratory.4,alternative="two.sided",conf.level=0.95,correct=TRUE)
#Ho->Null Hypothesis ->mu of Lab1=mu of Lab4
#H1->Alt Hypothesis->mu of Lab1 != mu of Lab4
#p-value< 0.05 ; P Low Null Go-> mu is not equal of Lab1&Lab4
t.test(LabTAT$Laboratory.1,LabTAT$Laboratory.4,alternative = "greater",var.equal = T)
#Ho->Null Hypothesis ->mu of Lab1 < mu of Lab4
#H1->Alt Hypothesis->mu of Lab1 > mu of Lab4
#p-value<0.05 ; P Low Null Go-> mu of Lab 1> mu of Lab 3
t.test(LabTAT$Laboratory.2,LabTAT$Laboratory.3,alternative="two.sided",conf.level=0.95,correct=TRUE)
#Ho->Null Hypothesis ->mu of Lab2=mu of Lab3
#H1->Alt Hypothesis->mu of Lab2 != mu of Lab3
#p-value< 0.05 ; P Low Null Go-> mu is not equal of Lab2&Lab3
t.test(LabTAT$Laboratory.2,LabTAT$Laboratory.3,alternative = "greater",var.equal = T)
#Ho->Null Hypothesis ->mu of Lab2 < mu of Lab3
#H1->Alt Hypothesis->mu of Lab2 > mu of Lab3
#p-value=1 > 0.05 ; P High Null Fly-> mu of Lab 2< mu of Lab 3
t.test(LabTAT$Laboratory.2,LabTAT$Laboratory.4,alternative="two.sided",conf.level=0.95,correct=TRUE)
#Ho->Null Hypothesis ->mu of Lab2=mu of Lab4
#H1->Alt Hypothesis->mu of Lab2 != mu of Lab4
#p-value< 0.05 ; P Low Null Go-> mu is not equal of Lab2&Lab4
t.test(LabTAT$Laboratory.2,LabTAT$Laboratory.4,alternative = "greater",var.equal = T)
#Ho->Null Hypothesis ->mu of Lab2 < mu of Lab4
#H1->Alt Hypothesis->mu of Lab2 > mu of Lab4
#p-value< 0.05 ; P low Null Go-> mu of Lab 2> mu of Lab 4
t.test(LabTAT$Laboratory.3,LabTAT$Laboratory.4,alternative="two.sided",conf.level=0.95,correct=TRUE)
#Ho->Null Hypothesis ->mu of Lab3=mu of Lab4
#H1->Alt Hypothesis->mu of Lab3 != mu of Lab4
#p-value< 0.05 ; P Low Null Go-> mu is not equal of Lab3&Lab4
t.test(LabTAT$Laboratory.3,LabTAT$Laboratory.4,alternative = "greater",var.equal = T)
#Ho->Null Hypothesis ->mu of Lab3 < mu of Lab4
#H1->Alt Hypothesis->mu of Lab3 > mu of Lab4
#p-value< 0.05 ; P low Null Go-> mu of Lab 3> mu of Lab 4







t.test(InterestRateWaiver,StandardPromotion,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.02523 < 0.05 accept alternate Hypothesis 
# unequal means

?t.test
t.test(InterestRateWaiver,StandardPromotion,alternative = "greater",var.equal = T)

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (InterestRateWaiver-StandardPromotion) < 0
# Alternative Hypothesis -> (StandardPromotion - InterestRateWaiver) > 0
# p-value = 0.01211 < 0.05 => p low null go => accept alternate hypothesis
# InterestRateWaiver better promotion than StandardPromotion


#########Q3= Chi Square(Male Female Buyer Ratio)#################
br<-read.csv(file.choose())
attach(br)
tbr<-data.frame(t(br[-1]))
colnames(tbr)<-c("Male","Female")
chisq.test(tbr)
# p-value = 0.6603 means>0.05 -> Accept Null Hypothesis
# Null Hypothesis -> H0-> Allproportions are equal
# Alt Hypothesis-> Ha->Not all proportions are equal
library(dummies)
library(psych)
?dummy.code
dummy.code(q4cof,group = "Error Free")
##############Q4 CustOrderForm##########
q4cof<-read.csv(file.choose())
View(q4cof)
dt <- ifelse(q4cof=="Error Free",0,1)


View(dt)
df_dt<-data.frame(dt)
summary(df_dt)
attach(df_dt)
stk_q4<- stack(df_dt)
summary(stk_q4)
chisq.test(values,ind)
#H0=Defective % not varies by the centre.
#H1=Defective % varies by the centre
#P-value=0.2771 > 0.05 => P high Null Fly => Accept Null H0 => Def % not Varies


########q5 faltoons#########
faltoons<-read.csv(file.choose())
View(faltoons)
attach(faltoons)
stkfaltoons<-stack(faltoons)
table(Weekdays,Weekend)
chisq.test(table(Weekdays,Weekend))
#H0=Male V/s Female Ratio is not equal on all days of week.
# H1= Male V/s Female Ratio is equal on all days of week.
# P=1 >0.05 ; so P high Null Fly ; Accept H0-> Ratio not equal on all days of week
