#회귀분석 연습문제

#p.153~


#label 통하여 보기 편하게함.

hsb2<-read.table("DataSet/hsb2.txt", header=T, sep="\t")

head(hsb2)

hsb2$gender<-factor(hsb2$female, labels=c('male', 'female'))

hsb2$ses2<-factor(hsb2$ses, labels=c('low', 'middle', 'high'))

hsb2$prog2<-factor(hsb2$prog, labels=c('general', 'academic', 'vocational'))

hsb2$race2<-factor(hsb2$race, labels=c('AfricanAmerican', 'Asian', 'Hispanic', 'White'))

hsb2$schtyp2<-factor(hsb2$schtyp, labels=c('public', 'private'))


head(hsb2)

str(hsb2)

#p.155

hsb2_cat = hsb2 %>% select(gender, ses2, prog2, race2)

lapply(hsb2_cat, table)


#p.156

#1

head(hsb2)

hsb2_new = hsb2 %>% select(read:socst)

corrl = cor(hsb2_new)
ggcorrplot::ggcorrplot(corrl, hc.order = T, type = 'lower', lab = T)

#2

reg_hsb2 = lm(read:socst, data=hsb2)

summary(reg_hsb2)

