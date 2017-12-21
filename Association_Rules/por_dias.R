dia_22 <- read.csv2(file="/home/bastos/Documentos/DM2/dummydata/22022016_0930_16.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
dia_23 <- read.csv2(file="/home/bastos/Documentos/DM2/dummydata/23022016_0930_16.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
dia_24 <- read.csv2(file="/home/bastos/Documentos/DM2/dummydata/24022016_0930_16.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
dia_25 <- read.csv2(file="/home/bastos/Documentos/DM2/dummydata/25022016_1030_17.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
dia_26 <- read.csv2(file="/home/bastos/Documentos/DM2/dummydata/26022016_1030_17.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
#([0-9]+)\;([0-9]+)\:([0-9]+)\:([0-9]+)\;.+\;[0-9]+\:[0-9]+\:[0-9]+\;[0-9]+\;

#dia_22
data_22 <- data.frame(dia_22$id, gsub("\\s[0-9]*$", "", dia_22$zona))
colnames(data_22) <- c("id","zona")

#dia_23
data_23 <- data.frame(dia_23$id, gsub("\\s[0-9]*$", "", dia_23$zona))
colnames(data_23) <- c("id","zona")

#dia_24
data_24 <- data.frame(dia_24$id, gsub("\\s[0-9]*$", "", dia_24$zona))
colnames(data_24) <- c("id","zona")

#dia_25
data_25 <- data.frame(dia_25$id, gsub("\\s[0-9]*$", "", dia_25$zona))
colnames(data_25) <- c("id","zona")

#dia_26
data_26 <- data.frame(dia_26$id, gsub("\\s[0-9]*$", "", dia_26$zona))
colnames(data_26) <- c("id","zona")

#valores unicos
data_22 <- unique(data_22[c("id", "zona")])
data_23 <- unique(data_23[c("id", "zona")])
data_24 <- unique(data_24[c("id", "zona")])
data_25 <- unique(data_25[c("id", "zona")])
data_26 <- unique(data_26[c("id", "zona")])

#regras
library(carenR)
rls_22<-caren(data_22,Bas=TRUE)
rls_23<-caren(data_23,Bas=TRUE)
rls_24<-caren(data_24,Bas=TRUE)
rls_25<-caren(data_25,Bas=TRUE)
rls_26<-caren(data_26,Bas=TRUE)

#Plot de support
##support_22
hist(rls_22$Sup, main="Distribution of Support_22", xlab="Support")
nrules_22 <- apply(as.array(seq(0,1,0.05)),1,function(s)nrow(rls_22[rls_22$Sup>=s,]))
nrules_22
plot(seq(0,1,0.05),nrules_22,type='l',xlab='min-sup',ylab='nrules',
     main='Variation of nrules_22 with min.sup')

##support_23
hist(rls_23$Sup, main="Distribution of Support_23", xlab="Support")
nrules_23 <- apply(as.array(seq(0,1,0.05)),1,function(s)nrow(rls_23[rls_23$Sup>=s,]))
nrules_23
plot(seq(0,1,0.05),nrules_23,type='l',xlab='min-sup',ylab='nrules',
     main='Variation of nrules_23 with min.sup')

##support_24
hist(rls_24$Sup, main="Distribution of Support_24", xlab="Support")
nrules_24 <- apply(as.array(seq(0,1,0.05)),1,function(s)nrow(rls_24[rls_24$Sup>=s,]))
nrules_24
plot(seq(0,1,0.05),nrules_24,type='l',xlab='min-sup',ylab='nrules',
     main='Variation of nrules_24 with min.sup')

##support_25
hist(rls_25$Sup, main="Distribution of Support_25", xlab="Support")
nrules_25 <- apply(as.array(seq(0,1,0.05)),1,function(s)nrow(rls_25[rls_25$Sup>=s,]))
nrules_25
plot(seq(0,1,0.05),nrules_25,type='l',xlab='min-sup',ylab='nrules',
     main='Variation of nrules_25 with min.sup')

##support_26
hist(rls_26$Sup, main="Distribution of Support_26", xlab="Support")
nrules_26 <- apply(as.array(seq(0,1,0.05)),1,function(s)nrow(rls_26[rls_26$Sup>=s,]))
nrules_26
plot(seq(0,1,0.05),nrules_26,type='l',xlab='min-sup',ylab='nrules',
     main='Variation of nrules_26 with min.sup')

#conf
##conf_22
hist(rls_22$Conf, main="Distribution of Confidence_22", xlab="Confidence")
nrules_conf_22 <- apply(as.array(seq(0,1,0.05)),1,function(c)nrow(rls_22[rls_22$Conf>=c,]))
plot(seq(0,1,0.05),nrules_conf_22,type='l',xlab='min-conf',ylab='nrules',
     main='Variation of nrules_22 with min.conf')

##conf_23
hist(rls_23$Conf, main="Distribution of Confidence_23", xlab="Confidence")
nrules_conf_23 <- apply(as.array(seq(0,1,0.05)),1,function(c)nrow(rls_23[rls_23$Conf>=c,]))
plot(seq(0,1,0.05),nrules_conf_23,type='l',xlab='min-conf',ylab='nrules',
     main='Variation of nrules_23 with min.conf')

##conf_24
hist(rls_24$Conf, main="Distribution of Confidence_24", xlab="Confidence")
nrules_conf_24 <- apply(as.array(seq(0,1,0.05)),1,function(c)nrow(rls_24[rls_24$Conf>=c,]))
plot(seq(0,1,0.05),nrules_conf_24,type='l',xlab='min-conf',ylab='nrules',
     main='Variation of nrules_24 with min.conf')

##conf_25
hist(rls_25$Conf, main="Distribution of Confidence_25", xlab="Confidence")
nrules_conf_25 <- apply(as.array(seq(0,1,0.05)),1,function(c)nrow(rls_25[rls_25$Conf>=c,]))
plot(seq(0,1,0.05),nrules_conf_25,type='l',xlab='min-conf',ylab='nrules',
     main='Variation of nrules_25 with min.conf')

##conf_26
hist(rls_26$Conf, main="Distribution of Confidence_26", xlab="Confidence")
nrules_conf_26 <- apply(as.array(seq(0,1,0.05)),1,function(c)nrow(rls_26[rls_26$Conf>=c,]))
plot(seq(0,1,0.05),nrules_conf_26,type='l',xlab='min-conf',ylab='nrules',
     main='Variation of nrules_26 with min.conf')

#regras refeitas sup=0.1 e conf=0.6
rls_22<-caren(data_22,Bas=TRUE, min.sup = 0.1, min.conf = 0.6, imp=0.01)
rls_23<-caren(data_23,Bas=TRUE, min.sup = 0.1, min.conf = 0.6, imp=0.01)
rls_24<-caren(data_24,Bas=TRUE, min.sup = 0.1, min.conf = 0.6, imp=0.01)
rls_25<-caren(data_25,Bas=TRUE, min.sup = 0.1, min.conf = 0.6, imp=0.01)
rls_26<-caren(data_26,Bas=TRUE, min.sup = 0.1, min.conf = 0.6, imp=0.01)

#regras sequenciais csv (modifica data_*)
##dia_22
library(chron)
dia_22 <- dia_22[order(dia_22$id,60*24*as.numeric(times(dia_22$hora_e_zona)),decreasing=F),]
data_22 <- data.frame(dia_22$id, dia_22$zona)
colnames(data) <- c("id","zona")
write.csv(data_22,file = "dia_22.csv")
##dia_23
dia_23 <- dia_23[order(dia_23$id,60*24*as.numeric(times(dia_23$hora_e_zona)),decreasing=F),]
data_23 <- data.frame(dia_23$id, dia_23$zona)
colnames(data) <- c("id","zona")
write.csv(data_23,file = "dia_23.csv")
##dia_24
dia_24 <- dia_24[order(dia_24$id,60*24*as.numeric(times(dia_24$hora_e_zona)),decreasing=F),]
data_24 <- data.frame(dia_24$id, dia_24$zona)
colnames(data) <- c("id","zona")
write.csv(data_24,file = "dia_24.csv")
##dia_25
dia_25 <- dia_25[order(dia_25$id,60*24*as.numeric(times(dia_25$hora_e_zona)),decreasing=F),]
data_25 <- data.frame(dia_25$id, dia_25$zona)
colnames(data) <- c("id","zona")
write.csv(data_25,file = "dia_25.csv")
##dia_26
dia_26 <- dia_26[order(dia_26$id,60*24*as.numeric(times(dia_26$hora_e_zona)),decreasing=F),]
data_26 <- data.frame(dia_26$id, dia_26$zona)
colnames(data) <- c("id","zona")
write.csv(data_26,file = "dia_26.csv")




