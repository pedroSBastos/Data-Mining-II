#DADOS -> R
dia_22 <- read.csv2(file="/home/juliana/Área de Trabalho/dummydata/22022016_0930_16.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
dia_23 <- read.csv2(file="/home/juliana/Área de Trabalho/dummydata/23022016_0930_16.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
dia_24 <- read.csv2(file="/home/juliana/Área de Trabalho/dummydata/24022016_0930_16.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
dia_25 <- read.csv2(file="/home/juliana/Área de Trabalho/dummydata/25022016_1030_17.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
dia_26 <- read.csv2(file="/home/juliana/Área de Trabalho/dummydata/26022016_1030_17.csv", col.names=c("id","hora_e_loja","zona","hora_e_zona","tempo_per"))
#([0-9]+)\;([0-9]+)\:([0-9]+)\:([0-9]+)\;.+\;[0-9]+\:[0-9]+\:[0-9]+\;[0-9]+\;

#juntar todo
todos_dias <- dia_22
maximo <- max(todos_dias$id)
aux <- dia_23
aux$id <- dia_23$id + maximo
todos_dias <- rbind(todos_dias, aux)
maximo <- max(todos_dias$id)
aux <- dia_24
aux$id <- dia_24$id + maximo
todos_dias <- rbind(todos_dias, aux)
maximo <- max(todos_dias$id)
aux <- dia_25
aux$id <- dia_25$id + maximo
todos_dias <- rbind(todos_dias, aux)
maximo <- max(todos_dias$id)
aux <- dia_26
aux$id <- dia_26$id + maximo
todos_dias <- rbind(todos_dias, aux)

################################PARTE1_TRATAMENTO_DE_DADOS######################

#DETETAR EMPREGADOS(OUTLIERS)
#numero de ocorrecias de id's
id_freq <- NULL
new <- NULL
for (i in unique(todos_dias$id)){
  new <- data.frame(i,length(which(todos_dias$id==i)))
  id_freq <- rbind(id_freq,new)
}
id_freq <- id_freq[order(id_freq$length.which.todos_dias.id....i..,decreasing=T),]
boxplot(id_freq$length.which.todos_dias.id....i..,main="Nº de mudanças de zona por ID", ylab="Nº de mudanças")

#soma de permanencias
soma_per <- NULL
new <- NULL
for (i in unique(todos_dias$id)){
  new <- data.frame(i,sum(todos_dias$tempo_per[which(todos_dias$id==i)]))
  soma_per <- rbind(soma_per,new)
}
soma_per <- soma_per[order(soma_per$sum.todos_dias.tempo_per.which.todos_dias.id....i...,decreasing = T),]
boxplot(soma_per$sum.todos_dias.tempo_per.which.todos_dias.id....i...,main="Tempo de permanência por ID", ylab="Tempo de Permanência")

#decidimos apenas analisar os que tem menos de 500 moves os restantes serao empregados
a_remover <- c()
a_remover <- c(id_freq$i[id_freq$length.which.todos_dias.id....i.. >500])

#decidomos remover os que tem mais de 25000
a_remover <- c(a_remover,soma_per$i[soma_per$sum.todos_dias.tempo_per.which.todos_dias.id....i...>25000])


#quem tem horas fora do normal
#dia 22 das 8h30(510) as 17h(1020)
a_remover <- c(a_remover, dia_22$id[60*24*as.numeric(times(dia_22$hora_e_zona))<510])
a_remover <- c(a_remover, dia_22$id[60*24*as.numeric(times(dia_22$hora_e_zona))>1020])

#dia 23 das 8h30(510) as 17h(1020)
a_remover <- c(a_remover, dia_23$id[60*24*as.numeric(times(dia_23$hora_e_zona))<510])
a_remover <- c(a_remover, dia_23$id[60*24*as.numeric(times(dia_23$hora_e_zona))>1020])

#dia 24 das 8h30(510) as 17h(1020)
a_remover <- c(a_remover, dia_24$id[60*24*as.numeric(times(dia_24$hora_e_zona))<510])
a_remover <- c(a_remover, dia_24$id[60*24*as.numeric(times(dia_24$hora_e_zona))>1020])

#dia 25 das 9h30(570) as 18h(1080)
a_remover <- c(a_remover, dia_25$id[60*24*as.numeric(times(dia_25$hora_e_zona))<570])
a_remover <- c(a_remover, dia_25$id[60*24*as.numeric(times(dia_25$hora_e_zona))>1080])

#dia 26 das 9h30(570) as 18h(1080)
a_remover <- c(a_remover, dia_26$id[60*24*as.numeric(times(dia_26$hora_e_zona))<570])
a_remover <- c(a_remover, dia_26$id[60*24*as.numeric(times(dia_26$hora_e_zona))>1080])

a_remover <- unique(a_remover)
a_remover

#REMOVER EMPREGADOS OUTLIERS

#dia_22
for(e in a_remover){
  dia_22 <- dia_22[!(dia_22$id==e),] 
}
#dia_23
for(e in a_remover){
  dia_23 <- dia_23[!(dia_23$id==e),] 
}
#dia_24
for(e in a_remover){
  dia_24 <- dia_24[!(dia_24$id==e),] 
}
#dia_25
for(e in a_remover){
  dia_25 <- dia_25[!(dia_25$id==e),] 
}
#dia_26
for(e in a_remover){
  dia_26 <- dia_26[!(dia_26$id==e),] 
}

#REMOVER TEMPOS FRACOS
#dia_22
#sub_zona(tudo)
dia_22 <- dia_22[!(dia_22$tempo_per<16),]
#zona
dia_22 <- dia_22[!(dia_22$tempo_per<31 & !(grepl("\\s[0-9]*$",dia_22$zona))),]

#dia_23
#sub_zona(tudo)
dia_23 <- dia_23[!(dia_23$tempo_per<16),]
#zona
dia_23 <- dia_23[!(dia_23$tempo_per<31 & !(grepl("\\s[0-9]*$",dia_23$zona))),]

#dia_24
#sub_zona(tudo)
dia_24 <- dia_24[!(dia_24$tempo_per<16),]
#zona4
dia_24 <- dia_24[!(dia_24$tempo_per<31 & !(grepl("\\s[0-9]*$",dia_24$zona))),]

#dia_25
#sub_zona(tudo)
dia_25 <- dia_25[!(dia_25$tempo_per<16),]
#zona
dia_25 <- dia_25[!(dia_25$tempo_per<31 & !(grepl("\\s[0-9]*$",dia_25$zona))),]

#dia_26
#sub_zona(tudo)
dia_26 <- dia_26[!(dia_26$tempo_per<16),]
#zona
dia_26 <- dia_26[!(dia_26$tempo_per<31 & !(grepl("\\s[0-9]*$",dia_26$zona))),]


#JUNTAR TUDO EM DATA E DATA_GSP
#$ character looks for the pattern at the end of the string
#[0-9]* looks for 1 or more digits
#\\s matches the whiteSpace

#DATA

#dia_22
data <- data.frame(dia_22$id, gsub("\\s[0-9]*$", "", dia_22$zona))
colnames(data) <- c("id","zona")

#dia_23
maximo<-max(data$id)
new <- data.frame(dia_23$id + maximo, gsub("\\s[0-9]*$", "", dia_23$zona))
colnames(new) <- c("id","zona")
data <- rbind(data,new)

#dia_24
maximo<-max(data$id)
new <- data.frame(dia_24$id + maximo, gsub("\\s[0-9]*$", "", dia_24$zona))
colnames(new) <- c("id","zona")
data <- rbind(data,new)

#dia_25
maximo<-max(data$id)
new <- data.frame(dia_25$id + maximo, gsub("\\s[0-9]*$", "", dia_25$zona))
colnames(new) <- c("id","zona")
data <- rbind(data,new)

#dia_26
maximo<-max(data$id)
new <- data.frame(dia_26$id + maximo, gsub("\\s[0-9]*$", "", dia_26$zona))
colnames(new) <- c("id","zona")
data <- rbind(data,new)

#valores unicos
data <- unique(data[c("id", "zona")])

###############################PARTE_2_REGRAS###################################
#regras
library(carenR)
rls<-caren(data,Bas=TRUE, min.sup = 0.035, min.conf = 0.8, imp = 0.05, chi = T) #min.sup=0.035 min.conf=0.80
#rls<-caren(data,Bas=TRUE)
ar.pp(rls)

ar.gplot(rls)

#ar.gplot
library(sna)
ar.gplot(rls)
#############################TERMINADO##########################################
#ANALISE DE REGRAS
#suporte
hist(rls$Sup, main="Distribution of Support", xlab="Support")
nrules<- apply(as.array(seq(0,1,0.05)),1,function(s)nrow(rls[rls$Sup>=s,]))
nrules
plot(seq(0,1,0.05),nrules,type='l',xlab='min-sup',ylab='nrules',
     main='Variation of nrules with min.sup')
#escolha 0.05
abline(v=0.05,col=4,lty=2)

#confianca
hist(rls$Conf, main="Distribution of Confidence", xlab="Confidence")
nrules_conf <- apply(as.array(seq(0,1,0.05)),1,function(c)nrow(rls[rls$Conf>=c,]))
plot(seq(0,1,0.05),nrules_conf,type='l',xlab='min-conf',ylab='nrules',
     main='Variation of nrules with min.conf')
#escolha 0.8
abline(v=0.8,col=4,lty=2)

#lift
nrules<-apply(as.array(seq(0.7,8.1,0.2)),1,function(l) nrow(rls[rls $Lift>=l,])) 
plot(seq(0.7,8.1,0.2),nrules,type='l',xlab='lift',ylab='nrules',main='Varia tion of nrules with lift')

#chi2
nrules<-apply(as.array(seq(0,61,0.2)), 1,function(chi) nrow(rls[rls $Chi2>=chi,])) 
plot(seq(0,61,0.2),nrules,type='l',xlab='ch i^2',ylab='nrules',main='Variation of nrules with chi^2')

# plot sup-conf
plot(rls$Conf,rls$Sup,col=as.numeric(rls$Cons),pch=15) 
legend(x='topleft',legend=levels(rls$Cons),col=1:2,pch=15)