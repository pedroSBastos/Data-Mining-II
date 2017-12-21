
#dia_22
data_gsp_aux <- data.frame(dia_22$id[grepl("\\s[0-9]*$",dia_22$zona)],dia_22$zona[grepl("\\s[0-9]*$",dia_22$zona)],dia_22$hora_e_zona[grepl("\\s[0-9]*$",dia_22$zona)])
colnames(data_gsp_aux) <- c("id","zona","tempo")

#dia_23
new <- NULL
maximo<-max(data_gsp_aux$id)
new <- data.frame(dia_23$id[grepl("\\s[0-9]*$",dia_23$zona)] + maximo,dia_23$zona[grepl("\\s[0-9]*$",dia_23$zona)],dia_23$hora_e_zona[grepl("\\s[0-9]*$",dia_23$zona)])
colnames(new) <- c("id","zona","tempo")
data_gsp_aux <- rbind(data_gsp_aux,new)

#dia_24
new <- NULL
maximo<-max(data_gsp_aux$id)
new <- data.frame(dia_24$id[grepl("\\s[0-9]*$",dia_24$zona)] + maximo,dia_24$zona[grepl("\\s[0-9]*$",dia_24$zona)],dia_24$hora_e_zona[grepl("\\s[0-9]*$",dia_24$zona)])
colnames(new) <- c("id","zona","tempo")
data_gsp_aux <- rbind(data_gsp_aux,new)

#dia_25
new <- NULL
maximo<-max(data_gsp_aux$id)
new <- data.frame(dia_25$id[grepl("\\s[0-9]*$",dia_25$zona)] + maximo,dia_25$zona[grepl("\\s[0-9]*$",dia_25$zona)],dia_25$hora_e_zona[grepl("\\s[0-9]*$",dia_25$zona)])
colnames(new) <- c("id","zona","tempo")
data_gsp_aux <- rbind(data_gsp_aux,new)

#dia_26
new <- NULL
maximo<-max(data_gsp_aux$id)
new <- data.frame(dia_26$id[grepl("\\s[0-9]*$",dia_26$zona)] + maximo,dia_26$zona[grepl("\\s[0-9]*$",dia_26$zona)],dia_26$hora_e_zona[grepl("\\s[0-9]*$",dia_26$zona)])
colnames(new) <- c("id","zona","tempo")
data_gsp_aux <- rbind(data_gsp_aux,new)

#head function
strhead <- function(s,n) {
  if(n<0) 
    substr(s,1,nchar(s)+n) 
  else 
    substr(s,1,n)
}

library(chron)
data_gsp_aux <- data_gsp_aux[order(data_gsp_aux$id,strhead(as.character(data_gsp_aux$zona),-1),60*24*as.numeric(times(data_gsp_aux$tempo)),decreasing=F),]

data_gsp <- data.frame(data_gsp_aux$id,data_gsp_aux$zona)
colnames(data_gsp) <- c("id","zona")

#tail function
strtail <- function(s,n=1) {
  if(n<0) 
    substring(s,1-n) 
  else 
    substring(s,nchar(s)-n+1)
}

#zonas com mais de duas sub-zonas
###################################Correr_a_1_vez################################
zonas_de_interesse <- c()
for(z in unique(data_gsp$zona)){
  if(as.numeric(strtail(z,1))>2){
    zonas_de_interesse <- c(zonas_de_interesse,z)
  }
}

zonas_de_interesse <- strhead(zonas_de_interesse,-1)
zonas_de_interesse <- unique(zonas_de_interesse)

for(z in zonas_de_interesse){
  data_gsp <- data_gsp[!(grepl(z,data_gsp$zona)),] 
}

zonas_de_interesse <- data_gsp$zona
zonas_de_interesse <- strhead(as.character(zonas_de_interesse),-1)
zonas_de_interesse <- unique(zonas_de_interesse)
###################################Correr_a_1_vez################################

###################################Correr_a_2_vez################################

for(z in zonas_de_interesse){
  data_gsp <- data_gsp[!(grepl(z,data_gsp$zona)),] 
}
###################################Correr_a_2_vez################################

#SEPARACAO POR SUB-ZONA
#Caracole
data_gsp_caracole <- data_gsp
data_gsp_caracole <- data.frame(data_gsp_caracole$id[grepl("Caracole",data_gsp_caracole$zona)],data_gsp_caracole$zona[grepl("Caracole",data_gsp_caracole$zona)])
colnames(data_gsp_caracole) <- c("id","zona")

write.csv(data_gsp_caracole,file = "data_gsp_caracole.csv")

#Benhardt
data_gsp_benhardt <- data_gsp
data_gsp_benhardt <- data.frame(data_gsp_benhardt$id[grepl("Benhardt",data_gsp_benhardt$zona)],data_gsp_benhardt$zona[grepl("Benhardt",data_gsp_benhardt$zona)])
colnames(data_gsp_benhardt) <- c("id","zona")

write.csv(data_gsp_benhardt,file = "data_gsp_benhardt.csv")

#Van Thield
data_gsp_van_thield <- data_gsp
data_gsp_van_thield<- data.frame(data_gsp_van_thield$id[grepl("Van",data_gsp_van_thield$zona)],data_gsp_van_thield$zona[grepl("Van",data_gsp_van_thield$zona)])
colnames(data_gsp_van_thield) <- c("id","zona")

write.csv(data_gsp_van_thield,file = "data_gsp_van_thield.csv")

#Fendi Home
data_gsp_fendi_home <- data_gsp
data_gsp_fendi_home <- data.frame(data_gsp_fendi_home$id[grepl("Fendi",data_gsp_fendi_home$zona)],data_gsp_fendi_home$zona[grepl("Fendi",data_gsp_fendi_home$zona)])
colnames(data_gsp_fendi_home) <- c("id","zona")

write.csv(data_gsp_fendi_home,file = "data_gsp_fendi_home.csv")

#Kitchen
data_gsp_kitchen <- data_gsp
data_gsp_kitchen <- data.frame(data_gsp_kitchen$id[grepl("Kitchen",data_gsp_kitchen$zona)],data_gsp_kitchen$zona[grepl("Kitchen",data_gsp_kitchen$zona)])
colnames(data_gsp_kitchen) <- c("id","zona")

write.csv(data_gsp_kitchen,file = "data_gsp_kitchen.csv")

#Textile
data_gsp_textile <- data_gsp
data_gsp_textile <- data.frame(data_gsp_textile$id[grepl("Textile",data_gsp_textile$zona)],data_gsp_textile$zona[grepl("Textile",data_gsp_textile$zona)])
colnames(data_gsp_textile) <- c("id","zona")

write.csv(data_gsp_textile,file = "data_gsp_textile.csv")