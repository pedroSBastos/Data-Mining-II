dados <- read.csv("~/Dropbox/D.M II/Trabalho_2-CF/playlist_pre2-train.bas", sep = " ")

#SESSIONID sempre igual a USERID. Para verificar:
which(dados$SESSIONID!=dados$USERID)

library("recommenderlab")

#USERID e REQUESTEDURL
id_request <- dados[,c(2,4)]

#matriz de ratings
t_id_request <- table(id_request$USERID, id_request$REQUESTEDURL)
cn <- colnames(t_id_request)
m_id_request <- matrix(t_id_request, nrow(t_id_request), ncol(t_id_request))
colnames(m_id_request) <- cn

#matriz binaria
mb_id_request <- as(m_id_request,"binaryRatingMatrix")

######################################1##########################################
#numero de ocorrecias minimo de id
id_freq <- NULL
new <- NULL
for (i in unique(dados$USERID)){
  new <- data.frame(i,length(which(dados$USERID==i)))
  id_freq <- rbind(id_freq,new)
}
min(id_freq) # no minimo ouvio 2 musicas given=2

#avaliacao
scheme <- evaluationScheme(mb_id_request, method = "cross-validation", k=10, given=2)

algorithms <-list(
  Popular=list(name="POPULAR",param=NULL),
  UBased=list(name="UBCF", param=list(method = "Jaccard", nn = 40)),
  IBased=list(name="IBCF", param=list(method = "Jaccard", k = 40)),
  ARules=list(name = "AR",param = list(support = 0.03, confidence = 0.3, maxlen = 3))
) 

evlist <- evaluate(scheme, algorithms, n = c(1,2,3,4,5,6,7,8,9,10))
plot(evlist, annotate = c(1, 4), legend = "bottomright")

#UBCF
algorithmsUBCF <-list(
  UBCF20=list(name="UBCF", param=list(method = "Jaccard", nn = 20)),
  UBCF40=list(name="UBCF", param=list(method = "Jaccard", nn = 40)),
  UBCF60=list(name="UBCF", param=list(method = "Jaccard", nn = 60)),
  UBCF80=list(name="UBCF", param=list(method = "Jaccard", nn = 80)),
  UBCF100=list(name="UBCF", param=list(method = "Jaccard", nn = 100))
)

#avaliacao
evlist <- evaluate(scheme, algorithmsUBCF, n = c(1,2,3,4,5,6,7,8,9,10))
plot(evlist, annotate = T, legend = "bottomright")

#################################### 1 ##########################################
#################################### 2 ##########################################
obs <- read.csv("~/Dropbox/D.M II/Trabalho_2-CF/plylist_pre2_obs_ordered.csv", sep = " ")

id_request_obs <- obs[,c(2,4)]

nomes<-colnames(mb_id_request) 
levels(id_request_obs$REQUESTEDURL)<-nomes #igualar o numero de colunas

#matriz de ratings
t_id_request_obs <- table(id_request_obs$USERID, id_request_obs$REQUESTEDURL)
cn_obs <- colnames(t_id_request_obs)
m_id_request_obs <- matrix(t_id_request_obs, nrow(t_id_request_obs), ncol(t_id_request_obs))
colnames(m_id_request_obs) <- cn_obs

#binary matrix
mb_id_request_obs <- as(m_id_request_obs,"binaryRatingMatrix")

recUB <- Recommender(mb_id_request, method = "UBCF")

#Recomendacoes
recUB10 <- predict(recUB, mb_id_request_obs, method = "Jaccard", nn=100 ,n = 10) #10 recomendacoes

#formato pretendido
top10 <- as(recUB10, "list")
top_ids <- data.frame(matrix(unlist(top10), nrow=length(top10), byrow=T))
to_text <- paste(top_ids$X1,top_ids$X2,top_ids$X3,top_ids$X4,top_ids$X5,top_ids$X6,top_ids$X7,top_ids$X8,top_ids$X9,top_ids$X10, sep=",")
top_ids <- data.frame(unique(obs$USERID),to_text)
colnames(top_ids) <- c("USERID","REQUESTEDURL")

write.table(top_ids, file="PedroJuliana_recommendations.csv", row.names=FALSE, sep=",")

##################################2##############################################
#NA