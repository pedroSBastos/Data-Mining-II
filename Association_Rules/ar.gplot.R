###########################################
#
# Plotting the AR as a graph
#
# as used initially for PortalExecutivo
#
###########################################

# required package sna

ar.gplot <- function(rls,min.conf=0.5, as.tree=T, layout.hor=T) {
	x<-ar.dat(rls,min.conf)
	s<-tree.node.placing.structure(x)
	if(as.tree) coord<-tree.node.coord(s,layout.hor) else coord<-NULL
	gplot(x,label=rownames(x),label.cex=0.5,displayisolates=F,mode='circle',
		coord=coord[rownames(x),],
		pad=0.1,edge.lwd=1,vertex.cex=1,gmode='bimodal',edge.col='red',vertex.col='black',
		jitter=F)
}

ar.dat <- function(rls,min.conf=0.5){
	ants<-NULL
	conss<-NULL
	confs<-NULL
	rls<-rls[rls$Conf>=min.conf,]
	for(i in 1:nrow(rls)) {
		#ants[i] <-paste(unlist(rls$Ant[i]),collapse=' & ')
		#TODO teste de character(0) menos bronco
		if(as.character(rls$Ant[i])=="character(0)") ants[i]<-"_void_"
		else ants[i] <-paste(unlist(rls$Ant[i]),collapse=' & ')
		conss[i]<-as.character(rls$Cons[i])
		confs[i]<-rls$Conf[i]
		}
	df<-data.frame(o=ants,d=conss,w=confs)
	labels<-unique(union(ants,conss))
	n<-length(labels)
	dat<-matrix(0,n,n,dimnames=list(labels,labels))
	#rownames(dat)<-labels
	#colnames(dat)<-labels
	for(i in 1:nrow(rls)) dat[as.character(df$o[i]),as.character(df$d[i])]<-df$w[i]
	dat
}

oddity.ar.dat <- function(rls,min.conf=0.5){
ants<-NULL
conss<-NULL
confs<-NULL
for(i in 1:nrow(rls)) {
	ants[i] <-paste(unlist(rls$Ant[i]),collapse=' & ')
	conss[i]<-paste(rls$Cons[i])
	confs[i]<-(if(rls$Conf[i]>=min.conf) rls$Conf[i] else 0)
	}
df<-data.frame(o=ants,d=conss,w=confs)
labels<-unique(union(ants,conss))
n<-length(labels)
dat<-matrix(0,n,n,dimnames=list(labels,labels))
#rownames(dat)<-labels
#colnames(dat)<-labels
for(i in 1:nrow(rls)) if(!is.null(df$o[i])) dat[df$o[i],df$d[i]]<-df$w[i]
dat
}

tree.node.placing.structure <- function(d) {
level<-1
nodes<-NULL
levels<-NULL
parents.strings<-NULL
sums<-apply(d,1,sum)
parents.level<-names(sums[sums==0])
nodes<-sort(parents.level)
levels<-rep(level,length(nodes))
parents.strings<-nodes
expanded<-rep(FALSE,length(nodes))
struct <- data.frame(node=nodes,level=levels,parents=parents.strings,expanded=expanded)
while(any(!struct$expanded)) {
	level<-level+1
	for(p in as.character(struct$node[!struct$expanded])) {
		nodes<-sort(setdiff(names(d[,p][d[,p]>0]),struct$node))
		if(length(nodes)!=0) {
			levels<-rep(level,length(nodes))
			parents.strings<-apply(as.array(nodes),1,f<-function(y){paste(sort(names(d[y,][d[y,]>0])),collapse='+')})
			expanded<-rep(FALSE,length(nodes))
			more.struct <- data.frame(node=nodes,level=levels,parents=parents.strings,expanded=expanded)
			struct<-rbind(struct,more.struct)
			}
		struct$expanded[struct$node==p]<-T
		}
	}
# handle remainers, which belong to cycles
rem<-setdiff(as.character(rownames(d)),struct$node)
n.rem<-length(rem)
if(n.rem!=0) {
	nodes<-rem
	parents.strings<-rep("",n.rem)
	levels<-rep(level,n.rem)
	expanded<-rep(TRUE,n.rem)
	more.struct <- data.frame(node=nodes,level=levels,parents=parents.strings,expanded=expanded)
	struct<-rbind(struct,more.struct)
	}
struct[,c('node','level','parents')]
}

tree.node.coord <- function(s,layout.hor=T) { # s as given by tree.node.placing.structure
H<-10
names<-NULL
x<-NULL
y<-NULL
xc<-0
for(lv in 1:max(s$level)) {
	nodes<-as.character(s$node[s$level==lv])
	n.nodes<-length(nodes)
# pode-se colocar opcao curved.level
	xnew <- rep(xc,n.nodes)
#	if(n.nodes==1) xnew<-xc-0.2
#	else xnew <- xc-(1-((n.nodes:1)-((n.nodes+1)/2))^2/((n.nodes-1)^2/4))*0.2
	xc<-xc-1
	ynew <-((1:n.nodes)*H/n.nodes)-(0.5*H)/(n.nodes)
	names<-c(names,nodes)
	x<-c(x,xnew)
	y<-c(y,ynew)
	}
if(layout.hor)
	data.frame(x=x,y=y,row.names=names)
else
	data.frame(x=y,y=x,row.names=names)

}

# stolen from SNA

mygplot <-
function (dat, g = 1, gmode = "digraph", diag = FALSE, label = c(1:dim(dat)[2]), 
    coord = NULL, jitter = TRUE, thresh = 0, usearrows = TRUE, 
    mode = "mds", displayisolates = TRUE, pad = 0, vertex.pch = 19, 
    label.cex = 1, vertex.cex = 1, label.col = 1, edge.col = 1, 
    vertex.col = 1, arrowhead.length = 0.2, edge.type = 1, edge.lwd = 0, 
    ...) 
{
    if (length(dim(dat)) > 2) 
        d <- dat[g, , ]
    else d <- dat
    if (gmode == "graph") {
        usearrows <- FALSE
        n <- dim(d)[1]
    }
    else if (gmode == "twomode") {
        n <- sum(dim(d))
        temp <- matrix(0, nrow = n, ncol = n)
        temp[1:dim(d)[1], (dim(d)[1] + 1):n] <- d
        d <- temp
        if (all(label == 1:dim(dat)[2])) 
            label <- 1:n
    }
    else n <- dim(d)[1]
    d[is.na(d)] <- 0
    if (!is.null(coord)) {
        x <- coord[, 1]
        y <- coord[, 2]
    }
    else if (mode == "princoord") {
        cd <- cor(rbind(d, t(d)), use = "pairwise.complete.obs")
        cd <- replace(cd, is.na(cd), 0)
        e <- eigen(cd, symmetric = TRUE)
        x <- Re(e$vectors[, 1])
        y <- Re(e$vectors[, 2])
    }
    else if (mode == "eigen") {
        e <- eigen(d, symmetric = (gmode != "digraph"))
        x <- Re(e$vectors[, 1])
        y <- Re(e$vectors[, 2])
    }
    else if (mode == "mds") {
        Dmat <- matrix(nrow = n, ncol = n)
        diag(Dmat) <- 0
        for (i in 1:n) for (j in 1:n) if (i > j) 
            Dmat[i, j] <- sqrt(sum(abs(d[i, ] - d[j, ])) + sum(abs(d[, 
                i] - d[, j])))
        Dmat[upper.tri(Dmat)] <- t(Dmat)[upper.tri(Dmat)]
        Imat <- matrix(nrow = n, ncol = n)
        Imat[, ] <- 0
        diag(Imat) <- 1
        Hmat <- Imat - (1/n) * rep(1, n) %*% t(rep(1, n))
        Amat <- -0.5 * Dmat^2
        Bmat <- Hmat %*% Amat %*% Hmat
        e <- eigen(Bmat)
        x <- Re(e$vectors[, 1])
        y <- Re(e$vectors[, 2])
    }
    else if (mode == "random") {
        x <- runif(n, -1, 1)
        y <- runif(n, -1, 1)
    }
    else if (mode == "circle") {
        x <- sin(2 * pi * ((0:(n - 1))/n))
        y <- cos(2 * pi * ((0:(n - 1))/n))
    }
    else if (mode == "circrand") {
        tempd <- rnorm(n, 1, 0.25)
        tempa <- runif(n, 0, 2 * pi)
        x <- tempd * sin(tempa)
        y <- tempd * cos(tempa)
    }
    else if (mode == "rmds") {
        require(mva)
        tempmds <- cmdscale(dist(d))
        x <- tempmds[, 1]
        y <- tempmds[, 2]
    }
    else if (mode == "geodist") {
        require(mva)
        tempmds <- cmdscale(as.dist(geodist(d)$gdist))
        x <- tempmds[, 1]
        y <- tempmds[, 2]
    }
    else if (mode == "adj") {
        require(mva)
        tempmds <- cmdscale(as.dist(-d + max(d)))
        x <- tempmds[, 1]
        y <- tempmds[, 2]
    }
    else if (mode == "seham") {
        require(mva)
        temp <- sedist(d)
        tempmds <- cmdscale(as.dist(temp))
        x <- tempmds[, 1]
        y <- tempmds[, 2]
    }
    if (jitter) {
        x <- jitter(x)
        y <- jitter(y)
    }
    use <- displayisolates | (!is.isolate(d, ego = 1:dim(d)[1]))
    if ((length(x) > 0) & (!all(use == FALSE))) 
        plot(x[use], y[use], xlim = c(min(x[use]) - pad, max(x[use]) + 
            pad), ylim = c(min(y[use]) - pad, max(y[use]) + pad), 
            type = "p", pch = vertex.pch, xlab = expression(lambda[1]), 
            ylab = expression(lambda[2]), col = vertex.col, cex = vertex.cex, 
            ...)
    else plot(0, 0, type = "n", pch = vertex.pch, xlab = expression(lambda[1]), 
        ylab = expression(lambda[2]), col = vertex.col, cex = vertex.cex, 
        ...)
    px0 <- vector()
    py0 <- vector()
    px1 <- vector()
    py1 <- vector()
    e.lwd <- vector()
    for (i in c(1:n)[use]) for (j in c(1:n)[use]) if ((i != j) & 
        (d[i, j] > thresh)) {
        px0 <- c(px0, as.real(x[i]))
        py0 <- c(py0, as.real(y[i]))
        px1 <- c(px1, as.real(x[j]))
        py1 <- c(py1, as.real(y[j]))
        if (edge.lwd > 0) 
            e.lwd <- c(e.lwd, edge.lwd * d[i, j])
        else e.lwd <- c(e.lwd, 1)
    }
    if (usearrows & (length(px0) > 0)) 
        arrows(as.vector(px0), as.vector(py0), as.vector(px1), 
            as.vector(py1), length = arrowhead.length, angle = 15, 
            col = edge.col, lty = edge.type, lwd = e.lwd)
    else if (length(px0) > 0) 
        segments(as.vector(px0), as.vector(py0), as.vector(px1), 
            as.vector(py1), col = edge.col, lty = edge.type, 
            lwd = e.lwd)
    if ((length(label) > 0) & (!all(use == FALSE))) 
        text(x[use], y[use], label[use], pos = 1, cex = label.cex, 
            col = label.col)
}

is.isolate <- function (dat, ego, g = 1, diag = FALSE) 
{
	dat <- as.sociomatrix.sna(dat)
	if (is.list(dat)) 
		return(is.isolate(dat[[g]], ego = ego, g = 1, diag = diag))
	if (length(dim(dat)) > 2) 
		d <- dat[g, , ]
	else d <- dat
	if (!diag) 
		diag(d) <- NA
	o <- vector()
	for (i in 1:length(ego)) o <- c(o, all(is.na(d[ego[i], ]) | 
										(d[ego[i], ] == 0)) & all(is.na(d[, ego[i]]) | (d[, ego[i]] == 
											0)))
	o
}
