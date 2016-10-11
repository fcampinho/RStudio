require("igraph")
#data.test<-read.table("jogar-tenis.csv", header=T, sep=";")

#> rep("Nao", 10)
# [1] "Nao" "Nao" "Nao" "Nao" "Nao" "Nao" "Nao" "Nao" "Nao" "Nao"
#> entropia(rep("Nao", 10))
#[1] 0
#> exemplo<-c(rep("Nao", 5),rep("Sim", 5))
#> exemplo<-exemplo[shuffle(length(exemplo))]
#> entropia(exemplo)
#[1] 1
#> entropia(data.test$Jogar.Tênis)
#[1] 0.940286

#testes com id3
#data<-read.table("../../dataset/jogar-tenis.csv", header=T, sep=";")
#resultado<-runID3(data[,2:5], data[,6])
#plotID3(resultado)


entropia<-function(data){
	resp<-data/sum(data)
	resp<-sum(resp*log2(resp))*(-1)
	resp
}

ga<-function(data, classe){
	niveis<-levels(data)

	resp<-c()

	for(i in 1:length(niveis)){
		sv<-table(classe[which(data==niveis[i])])
		sv<-subset(sv, sv!=0)
		if(length(sv)>0)
			resp[i]<-entropia(sv)*(sum(sv)/length(data))
	}

	resp<-entropia(table(classe))-sum(resp)
	resp
}

runID3<-function(data,classe){
	nos<-list()

	rotulos<-names(data)
	remove.rotulos<-c()
	classe<-factor(classe)

	if(length(levels(classe))<2){
		nos$id=levels(classe)[1]
		nos$arestas<-NA
		nos$filhos<-NA
		return(nos)
	}

	nos.ga<-c()
	for(i in 1:ncol(data)){
		nos.ga<-c(nos.ga, ga(data[,i], classe))
	}

	max.ga<-which.max(nos.ga)
	nos$id<-names(data)[max.ga]
	nos$arestas<-levels(data[,max.ga])
	nos$filhos<-list()


	remove.rotulos<-c(nos$id)


	for(i in 1:length(nos$arestas)){
		index<-which(data[[max.ga]]==nos$arestas[i])
		nos$filhos[[i]]=runID3(data[index,-which(rotulos%in%remove.rotulos)],factor(classe[index]))
	}

	nos

}

createTree<-function(data, id=0){
	resp<-list()
	resp$edges<-c()
	resp$labels<-c()
	resp$last<-id
	resp$connections<-c()

	if(is.na(data$aresta[1])){
		resp$node<-data$id
		resp$last<-id+1
		return(resp)
	}

	resp$node<-c(data$id)
	resp$last<-id+1

	mid<-resp$last
	if(length(data$aresta)>0){
		for(i in 1:length(data$aresta)){
			resp$connections<-c(resp$connections, c(mid,resp$last+1))
			result<-createTree(data$filhos[[i]], resp$last)
			resp$last<-result$last
			resp$node<-c(resp$node, result$node)
			resp$labels<-c(resp$labels, data$aresta[i], result$labels)
			resp$edges<-c(resp$edges, c(data$id,data$filhos[[i]]$id), result$edges)
			resp$connections<-c(resp$connections, result$connections)
		}
	}

	return (resp)
}

plotID3<-function(data, interativo=F){
	result<-createTree(data)
	g<-make_empty_graph(n = length(result$node))
	g<-add_edges(g,result$connections)
	V(g)$label<-result$node
	E(g)$label<-result$labels
	if(interativo){
		tkplot(g)
	}else{
		plot(g,layout = layout.reingold.tilford(g, root=1))
	}
}


