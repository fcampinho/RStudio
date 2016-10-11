activation.func<-function(x, threshold){
	if(x > threshold)
		return (1)
	return (0)
}

perceptron<-function(train.set=NA, label.set=NA, threshold=0.5, iter=100, alpha=0.1){
	if(is.na(train.set) && is.na(label.set)){
		stop("Train and/or label undefined")
	}

	bias = 0
	#bias = runif(min=-1, max=1, n=1)
	w = rep(0, ncol(train.set))
	#w = runif(min=-1, max=1, n=ncol(train.set))
	error = 0
	counter=1

	for(i in 1:iter){
		cat("***Running iteration: ", i, "\n")
		j<-floor(runif(1,min=1, max=nrow(train.set)+1)) # escolhe um exemplo aleatorio...
		y<-activation.func(sum(w*train.set[j,])+bias, threshold)	
		w<-w + alpha*(label.set[j]-y)*train.set[j,]
		bias<-bias+alpha*(label.set[j]-y)
		error<-(error+abs(label.set[j]-y))/counter
		counter<-counter+1
	}

	result<-list()
	result$w = w
	result$bias = bias
	result$error = error

	result
}

test<-function(model, data, threshold){
	y<-activation.func(sum(model$w*data)+model$bias,threshold)
	y
}

#exemplo
#data<-read.csv("data.csv", header=F, sep=";")
#teste<-read.csv("teste.csv", header=F, sep=";")
#model=perceptron(data[,c(1,2,3)], data[,4], iter=100)
#test(model, teste[1,c(1,2,3)], 0.5)
#test(model, teste[2,c(1,2,3)], 0.5)
#test(model, teste[3,c(1,2,3)], 0.5)
#test(model, teste[4,c(1,2,3)], 0.5)
#test(model, teste[5,c(1,2,3)], 0.5)
