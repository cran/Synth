collect.optimx <-
function(res,opt="min"){ 
# opt can be "min" or "max"
# number of methods
reslist <- attr(res, "details")
m       <- length(reslist)
pars    <- length(reslist[[1]]$par)

store   <- matrix(NA,ncol=pars+1,nrow=m)
colnames(store) <- c(paste("par",1:pars,sep=""),"fvalues")
for(i in 1:m){
store[i,1:pars] <- reslist[[i]]$par
if(is.null(reslist[[i]]$value)){reslist[[i]]$value <- NA}
store[i,"fvalues"] <- reslist[[i]]$value
}
out.list    <- data.frame(store)
if(opt=="max"){
optm <- which(out.list$fvalues==max(out.list$fvalues,na.rm=TRUE))
}
if(opt=="min"){
optm <- which(out.list$fvalues==min(out.list$fvalues,na.rm=TRUE))
}
opt.par    <- out.list[optm,1:pars]
opt.fvalue <- out.list[optm,"fvalues"]
out <- list(out.list=out.list,par=opt.par,value=opt.fvalue)
return(out)
}

