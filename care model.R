line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
  switch(side,
         `1` = grconvertY(-line * y_off, 'npc', 'user'),
         `2` = grconvertX(-line * x_off, 'npc', 'user'),
         `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
         `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
}
N=1000 #Pop Size
gen=10000 #No. of generations (although not discrete)


care_h=4
care_l=5
w=5 #Size of clutch

hi_g = 0.000006
lo_g = 0.000014

hbase=0.001 #Base death rates
lbase=0.01

randmut =0.001 #0.1% 
std_dev=0.3

step=10
qlty=sample(c(hi_g,lo_g),N,replace = TRUE)

pop=data.frame(id= 1:N, #Unique identification
               careh= pmax(rnorm(N,care_h),0),
               carel= pmax(rnorm(N,care_l),0), #Care prob.
               quality=qlty,
               index=rep(1),
               alive=rep(1),
               fit=rep(0),
               fitx=rep(0))
  
finalfith=vector("list")
finalcareh=vector("list")
  
finalfitl=vector("list")
finalcarel=vector("list")
  
for (o in 1:gen){  
  sum_r1=sum(pop$index)
  while(sum_r1>0){
    
    #Sampling the pair
    k=sample(pop$id,size=2, prob=pop$index, replace = FALSE)
    
    #Generating relevant variables
    i1<-k[1]
    i2<-k[2]
   
    g1=pop$quality[i1]
    g2=pop$quality[i2]
    
    if(g1==hi_g){
      c1=min(pop$careh[i1],sqrt((1-hbase)/g1))
      drate_1=min(hbase+(g1*c1*c1),1)
    }
   else{
     c1=min(pop$carel[i1],sqrt((1-lbase)/g1))
     drate_1=min(lbase+(g1*c1*c1),1)
   }
    if(g2==hi_g){
      c2=min(pop$careh[i2],sqrt((1-hbase)/g2))
      drate_2=min(hbase+(g2*c2*c2),1)
    }
    else{
      c2=min(pop$carel[i2],sqrt((1-lbase)/g2))
      drate_2=min(lbase+g2*c2*c2,1)
    }
    #Updating Fitnesses
    thisfit=w*(1-exp((-0.01*((c1+c2)/w))-100))
    pop$fitx[i1]=pop$fitx[i2]=thisfit
    
    pop$fit[i1]=pop$fit[i1]+thisfit
    pop$fit[i2]=pop$fit[i2]+thisfit
    
    #Updating Indices- Removed from Round 1
    pop$index[i1]=0
    pop$index[i2]=0
    sum_r1=sum(pop$index)
    
   #Alive?
    pop$alive[i1]=sample(c(0,1),1,prob=c(drate_1,1-drate_1))
    pop$alive[i2]=sample(c(0,1),1,prob=c(drate_2,1-drate_2))
  }
  alives=split(pop,pop$alive)[["1"]]
  alives$index=rep(1)
  
  deads=split(pop,pop$alive)[["0"]]
  
  if (length(deads$id)==0){
    pop=alives
  }
  else{
    finalfith=c(as.numeric(finalfith),split(deads,deads$quality)[[as.character(hi_g)]]$fit)
    finalcareh=c(as.numeric(finalcareh),split(deads,deads$quality)[[as.character(hi_g)]]$careh)
    
    finalfitl=c(as.numeric(finalfitl),split(deads,deads$quality)[[as.character(lo_g)]]$fit)
    finalcarel=c(as.numeric(finalcarel),split(deads,deads$quality)[[as.character(lo_g)]]$carel)
    
    NN=nrow(deads)
    offspring= sample(pop$id,NN,prob = pop$fitx, replace = TRUE)
   
    careh_new= pop$careh[offspring]
    carel_new=pop$carel[offspring]
    
    deads$careh=pmax(careh_new + rnorm(NN,0,std_dev),0)
    deads$carel=pmax(carel_new + rnorm(NN,0,std_dev),0)
    
    deads$fit=deads$fitx=rep(0)
    deads$index=deads$alive=rep(1)
    
    deads$quality=sample(c(hi_g,lo_g),NN,replace = TRUE)
    
    pop=rbind(alives,deads) #New population
    pop=pop[order(pop$id),]
  }
  if (o %% 100==0){ 
    print(o)
    print(c(mean(pop$careh),mean(pop$carel)))
    a1=a2=a3=a4=c()
    for (a in 1:(length(finalcareh)%/%step)){
      a1=c(a1,mean(finalcareh[((a-1)*step+1):(a*step)]))
    }
    for (b in 1:(length(finalcarel)%/%step)){
      a2=c(a2,mean(finalcarel[((b-1)*step+1):(b*step)]))
    }
    for (c in 1:(length(finalfith)%/%step)){
      a3=c(a3,mean(finalfith[((c-1)*step+1):(c*step)]))
    }
    for (d in 1:(length(finalfitl)%/%step)){
      a4=c(a4,mean(finalfitl[((d-1)*step+1):(d*step)]))
    }
    par(mfrow=c(2,2))
    plot(seq(length(a3)),a3,type = "l",xlab="Avg Estimate of Gens", ylab="Final fitnesses")#,ylim =c(0,20))
    plot(seq(length(a1)),a1,type = "l",xlab="Avg Estimate of Gens", ylab="Final Cares(H)")#,ylim=c(10,60))
    text(line2user(line=mean(par('mar')[c(2, 4)]), side=2), 
         line2user(line=2, side=3), paste('High Quality Fitness and Care','Gen =',o), xpd=NA, cex=2, font=2)
    plot(seq(length(a4)),a4,type = "l",xlab="Avg Estimate of Gens", ylab="Final fitnesses")#,ylim =c(0,20))
    plot(seq(length(a2)),a2,type = "l",xlab="Avg Estimate of Gens", ylab="Final Cares(L)")#,ylim=c(10,60))
    text(line2user(line=mean(par('mar')[c(2, 4)]), side=2), 
         line2user(line=2, side=3), paste('Low Quality Fitness and Care','Gen =',o), xpd=NA, cex=2, font=2)
  }
}
