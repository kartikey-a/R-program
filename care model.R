N=10
gen=100
hi_f=0.5
r2_size=0.5*N

care_i=0.5 #Initial caring probs
w=5 #
w2=10

V0 = 0.1 #0 caring parents
V1 = 0.5 #1 caring parent
V2 = 1

hi_g = 0.3
lo_g = 0.6

qlty=rep.int(0,N)
for(i in 1:N){qlty[i]=sample(c(hi_g,lo_g),1)}

high_pop =data.frame(id= 1:(N*hi_f), #Unique identification
                  care=rep(care_i), #Care prob.
                  quality=rep(hi_g),
                  index=rep(1),
                  r2=rep(0),
                  fit=rep(0)) #Fitness

low_pop=data.frame(id= 1:(N*(1-hi_f)), #Unique identification
                   care=rep(care_i), #Care prob.
                   quality=rep(lo_g),
                   index=rep(1),
                   r2=rep(0),
                   fit=rep(0))

pop=rbind(high_pop,low_pop)

for (j in 1:gen){
  sum_r1=sum(pop$index)
  while(sum_r1>0){ #Round 1 loop
    
    #Sampling the pair
    k=sample(pop$id,size=2, prob =pop$index, replace = FALSE)
    
    #Generating relevant variables
    i1<-=k[1]
    i2<-=k[2]
    pop$care[i1]=c1
    pop$care[i2]=c2
    
    #Updating Fitnesses
    pop$fit[i1]=pop$fit[i1]+(w*V2*c1*c2 + w*V1*(1-c1)*c2 + w*V1*c1*(1-c2) + w*V0*(1-c1)*(1-c2))
    pop$fit[i2]=pop$fit[i2]+(w*V2*c1*c2 + w*V1*(1-c1)*c2 + w*V1*c1*(1-c2) + w*V0*(1-c1)*(1-c2))
    
    #Updating Indices- Removed from Round 1
    pop$index[i1]=0
    pop$index[i2]=0
    sum_r1=sum(pop$index)
    
    #Probabilties of getting to Round 2 (Prob of not caring [inherited] x Prob of not dying[based on quality])
    pop$r2[i1]=pop$qlty[i1]*(1-c1)
    pop$r2[i2]=pop$qlty[i2]*(1-c2)
    
  }
  #Round 2
  
  r2_ind= sample(pop$id,r2_size,prob=pop$r2) #The best fraction of population gets to participate in R2
  r2_dat= data.frame(id=r2_ind,
                     index=rep(1))
  r2_sum=sum(r2_dat$index)
  
  while(r2_sum>0){ #Round 2 loop
    
    #Sampling the pair
    k=sample(r2_ind,size=2, prob =r2_dat$index, replace = FALSE)
    
    #Generating relevant variables
    i1<-=k[1]
    i2<-=k[2]
    pop$care[i1]=c1
    pop$care[i2]=c2
    
    #Updating Fitnesses
    pop$fit[i1]=pop$fit[i1]+(w2*V0*(1-c1)*(1-c2))
    pop$fit[i2]=pop$fit[i2]+(w2*V0*(1-c1)*(1-c2))
    
    #Updating Indices- Removed from Round 2
    r2_dat$index[i1]=0
    r2_dat$index[i2]=0
    r2_sum=sum(r2_dat$index)
  } #One gen ends
  #Next Gen sampling
  
} #Gen loop ends


