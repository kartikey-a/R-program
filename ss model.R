#1. Parameters

N   = 1000 #No. of individuals of every sex in the popln
X   = 0.4 #Fraction of males which are sexually selected
Ns  = N*X #Sexually selected males
Nn  = N-Ns #Non-sexually selected males
gen = 500 #No. of generations


Wc = 3 #Eggs layed by caring females
Wd = 5 #Eggs layed by a deserting female
V0 = 0.1 #0 caring parents
V1 = 0.5 #1 caring parent
V2 = 1 #Both caring parents
p = 0.4#Paternity


sd_i=0.5 #Initial desertion prob. for SSMs
nd_i=0.5 #For NSSMs
fd_i=0.5 #For females
alpha=0.2  #Mating prob. for NSSMs

randmut =0.001 #0.1% 
std_dev=0.005

data_sd = data.frame(matrix(0, nrow = Ns ,ncol=gen)) 
data_nd = data.frame(matrix(0, nrow = Nn ,ncol=gen)) 
data_fd = data.frame(matrix(0, nrow = N,ncol=gen))

#2. Data frame for male population
#2.1 Sexually Selected Males

ss_male =data.frame(id= 1:Ns, #Unique identification
                    des_m =rep(sd_i), #Desertion prob.
                    mate_pair=rep(1), #Mating Prob. for pairing
                    mate_epc=rep(1), #Mating Prob. for EPC
                    fit=rep(0)) #Fitness


#2.2 Non-Sexually selected Males

ns_male =data.frame(id= (Ns+1):N, #Unique identification
                    des_m =rep(nd_i), #Desertion prob.
                    mate_pair=rep(alpha), #Mating Prob for pairing
                    mate_epc=rep(alpha), #Mating Prob for EPC
                    fit=rep(0)) #Fitness
#2.3 Data frame for all males

male = rbind(ss_male,ns_male)

#3. Data frame for females

female = data.frame(id= 1:N, #Unique identification
                    des_f =rep(fd_i),#Desertion prob.
                    mate_pair=rep(1),#Mating Prob. for pairing
                    fit=rep(0)) #Fitness


for (j in 1:gen){#Loop to simulate generations
  sum_male= sum(male$mate_pair)
  sum_female=sum(female$mate_pair)
  
  while (sum_female>0){ #Loop simulating breeding season
    
    #STEP-1: Sample an individual from both sexes to mate
    
    m =sample(male$id,size=1,prob=male$mate_pair, replace = FALSE)
    f =sample(female$id,size=1,prob=female$mate_pair,replace = FALSE)
    
    #STEP-1.1: Setting values of fd and md
    
    fd=female$des_f[f]
    md=male$des_m[m]
    
    #Step-1.2: Update mating probs of the chosen pair
    
    female$mate_pair[f]=0    
    male$mate_pair[m]=0
    
    if (m>Ns){ #EPC prob of paired-up males changes
      male$mate_epc[m]=alpha*md
    }
    else{
      male$mate_pair[m]=md
    }
    
    #STEP-2: Evaluate female's mating state to establish
    #baseline fitness
    
    status= sample(c("c","d"),1,prob=c(1-fd,fd))
    if (status=="c"){
      fem_fit=Wc*V1
      addon=Wc*(V2-V1)
    }
    else{
      fem_fit=Wd*V0
      addon=Wc*(V1-V0)
    }
    
    #STEP-3: Sampling for EP male
    
    if (m>Ns){ #If social male is an NSSM
      epc= sample(male$id,1,prob=male$mate_epc)
      is_epc="y"
    }
    else{
      is_epc="n"
    }
    
    #STEP-4: Fitness updates
    
    #If EPC has occured, then the fitnesses would be
    addon= addon*(1-md) #EPC occurs when social male is an NSSM
    fem_fit= fem_fit + addon #Total female fitness
    female$fit[f]=female$fit[f]+fem_fit #Female fitness updated
    
    if (is_epc=="y"){ 
      male$fit[epc]= male$fit[epc]+fem_fit*(1-p) #EP male fitness updated: Remaining paternity offered to EP male
      male$fit[m]= male$fit[m]+fem_fit*p #Social male fitness updated: Paternity of social male
    }
    else{
      male$fit[m]=fem_fit #Social male fitness updated
    }
    sum_male= sum(male$mate_pair)
    sum_female=sum(female$mate_pair)
  } #while-loop[breeding season] ends
  
  #Next generation
  s_next=sample(male$des_m[1:Ns],Ns,prob=male$fit[1:Ns],replace=TRUE) 
  n_next=sample(male$des_m[(Ns+1):N],Nn ,prob=male$fit[(Ns+1):N],replace=TRUE) 
  f_next=sample(female$des_f[1:N],N,prob=female$fit[1:N],replace=TRUE) 
  m_next=c(s_next,n_next) 
  male$des_m=m_next+rnorm(N,0,std_dev)
  
  for(i in 1:N){
    if(male$des_m[i]<0){ 
      male$des_m[i]=0
    }
    else if(male$des_m[i]>1){
      male$des_m[i]=1
    }
  }
  female$des_f=f_next+rnorm(N,0,std_dev)
  for(i in 1:N){
    if(female$des_f[i]<0){
      female$des_f[i]=0
    }
    else if(female$des_f[i]>1){ 
      female$des_f[i]=1 
    } 
  }
  Nsm= Ns*randmut 
  Nnm = Nn*randmut 
  Nfm = N*randmut 
  randmut_ss= sample(male$id[1:Ns],Nsm)
  for (l in 1:Nsm){ 
    male$des_m[randmut_ss[l]]= runif(1) 
  }
  randmut_ns= sample(male$id[(Ns+1):N],Nnm) 
  for (l in 1:Nnm){
    male$des_m[randmut_ns[l]]= runif(1)
  }
  randmut_f= sample(female$id[1:N],Nfm) 
  for (l in 1:Nfm){
    female$des_f[randmut_f[l]]= runif(1)
  }
  
  male$mate_pair[1:Ns]=rep(1) 
  male$mate_pair[(Ns+1):N]=rep(alpha)
  male$mate_epc[1:Ns]=rep(1)
  male$mate_epc[(Ns+1):N]=rep(alpha)
  female$mate_pair=rep(1)
  
  male$fit=rep(0) 
  female$fit= rep(0)
  
  data_sd[j]= male$des_m[1:Ns] 
  data_nd[j]= male$des_m[(Ns+1):N] 
  data_fd[j]= female$des_f 
  if (j %% 100 ==0){ 
    print(j)
  }
} #for-loop [generation] ends

#RESULTS
mean_sd=rep(0,gen) 
mean_nd=rep(0,gen) 
mean_fd=rep(0,gen) 
for(z in 1:gen){ 
  mean_sd[z]= mean(data_sd[[z]]) #i+1 is important 
  mean_nd[z]= mean(data_nd[[z]]) 
  mean_fd[z]= mean(data_fd[[z]])
}
ms=as.ts(mean_sd)
ns=as.ts(mean_nd)
fs=as.ts(mean_fd)
ts.plot(ms ,ns ,fs , gpars=list(col=c( " green" , " red" ," blue" )))
