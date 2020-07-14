
#1. Parameters

N   = 500 #No. of individuals of every sex in the popln
X   = 0.4 #Fraction of males which are sexually selected
Ns  = N*X #Sexually selected males
Nn  = N-Ns #Non-sexually selected males
gen = 100 #No. of generations


Wc = 3 #Eggs layed by caring females
Wd = 5 #Eggs layed by a deserting female
V0 = 0.2 #0 caring parents
V1 = 0.5 #1 caring parent
V2 = 0.8 #Both caring parents
p = #Paternity


sd_i=0.6 #Initial desertion prob. for SSMs
nd_i=0.2 #For NSSMs
fd_i=0.5 #For females
alpha=  #Mating prob. for NSSMs

#2. Data frame for male population
  #2.1 Sexually Selected Males

  ss_male =data.frame(id= 1:Ns, #Unique identification
                      md =rep(sd_i), #Desertion prob.
                      mate_pair=rep(1), #Mating Prob. for pairing
                      mate_epc=rep(sd), #Mating Prob. for EPC
                      fit=rep(0)) #Fitness
                    

  #2.2 Non-Sexually selected Males

  ns_male =data.frame(id= (Ns+1):N, #Unique identification
                      md =rep(nd_i), #Desertion prob.
                      mate_pair=rep(alpha), #Mating Prob for pairing
                      mate_epc=rep(alpha*nd), #Mating Prob for EPC
                      fit=rep(0)) #Fitness
  #2.3 Data frame for all males
  
  male = rbind(ss_male,ns_male)

#3. Data frame for females
female = data.frame(id= 1:N, #Unique identification
                    fd =rep(fd_i),#Desertion prob.
                    mate_pair=rep(1),#Mating Prob. for pairing
                    fit=rep(0)) #Fitness



for (j in 1:gen){ #Loop to simulate generations
  
}
sum_male= sum(male$mate_pair)
sum_female=sum(female$mate_pair)

while (sum_female>0){ #Loop simulating breeding season
  
  #STEP-1: Sample an individual from both sexes to mate
  
  m =sample(male$id,1,prob=male$mate_pair)
  f =sample(female$id,1,prob=female$mate_pair)
  
  #Step-1.1: Update mating probs of the chosen pair
  female$mate_pair[f]=0
  male$mate_pair[m]=0
 
  #STEP-2: Evaluate female's mating state to establish
  #baseline fitness
  status=sample(c("c","d"),1,prob=c(1-fd,fd))
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
  
  if (is_epc=="y"){ #If EPC has occured, then the fitnesses would be
    addon= addon*(1-nd) #EPC occurs when social male is an NSSM
    fem_fit= fem_fit + addon #Total female fitness
    female$fit[f]=fem_fit #Fitness updated
    
    male$fit[epc]=fem_fit*(1-p) #EP male fitness updated: Remaining paternity offered to EP male
    male$fit[m]=fem_fit*p #Social male fitness updated: Paternity of social male
  }
  
  else{
    addon= addon*(1-sd) #Social male is an SSM- hence no EP male
    fem_fit= fem_fit + addon
    female$fit[f]=fem_fit#Female fitness updated
    
    male$fit[m]=fem_fit #Social male fitness updated
  }
  a = male$md[m] #md
  b = female$fd[f] #fd
  c = runif(1,min=0,max=1) # No. to compare with md
  d = runif(1,min=0,max=1) #No. to compare with fd
}
print(female$fit)

