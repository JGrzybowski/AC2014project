GenerateSets <- function (classes, instancesL, instancesT, REJinstances, features, rangeMin, rangeMax, sigma){
  learningSet = array(,dim = c(instancesL*classes+REJinstances, features))
  testingSet = array(,dim = c(instancesT*classes+REJinstances, features))
  for (c in 1:classes){
    for(f in 1:features){
      m = mean(runif(instancesL,min=rangeMin,max=rangeMax)) 
      learningSet[(1:instancesL)+((c-1)*instancesL),f]= m
      testingSet[(1:instancesT)+((c-1)*instancesT),f]= m      
    }
  }
  learningSet = learningSet + rnorm(n=instancesL*classes*features,mean =0, sd = sigma)
  testingSet = testingSet + rnorm(n=instancesT*classes*features,mean =0, sd = sigma)
  #for(w in 1:REJinstances){
  data = runif(REJinstances*features, rangeMin, rangeMax)
  x = instancesL*classes+1
  y = instancesL*classes+REJinstances
  learningSet[x:y,] = data
  data = runif(REJinstances*features, rangeMin, rangeMax)
  x = instancesT*classes+1
  y = instancesT*classes+REJinstances
  testingSet[x:y,] = data
  #}
  list("learn" = learningSet, "test"=testingSet);
}

Normalize <- function(sets){
  columns = dim(sets$learn)[2];
  for (i in 1:columns){
    mini = min(min(sets$learn[,i]),min(sets$test[,i]));
    maxi = max(max(sets$learn[,i]),max(sets$test[,i]));
    sets$learn[,i] = (sets$learn[,i]-mini)/(maxi-mini);
    sets$test[,i] = (sets$test[,i]-mini)/(maxi-mini);
  }
  sets
}

CreateTT <- function(states, symbols, nondeterminism = 1){
  TT = array(data = sample(x = 0:states, size= states*symbols*nondeterminism, replace = TRUE),
             dim = c(states, symbols, nondeterminism))
  TT
}

ComputeNextState <- function(TT,inputSymbol,state){
  #What if (state == 0)
  if(0 %in% state)
    rej = TRUE
  else
    rej = FALSE
  #cat("\t q = ",state," in = ",inputSymbol);
  states = TT[state, inputSymbol, ]  
  
  if(rej)
    states = sort(unique(c(states,0)))
  else
    states = sort(unique(as.vector(states)))
}

debug <- function(){
  1
}

ClassifyWord <- function(TT,word){
  state = 1;
  
  for(i in 1:length(word)){
    input = word[i]
    state = ComputeNextState(TT,input,state);
    #cat("\t states: (",state,") \n");
  }
  sort(unique(state))
}

CalculateError <- function(TT,words,instances){
  error = 0;
  wordsNo = dim(words)[1]
  classesNo = dim(TT)[1]
  for(i in 1:(wordsNo)){
    #cat("#",i," word - ", words[i,], " \n");
    possibleClassification = (ClassifyWord(TT,words[i,]))
    b = (((i-1)%/%instances)+1)  
    if (classesNo*instances < i)
      b = 0
    #cat("word#",i,"detected=",a,"class=",b,"\n");
    if ( b %in% possibleClassification == FALSE){
      error=error+1;
    }
  }
  error
}

CalculateErrorFromVector <- function(vTT,words,instances,states,symbols,nondeterminism){
  #cat("Calculating Error... ");
  TT = HandlePSOVector(vTT,states,symbols,nondeterminism)
  CalculateError(TT,words,instances)
}

HandlePSOVector <- function(vTT,states,symbols,nondeterminism){
  TT = array(vTT,dim = c(states,symbols,nondeterminism))
  round(TT)
}

ChangeValuesToSymbols <- function(x,symbols){
  newVals = ceiling(x/(1/symbols))
  newVals[x == 0] = 1;
  newVals
}

CreateAutomata <- function(classes, features, 
                           learningInstancesPerClass, testingInstancesPerClass,
                           rangeMin, rangeMax, sigma, numberOfSymbols,
                           sets, nondeterminism){
  
  #cat("",file="outfile.txt",append=FALSE);
  
  sets = Normalize(sets)
  sets$learn = ChangeValuesToSymbols(sets$learn, numberOfSymbols)
  sets$test = ChangeValuesToSymbols(sets$test, numberOfSymbols)
  
  TT = CreateTT(classes, numberOfSymbols, nondeterminism);

# HYDRO PSO VERSION  
  results = hydroPSO(par = matrix(TT,nrow=1),fn = CalculateErrorFromVector,
                     words = sets$learn, instances = learningInstancesPerClass, 
                     states = classes, symbols = numberOfSymbols, nondeterminism = nondeterminism,
                     lower = rep(1,length(TT)), upper = rep(classes,length(TT)),  
                     control = list(parallel = 'parallelWin', par.nnodes = 8, REPORT = 10,maxit=400))

# PPSO VERSION  
#   results = optim_ppso_robust(parameter_bounds = matrix(c(0,1),c(1,2)),max_number_of_iterations = 5,
#                               number_of_parameters = length(TT),objective_function = CalculateErrorFromVector,
#                               projectfile = "ParallelProgress.txt", save_interval = 50, plot_progress = TRUE
#                               )

# STANDARD PSO VERSION
#   results = psoptim(c(TT),fn = CalculateErrorFromVector,
#                     words = sets$learn, instances = learningInstancesPerClass, 
#                     states = classes, symbols = numberOfSymbols, nondeterminism = nondeterminism,
#                     lower = 1, upper = classes,
#                     control = list(trace = 1, REPORT = 10, trace.stats =TRUE));
  TT = HandlePSOVector(results$par,classes,numberOfSymbols,nondeterminism);
  cat("Smallest Error", results$value,"\n");
  effi = CalculateError(TT, sets$test, testingInstancesPerClass);
  cat("Efficiency:",1-(effi/dim(sets$test)[1]),"\n");
  results
  
}

RunTest1 <- function(classes = 15, features = 10, 
                     learningInstancesPerClass = 10, testingInstancesPerClass = 5, rejectingInstances = 27,
                     rangeMin = 5, rangeMax = 10, sigma = 0.2, numberOfSymbols = 5, nondeterminism = 1)
{
  library("hydroPSO", lib.loc="~/R/win-library/3.1");
  
  sets = GenerateSets(classes, learningInstancesPerClass, testingInstancesPerClass, rejectingInstances, features, rangeMin, rangeMax, sigma)
    
  CreateAutomata  (classes, features, 
                   learningInstancesPerClass, testingInstancesPerClass, 
                   rangeMin, rangeMax, sigma, numberOfSymbols,
                   sets, nondeterminism)
  
}