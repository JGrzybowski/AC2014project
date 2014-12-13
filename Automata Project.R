### SETS FUNCTIONS ###
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

ChangeValuesToSymbols <- function(x,symbols){
  newVals = ceiling(x/(1/symbols))
  newVals[x == 0] = 1;
  newVals
}
### AUTOMATA FUNCTIONS ###
CreateTT <- function(states, symbols, nondeterminism = 1){
  TT = array(data = runif(n = states*states*symbols, min = 0.0, max = 1.0),
             dim = c(states, states, symbols))
  round(TT,digits = 3)
}

ComputeNextState <- function(TT,inputSymbol,state){
  newState = vector(mode="numeric",length=dim(TT)[1])
  for(i in 1:length(newState)){
    newState[i] = UseTriangleNorm(TT[i,,inputSymbol],state)
  }
  newState
  
  #if(rej)
  #  states = sort(unique(c(states,0)))
  #else
  #  states = sort(unique(as.vector(states)))
}

UseTriangleNorm <- function(row, column){
  result = vector(mode="numeric",length = length(column))
  for(i in 1:length(column)){
    result[i] = triMin(c(row[i], column[i]))
  }
  triMax(result)
}

triMax <- function(values){
  #max(values)
  values = atanh(values);
  value = tanh(sum(values))
  if(is.infinite(value))
    value= 0
  value
}

triMin <- function(values){
  #min(values)
  values = atanh(1-values)
  value = 1 - tanh(sum(values))
  if(is.infinite(value))
    value= 1
  value
}

debug <- function(){
  1
}

CalculateSymbolsVector <- function(value,numberOfSymbols){
  result = vector(mode = "numeric", numberOfSymbols)
  sd = rep(1/numberOfSymbols, numberOfSymbols)
  m = 0:(numberOfSymbols-1)*(sd)+(sd/2);
  v = rep(value, numberOfSymbols)
  tail = v<m  
  result[tail] = pnorm(v[tail],m[tail],sd[tail],TRUE)
  result[!tail] = pnorm(v[!tail],m[!tail],sd[!tail],FALSE)
  round(result,4)
}

ClassifyWord <- function(TT,word){  
  states = dim(TT)[1]
  symbols = dim(TT)[3]
  state = rep(0.3,states);
  Ytab = array(dim=c(states,symbols))
  for(i in 1:length(word)){
    symV = CalculateSymbolsVector(word[i],states)
    for(symbol in 1:symbols){
      Ytab[,symbol] = ComputeNextState(TT,symbol,state);
    }
    for(symbol in 1:symbols){
      for(place in 1:states)
        Ytab[place,symbol]=triMin(c(symV[place],Ytab[place,symbol]))
    }
    for(place in 1:states)
      state[place] = triMax(Ytab[place,])
    #cat("\t states: (",state,") \n");
  }
  state
}

CalculateError <- function(TT,words,instances,minChance){
  error = 0;
  wordsNo = dim(words)[1]
  classesNo = dim(TT)[1]
  for(i in 1:(wordsNo)){
    #cat("#",i," word - ", words[i,], " \n");
    possibleClassification = (ClassifyWord(TT,words[i,]))
    
    #If the word should be rejected
    if (classesNo*instances < i){
        if(sum(possibleClassification<minChance) != length(possibleClassification))
          error=error+sum(possibleClassification[possibleClassification>minChance]-minChance)
        expectedClass = 0
    }
    #If the word should be accepted in specific class  
    else {
      expectedClass = (((i-1)%/%instances)+1)  
      if (possibleClassification[expectedClass] < minChance)
        error=error+(minChance - possibleClassification[expectedClass]);
    }
    #cat("word#",i,"detected=",possibleClassification,"class=",expectedClass,"\n");
  }
  error
}

CalculateErrorFromVector <- function(vTT,words,instances,states,symbols,minChance){
  TT = HandlePSOVector(vTT,states,symbols)
  CalculateError(TT,words,instances,minChance)
}

HandlePSOVector <- function(vTT,states,symbols){
  array(vTT,dim = c(states,states,symbols))
}


CreateAutomata <- function(sets, classes, features, numberOfSymbols,
                           learningInstancesPerClass, testingInstancesPerClass,
                           rangeMin, rangeMax, minChance, iterations){
  
  #cat("",file="outfile.txt",append=FALSE);
  
  sets = Normalize(sets)
  #sets$learn = ChangeValuesToSymbols(sets$learn, numberOfSymbols)
  #sets$test = ChangeValuesToSymbols(sets$test, numberOfSymbols)
  
  TT = CreateTT(classes, numberOfSymbols);

# HYDRO PSO VERSION  
  library("hydroPSO", lib.loc="~/R/win-library/3.1");
  results = hydroPSO(par = matrix(TT,nrow=1),fn = CalculateErrorFromVector,
                     words = sets$learn, instances = learningInstancesPerClass, 
                     states = classes, symbols = numberOfSymbols, minChance = minChance,
                     lower = rep(0,length(TT)), upper = rep(1,length(TT)),  
                     control = list(parallel = 'parallelWin', par.nnodes = 8, REPORT = 10, maxit=iterations))

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
#   TT = HandlePSOVector(results$par,classes,numberOfSymbols);
#   cat("Smallest Error", results$value,"\n");
#   effi = CalculateError(TT, sets$test, testingInstancesPerClass,minChance);
#   cat("Efficiency:",1-(effi/dim(sets$test)[1]),"\n");
  results
}

### TESTS ###
RunTest0 <- function(classes = 5, 
                     features = 10, 
                     numberOfSymbols = 4,
                     learningInstancesPerClass = 10, testingInstancesPerClass = 5, 
                     rejectingInstances = 17,
                     rangeMin = 5, rangeMax = 10, 
                     sigma = 0.2,
                     minChance = 0.65,
                     iterations = 100){
  
  sets = GenerateSets(classes, learningInstancesPerClass, testingInstancesPerClass, rejectingInstances, features, rangeMin, rangeMax, sigma)
  CreateAutomata  (sets, classes, features, numberOfSymbols,
                   learningInstancesPerClass, testingInstancesPerClass, 
                   rangeMin, rangeMax, minChance, iterations)

}


RunTest1 <- function(classes = 15, features = 10, 
                     learningInstancesPerClass = 10, testingInstancesPerClass = 5, rejectingInstances = 27,
                     rangeMin = 5, rangeMax = 10, sigma = 0.2, numberOfSymbols = 5, minChance = 0.85, iterations = 500)
{
  sets = GenerateSets(classes, learningInstancesPerClass, testingInstancesPerClass, rejectingInstances, features, rangeMin, rangeMax, sigma)
    
  CreateAutomata(sets, classes, features, numberOfSymbols,
                   learningInstancesPerClass, testingInstancesPerClass, 
                   rangeMin, rangeMax, minChance, iterations)
  
}