### SETS FUNCTIONS ###
GenerateSets <- function (noClasses, noRepetitionsInClass, percTest, percForeign, features, rangeMin, rangeMax, sigma){
  noLearnWords = noClasses * noRepetitionsInClass
  noTestWords = percTest/100 * noClasses * noRepetitionsInClass
  noTrainWords = noLearnWords - noTestWords
  noForeignLearn =  percForeign/100 * noTrainWords
  noForeignTest = percForeign/100 * noTestWords
  noForeignWords = noForeignLearn + noForeignTest
  
  learningSet = array(,dim = c(noLearnWords, features+1))
  for (c in 1:noClasses){
    indexesOfClass = (1:noRepetitionsInClass)+((c-1)*noRepetitionsInClass);
    learningSet[indexesOfClass,1] = c
    for(f in 2:(features+1)){
      m = mean(runif(noRepetitionsInClass, min=rangeMin, max=rangeMax)) 
      learningSet[indexesOfClass,f] = m
    }
  }  
  learningSet[,-1] = learningSet[,-1] + rnorm(n=noLearnWords*features, mean=0, sd=sigma)
  
  learningSet = learningSet[sample(noLearnWords),]
  
  foreignSet= array(runif(n = noForeignWords*(features+1), min = rangeMin, max = rangeMax),dim=c(noForeignWords,features+1))  
  if(noForeignWords > 0)
    foreignSet[,1] = 0
  
  trainN = learningSet[1:noTrainWords,]
  testN = learningSet[(noTrainWords+1):noLearnWords,]
  
  if(noForeignLearn > 0)
    trainF = foreignSet[1:noForeignLearn,]
  else
    trainF = NULL
  
  if(noForeignTest > 0)
    testF = foreignSet[(noForeignLearn+1):(noForeignWords),]
  else
    testF = NULL

  list("learn" = rbind(trainN, trainF, deparse.level = 0), "test" = rbind(testN, testF, deparse.level = 0))
}

Normalize <- function(sets){
  columns = dim(sets$learn)[2]
  for (i in 2:columns){
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
CreateTT <- function(states, symbols, nondeterminism = 1, rejection = TRUE){
  if(is.infinite(nondeterminism))
  {
    TT = array(data = runif(n = states*states*symbols, min = 0.0, max = 1.0),
                dim = c(states, states, symbols))
    TT = round(TT,digits = 3)
  }
  else
  {
    if (rejection)
      minStates = 0
    else
      minStates = 1
    TT = array(data = 0,dim = c(states, states, symbols))
    for(symbol in 1:symbols){
      for(state in 1:states){
        TT[sample(1:states,sample(minStates:nondeterminism)),state,symbol] = 1;
      }
    }
  }
  
  TT
}

ComputeNextState <- function(TT,inputSymbol,state){
  newState = vector(mode="numeric",length=dim(TT)[1])
  for(i in 1:length(newState)){
    newState[i] = UseTriangleNorm(TT[i,,inputSymbol],state)
  }
  newState
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

ClassifyWord <- function(TT,word, discrete){  
  states = dim(TT)[1]
  symbols = dim(TT)[3]
  if(discrete == TRUE)
  {
    state = rep(0,states)
    state[1] = 1
    for(i in 1:length(word)){
      state = CalculateSymbolsVector(word[i],states)
    }    
  }
  else{
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
  }
  state
}

CalculateError <- function(TT,words,instances,minChance){
  error = 0;
  wordsNo = dim(words)[1]
  noFeatures = dim(words)[2]-1
  classesNo = dim(TT)[1]
  for(i in 1:(wordsNo)){
    #cat("#",i," word - ", words[i,], " \n");
    possibleClassification = (ClassifyWord(TT,words[i,2:noFeatures+1]))
    expectedClass = words[i,1]
    #If the word should be rejected
    if (expectedClass==0){
        if(sum(possibleClassification<minChance) != length(possibleClassification))
          error=error+sum(possibleClassification[possibleClassification>minChance]-minChance)
    }
    #If the word should be accepted in specific class  
    else {
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

HandlePSOVector <- function(vTT,states,symbols,rejecting,nondeterminism){
  TT = array(vTT,dim = c(states,states,symbols))
  if(is.infinite(nondeterminism) == FALSE)
    ChangeValuesToZerosAndOnes(TT,rejecting,nondeterminism)
  else
    TT
}

ChangeValuesToZerosAndOnes <- function(TT,rejecting, nondeterminism){
  states = dim(TT)[1]
  symbols = dim(TT)[3]
  if(rejecting == FALSE)
    minimumPSOValue = 0
  else
    minimumPSOValue = 0.2
  for(layer in 1:symbols)
    for(column in 1:states)
    {
      ord = order(TT[,column,layer], decreasing = TRUE)
      top = TT[,column,layer][ord[1:nondeterminism]]
      TT[,column,layer][ord[1:nondeterminism]][top>minimumPSOValue] = -Inf
    } 
  TT[!is.infinite(TT)] <- 0;
  TT[is.infinite(TT)] <- 1;
  TT
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