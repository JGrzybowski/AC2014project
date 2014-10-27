GenerateLearningSet <- function (classes, instances, features, rangeMin, rangeMax){
  learningSet = array(data = runif(instances*classes*features,min = rangeMin, max=rangeMax),dim = c(instances*classes, features));
  learningSet
}

GenerateTestingSet <- function(learningSet, testInstances=10, classes=10, sigma=0.5){
  features = dim(learningSet)[2];
  testingSet = array(dim = c(testInstances*classes,features));
  #means = vector(length = features);
  for (i in 1:features){
    m = mean(learningSet[,i]);
    difference = rnorm(n = testInstances*classes,mean = 0,sd = sigma);
    testingSet[,i] = m+difference;
  }  
  testingSet
}

Normalize <- function(set){
  columns = dim(set)[2];
  for (i in 1:columns){
    mini = min(set[,i]);
    maxi = max(set[,i]);
    set[,i] = (set[,i]-mini)/(maxi-mini);
  }
  set
}

CreateTT <- function(symbols, states){
  TT = array(data = 0,dim = c(states, states, symbols))
  positions = sample(x = 1:states, size = states*symbols, replace =TRUE)
  
  for(i in 0:((states*symbols)-1)){
    TT[positions[i+1],(i%%states)+1,(i%/%states)+1] = 1;
  }  
  TT
}

DoMagicMinMax <-function(row, state){
  results = vector(mode = "numeric", length = length(state));
  for (i in 1:length(state)){
    results[i] = min(row[i],state[i]);
    if(is.na(results[i]))
      debug()
  }
  
  max(results)
}

ComputeNextState <- function(TT,inputSymbol,state){
  newState = vector(mode = "numeric", length = length(state));
  for (i in 1:length(state)){
    newState[i] = DoMagicMinMax(TT[i,,inputSymbol],state);
  }
  if (1 %in% newState)
    newState
  else
    debug()
}

debug <- function(){
  1
}
ClassifyWord <- function(TT,word){
  states = dim(TT)[1];
  state = vector(mode = "numeric",length=states); 
  state[1]=1;
  for(i in 1:length(word)){
    input = word[i]
    state = ComputeNextState(TT,input,state);
  }
  match(1,state)
}

CalculateError <- function(TT,words,instances){
  error = c();
  for(i in 1:(dim(words)[1])){
    if (ClassifyWord(TT,words[i,]) != ((i-1)%/%instances)+1)
      {error=c(error,FALSE);}
    else
      {error=c(error,TRUE);}
    #cat("word: ", words[i,], "  class: ", a,"should be class: ", b, "\n");
  }
  
  dim(words)[1] - sum(error)
}

CalculateErrorFromVector <- function(vTT,words,instances,states,symbols){
  #cat("Calculating Error... ");
  TT = ChangeValuesToZerosAndOnes(vTT,states,symbols)
  error = 0;
  for(i in 1:(dim(words)[1])){
    a=(ClassifyWord(TT,words[i,]))
    b=(((i-1)%/%instances)+1)  
    #cat("word#",i,"detected=",a,"class=",b,"\n");
    if ( a != b ){
      error=error+1;
    }
  }
  #cat(error,"Errors... \n");
  #cat("errors:",error,"\n",file="outfile.txt",append=TRUE);
  error
}

ChangeValuesToZerosAndOnes <- function(vTT,states,symbols)
{
  TT = array(vTT,dim = c(states,states,symbols))
  for(layer in 1:symbols)
    for(column in 1:states)
    {
     TT[,column,layer][TT[,column,layer]==max(TT[,column,layer])][1] <- Inf;
    } 
  TT[!is.infinite(TT)] <- 0;
  TT[is.infinite(TT)] <- 1;
  TT
}

ChangeValuesToSymbols <- function(x,symbols){
    newVals = ceiling(x/(1/symbols))
    newVals[x == 0] = 1;
    newVals
}


CreateAutomata <- function(classes, features, 
                           learningInstancesPerClass, testingInstancesPerClass,
                           rangeMin, rangeMax, sigma, numberOfSymbols,
                           LearningSet, TestingSet){
  
  #cat("",file="outfile.txt",append=FALSE);
  
  LearningSet = Normalize(LearningSet);
  LearningSet = ChangeValuesToSymbols(LearningSet, numberOfSymbols);
  TestingSet = Normalize(TestingSet);
  TestingSet = ChangeValuesToSymbols(TestingSet, numberOfSymbols);
  
  TT = CreateTT(numberOfSymbols, classes);
  
  
# HYDRO PSO VERSION  
  results = hydroPSO(par = as.vector(TT),fn = CalculateErrorFromVector,
                     words = LearningSet, instances = learningInstancesPerClass, states = classes, symbols = numberOfSymbols,
                     lower = rep(0,length(TT)), upper = rep(10,length(TT)), 
                     control = list(parallel = 'parallelWin', par.nnodes = 8, REPORT = 10))

# PPSO VERSION  
#   results = optim_ppso_robust(parameter_bounds = matrix(c(0,1),c(1,2)),max_number_of_iterations = 5,
#                               number_of_parameters = length(TT),objective_function = CalculateErrorFromVector,
#                               projectfile = "ParallelProgress.txt", save_interval = 50, plot_progress = TRUE
#                               )

# STANDARD PSO VERSION
#   results = psoptim(as.vector(TT),fn = CalculateErrorFromVector,
#           words = LearningSet, instances = learningInstancesPerClass, states = classes, symbols = numberOfSymbols,
#           lower = 0, upper = 1, 
#           control = list(trace = 1, REPORT = 10, trace.stats =TRUE));
  TT = ChangeValuesToZerosAndOnes(results$par,classes,numberOfSymbols);
  cat("Smallest Error", results$value,"\n");
  effi = CalculateError(TT, TestingSet, testingInstancesPerClass);
  cat("Efficiency:",1-(effi/dim(TestingSet)[1]),"\n");
  results
  
}

RunTest1 <- function(classes = 3, features = 10, 
                     learningInstancesPerClass = 50, testingInstancesPerClass = 20,
                     rangeMin = 5, rangeMax = 10, sigma = 0.2, numberOfSymbols = 5)
{
  library("hydroPSO", lib.loc="~/R/win-library/3.1");
  
  
  LS = GenerateLearningSet(classes, learningInstancesPerClass, features, rangeMin, rangeMax);
  TS = GenerateTestingSet(LS, testingInstancesPerClass, classes, sigma);
  
  
  CreateAutomata  (classes, features, 
                   learningInstancesPerClass, testingInstancesPerClass, 
                   rangeMin, rangeMax, sigma, numberOfSymbols,
                   LearningSet = LS, TestingSet = TS)
  
}