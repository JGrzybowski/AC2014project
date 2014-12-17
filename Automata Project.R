# SETS FUNCTIONS -----

## Creating sets for automata  ================================
GenerateSets <- function( fromFile, pathTrain, pathTest, 
                        pathForeignTrain, pathForeignTest, 
                        noClasses = 0, noFeatures = 0, noRepetitionsInClass = 0,
                        minRand = -Inf, maxRand = Inf, distortion = 0,
                        percTestSize = -1, percForeignSize = -1){
### Obtaining the main dataset  ================================
  if(fromFile)
  {
    mainDataSet = LoadXlsxFile(pathTrain)
    noWords = dim(mainDataSet)[1]
  }
  else 
  {
    noWords = noClasses * noRepetitionsInClass
    mainDataSet = Generate.LearningSet(noClasses, noRepetitionsInClass, noFeatures, noWords, minRand, maxRand, distortion)
  }
### Obtaining the test dataset  ================================
  if(fromFile && !missing(pathTest))
  { 
    sets = list("learn" = mainDataSet, "test" = LoadXlsxFile(pathTest))
  }
  else{
    noTestWords = (percTestSize * noWords) %/% 100
    testIndexes = sample(1:noWords,size = noTestWords)
    sets = list("learn" = mainDataSet[-testIndexes,], "test" = mainDataSet[testIndexes,])                            
  }
### Obtaining the foreign train dataset  ================================
  if(fromFile && !missing(pathForeignTrain))
  {
    foreignTrain = LoadXlsxFile(pathForeignTrain)
  }
  else
  {
    noForeignTrain = (percForeignSize * dim(sets$learn)[1]) %/% 100
    foreignTrain = array(data = runif(noForeignTrain * (noFeatures+1), minRand, maxRand), dim = c(noForeignTrain,noFeatures+1))
  }
  foreignTrain[,1] = 0
  sets$learn = rbind(sets$learn,foreignTrain,deparse.level = 0)
### Obtaining the foreign test dataset  ================================
  if(fromFile && !missing(pathForeignTest))
  {
    foreignTest = LoadXlsxFile(pathForeignTest)
  }
  else
  {
    noForeignTest = (percForeignSize * dim(sets$test)[1]) %/% 100
    foreignTest = array(data = runif(noForeignTrain * (noFeatures+1), minRand, maxRand), dim = c(noForeignTest,noFeatures+1))
  }
  foreignTest[,1] = 0
  sets$test = rbind(sets$test,foreignTest,deparse.level = 0)  
  sets
}

# Generating the learning set ================================
Generate.LearningSet <- function(noClasses, noRepetitionsInClass, noFeatures, noLearnWords, rangeMin, rangeMax, sigma){
  learningSet = array(,dim = c(noLearnWords, noFeatures+1))
  for (c in 1:noClasses){
    indexesOfClass = (1:noRepetitionsInClass)+((c-1)*noRepetitionsInClass);
    learningSet[indexesOfClass,1] = c
    for(f in 2:(noFeatures+1)){
      m = mean(runif(noRepetitionsInClass, min=rangeMin, max=rangeMax)) 
      learningSet[indexesOfClass,f] = m
    }
  }  
  learningSet[,-1] = learningSet[,-1] + rnorm(n=noLearnWords*noFeatures, mean=0, sd=sigma)
  learningSet
}

LoadXlsxFile <- function(fileName, sheet = 1){
  if(!require(openxlsx))
    install.packages("openxlsx", dependencies = TRUE)
  read.xlsx(fileName, sheet, colNames = FALSE)
}
Normalize <- function(sets){
  columns = dim(sets$learn)[2]
  for (i in 2:columns){
    mini = min(min(sets$learn[,i]),min(sets$test[,i]));
    maxi = max(max(sets$learn[,i]),max(sets$test[,i]));
    if(maxi == mini)
      mini=0
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
# AUTOMATA FUNCTIONS -----
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
    TT = array(data = 0, dim = c(states, states, symbols))
    for(symbol in 1:symbols){
      for(state in 1:states){
        TT[sample(1:states,sample(minStates:nondeterminism)),state,symbol] = 1;
      }
    }
  }
  
  TT
}

ComputeNextState <- function(TT,inputSymbol,state){
  newState = vector(mode="numeric", length = dim(TT)[1])
  for(i in 1:length(newState)){
    newState[i] = UseTriangleNorm(TT[i,,as.numeric(inputSymbol)],state)
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

CalculateSymbolsVector <- function(value,numberOfSymbols){
  value = as.numeric(value)
  result = vector(mode = "numeric", numberOfSymbols)
  sd = rep(1/numberOfSymbols, numberOfSymbols)
  m = 0:(numberOfSymbols-1)*(sd)+(sd/2);
  v = rep(value, numberOfSymbols)
  tail = v < m
  result[tail] = pnorm(v[tail],m[tail],sd[tail],TRUE)
  result[!tail] = pnorm(v[!tail],m[!tail],sd[!tail],FALSE)
  round(result,4)
}
# CLASSIFYING WORDS -----
ClassifyWord <- function(TT,word, discrete){  
  states = dim(TT)[1]
  symbols = dim(TT)[3]
  if(discrete == TRUE)
  {
    state = rep(0,states)
    state[1] = 1
    for(i in 1:length(word)){
      state = ComputeNextState(TT,word[i],state)
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

CalculateError <- function(TT,words,minChance,discrete){
  error = 0;
  wordsNo = dim(words)[1]
  noFeatures = dim(words)[2]-1
  classesNo = dim(TT)[1]
  for(i in 1:(wordsNo)){
    #cat("#",i," word - ", words[i,], " \n");
    possibleClassification = ClassifyWord(TT,words[i,2:noFeatures+1],discrete)
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

CalculateErrorFromVector <- function(vTT,words,states,symbols,minChance,rejecting,boundNonDeterminism, discrete){
  TT = HandlePSOVector(vTT,states,symbols,rejecting,boundNonDeterminism)
  CalculateError(TT,words,minChance,discrete)
}

HandlePSOVector <- function(vTT,states,symbols,rejecting,boundNonDeterminism){
  TT = array(vTT,dim = c(states,states,symbols))
  if(is.infinite(boundNonDeterminism) == FALSE)
    ChangeValuesToZerosAndOnes(TT,rejecting,boundNonDeterminism)
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
  for(symbol in 1:symbols)
    for(column in 1:states)
    {
      ord = order(TT[,column,symbol], decreasing = TRUE)
      top = TT[ord[1:nondeterminism],column,symbol]
      TT[ord[1:nondeterminism],column,symbol][top>minimumPSOValue] = -Inf
    } 
  TT[!is.infinite(TT)] <- 0;
  TT[is.infinite(TT)] <- 1;
  TT
}

# Old functions -----
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

# TESTS -----
TestAllPhases.Generated <- function(){
  for (phase in Phases){
    cat("Running test: ",phase,".\n")
    AC2014(phase, inputType = "gen", noClasses = 2, noFeatures = 3, discretization = 4, noRepetitionsInClass = 10, 
           minRand = 0, maxRand = 25, distortion = 0.6, 
           percForeignSize = 15, percTestSize = 20, 
           parallel = "NO", PSOtrace = 10, PSOmaxit = 15)
  }
}
TestAllPhases.Autodetected <- function(){
  for (phase in Phases){
    cat("Running test: ",phase,".\n")
    AC2014(phase, noClasses = 2, noFeatures = 3, discretization = 4, noRepetitionsInClass = 10, 
           minRand = 0, maxRand = 25, distortion = 0.6, 
           percForeignSize = 15, percTestSize = 20, 
           parallel = "NO", PSOtrace = 10, PSOmaxit = 15)
  }
}

TestAllPhases.File <- function(){
  for (phase in Phases){
    cat("Running test: ",phase,".\n")
    AC2014(phase, inputType = "red", 
           pathTrain = "SmallInputTest.xlsx", pathTest = "SmallInputTest.xlsx", 
           pathForeignTrain = "SmallInputTest.xlsx", pathForeignTest = "SmallInputTest.xlsx", 
           discretization = 5,
           parallel = "NO", PSOtrace = 10, PSOmaxit = 15)
  }
}
# Project running function -----
AC2014 <- function(phase, inputType = "", 
                   pathTrain = "", pathTest = "", 
                   pathForeignTrain = "", pathForeignTest = "", 
                   pathOutputClass = "", pathOutputErr = "",
                   noClasses = 0, noFeatures = 0, noRepetitionsInClass = 0,
                   minRand = -Inf, maxRand = Inf, distortion = 0,
                   percTestSize = -1, percForeignSize = -1,
                   discretization = 0, boundNonDeterminism = 0,
                   parallel = "YES",
                   PSOtrace, PSOfnscale, PSOmaxit, PSOmaxf,
                   PSOabstol, PSOreltol, PSOREPORT, PSOtrace.stats,
                   PSOs, PSOk, PSOp, PSOw, PSOc.p, PSOc.g, PSOd, PSOv.max, 
                   PSOrand.order, PSOmax.restart, PSOmaxit.stagnate) 
{
### Validate Phase dependent arguments -----
  if(!(phase %in% Phases))
    stop("Phase must be one of {a1,a2,a3,a4,a5,a6}!")
### REJECTION =====
  if(phase %in% NonRejectingPhases)
  {
    rejection = FALSE
    percForeignSize = 0
  }
  else 
    rejection = TRUE
### DETERMINISM =====
  if(phase %in% DeterministicPhases)
  {
    minChance = 0.5 
    boundNonDeterminism = 1
    discrete = TRUE
  }
  else if(phase %in% NonDeterministicPhases)
  {
    minChance = 0.5
    discrete = TRUE
  }
  else
  {
    minChance = 0.75
    boundNonDeterminism = Inf
    discrete = FALSE
  }  

### Validate inputType =====
  if(inputType == "")
    if(pathTrain == "")
      inputType = "gen"
    else
      inputType = "red"
### Validate parameters for file input =====
  if(inputType == "red")
  {
    if(noClasses != 0 || noFeatures != 0 || noRepetitionsInClass != 0 || minRand != -Inf || maxRand!= Inf || distortion != 0)
      stop("One of the parameters cannot be declared when inputType is red. Please read user's manual before using the program.")
    if(pathTrain == "")
      stop("The path to input file must be given in parameter pathTrain!")
    if(pathTest == "" && percTestSize == -1)
      stop("The path to the test data or percentage of test data must be declared!")
    if(discretization <= 0)
      stop("discretization must be real number bigger than 0!")
    if(phase %in% c("a2","a4","a6"))
    {
      if(pathForeignTrain == "" && perccForeignSize == -1)
        stop("The path to the foreign data or percentage of foreign data must be declared!")
      if(pathForeignTest == "" && percForeignSize == -1)
        stop("The path to the foreign data or percentage of foreign data must be declared!")    
    }
    sets = GenerateSets(fromFile = TRUE, 
                        pathTrain, pathTest, 
                        pathForeignTrain, pathForeignTest,
                        percTestSize = percTestSize)
    noClasses = length(unique(unique(sets$learn[,1]),unique(sets$test[,1])))
    noFeatures = dim(sets$learn)[2]-1
  }
### Validate parameters for generated input =====
  else
  {
    if(!missing(pathTrain) 
       || !missing(pathTest) 
       || !missing(pathForeignTrain) 
       || !missing(pathForeignTest) )
      stop("One of the parameters cannot be declared when inputType is gen. Please read user's manual before using the program.")
    if(noClasses <= 1)
      stop("noClasses must be integer bigger than 1!")
    if(noFeatures <= 0)
      stop("noFeatures must be integer bigger than 0!")
    if(noRepetitionsInClass <= 0)
      stop("noRepetitionsInClass must be integer bigger than 0!")
    if(minRand == -Inf || maxRand == Inf)
      stop("Both minRand and maxRand mus be explicitly declared real numbers!")
    if(distortion <= 0)
      stop("distortion must be real number bigger than 0!")
    if(discretization <= 0)
      stop("discretization must be real number bigger than 0!")
    if(percTestSize < 0)
      stop("The percentage of test data must be declared!")
    if(percForeignSize < 0)
      stop("The percentage of foreign data must be declared!")
    sets = GenerateSets(fromFile = FALSE, noClasses = noClasses, noFeatures = noFeatures, 
                        noRepetitionsInClass = noRepetitionsInClass,
                        minRand = minRand, maxRand = maxRand, distortion = distortion,
                        percTestSize = percTestSize, percForeignSize = percForeignSize)
  }
  
  sets = Normalize(sets)
### DISCRETIZATION =====  
  if(discrete)
  {
    sets$learn[,2:(noFeatures+1)] = ChangeValuesToSymbols(sets$learn[,2:(noFeatures+1)], discretization)
    sets$test[,2:(noFeatures+1)] = ChangeValuesToSymbols(sets$test[,2:(noFeatures+1)], discretization)
  }
### PSO Control parameters =====
  control = list()
  #SingleThread Control
  if(parallel == "NO"){
    if(!missing(PSOtrace))
      control$trace = PSOtrace
    if(!missing(PSOfnscale))
      control$fnscale = PSOfnscale
    if(!missing(PSOmaxf))
      control$maxf = PSOmaxf 
    if(!missing(PSOtrace.stats))
        control$trace.stats = PSOtrace.stats
    if(!missing(PSOs))
      control$s = PSOs
    if(!missing(PSOk))
      control$k = PSOk
    if(!missing(PSOp))
      control$p = PSOp
    if(!missing(PSOw))
      control$w = PSOw
    if(!missing(PSOc.p))
      control$c.p = PSOc.p
    if(!missing(PSOc.g))
      control$c.g = PSOc.g
    if(!missing(PSOd))
      control$d = PSOd
    if(!missing(PSOv.max))
      control$v.max = PSOv.max
    if(!missing(PSOrand.order))
      control$rand.order = PSOrand.order
    if(!missing(PSOmax.restart))
      control$max.restart = PSOmax.restart
    if(!missing(PSOmaxit.stagnate))
      control$maxit.stagnate = PSOmaxit.stagnate
  }
  #Parallel Control
  else if(parallel == "YES"){
    if(!missing(PSOtrace))
      control$verbose = (PSOtrace > 0)
    if(!missing(PSOmaxf))
      control$maxfn = PSOmaxf
    if(!missing(PSOs))
      control$npart = PSOs
    if(!missing(PSOk))
      control$K = PSOk
    if(!missing(PSOw))
      control$IW.w = PSOw
    if(!missing(PSOc.p))
      control$c2 = PSOc.p
    if(!missing(PSOc.g))
      control$c1 = PSOc.g
    control$parallel = "parallelWin"
    control$write2disk = FALSE
  }
  #Common Control
  if(!missing(PSOabstol))
    control$abstol = PSOabstol
  if(!missing(PSOreltol))
    control$reltol = PSOreltol
  if(!missing(PSOREPORT))
    control$REPORT = PSOREPORT
  if(!missing(PSOmaxit))
    control$maxit = PSOmaxit
    
  TT = CreateTT(noClasses, discretization, boundNonDeterminism, rejection)
  
  if(parallel == "NO")
  {
    if(require(pso) == FALSE)
      install.packages("pso", dependencies=TRUE)
    results = psoptim(par = matrix(TT,nrow=1), fn = CalculateErrorFromVector,
                      words = sets$learn, states = noClasses, symbols = discretization,
                      rejecting = rejection, discrete = discrete, boundNonDeterminism = boundNonDeterminism,
                      minChance = minChance, lower = rep(0,length(TT)), upper = rep(1,length(TT)),  
                      control = control)
  }
  else if(parallel == "YES")
  {
    if(require(hydroPSO) == FALSE)
      install.packages("hydroPSO", dependencies=TRUE)
    results = hydroPSO(par = matrix(TT,nrow=1), fn = CalculateErrorFromVector,
                      words = sets$learn, states = noClasses, symbols = discretization,
                      rejecting = rejection, discrete = discrete, boundNonDeterminism = boundNonDeterminism,
                      minChance = minChance, lower = rep(0,length(TT)), upper = rep(1,length(TT)),  
                      control = control, method = "spso2007" )
  }
  results
}
# Phases -----
Phases <- c("a1","a2","a3","a4","a5","a6")
NonRejectingPhases <- c("a1","a3","a5")
RejectingPhases <- c("a2","a4","a6")
DeterministicPhases <- c("a1","a2")
NonDeterministicPhases <- c("a3","a4")
DiscretePhases <- c("a1","a2","a3","a4")
FuzzyPhases <- c("a5","a6")