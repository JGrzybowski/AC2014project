# SETS FUNCTIONS -----
## Creating sets for automata  ================================
#' Creates list with learning and testing set using given parameters.
#' 
#' @param fromFile A boolean value describing if training data should be 
#' taken from file or rather be generated using given values.
#' @param pathTrain Prameter descibing path to file with data for training set.
#' Obligatory if fromFile is TRUE.
#' @param pathTest Prameter descibing path to file with data for testing set.
#' Nonobligatory. If it is not given, the percTestSize variable will be used.
#' @param pathForeignTrain Prameter descibing path to file with foreign data for training set.
#' Nonobligatory. If it is not given, the percForeignSize variable will be used.
#' @param pathForeignTest Prameter descibing path to file with foreign data for testing set.
#' Nonobligatory. If it is not given, the percForeignSize variable will be used.
#' 
#' All of Prameters below are obligatory if fromFile is FALSE.
#' @param noClasses A paramter telling how many classes will be in generated sets. 
#' Used only if data should be generated.
#' @param noFeatures A paramter telling how many features will have elements in generated sets. 
#' Used only if data should be generated.
#' @param noRepetitionsInClass A paramter telling how many elements of each class will be in training and testing set together. 
#' Used only if data should be generated.
#' 
#' @param minRand A parameter setting minimal value of mean of feature of given class.
#' Used only if data should be generated.
#' @param maxRand A parameter setting maximal value of mean of feature of given class.
#' Used only if data should be generated.
#' @param distortion A parateter setting standard deviation from mean of values of features in each class.
#' Used only if data should be generated.
#' 
#' Parameters below are used if paths to test set and foreign sets were not given or if sets should be generated.
#' They are both obligatory parameters when fromFile is FALSE.
#' @param percTestSize Describes what percentage of training set will be used as a testing set. 
#' Value of parameter is given in percents. Meaning if percTestSize = 20, 
#' then random 20% of training set will be used as a testing set.
#' @param percForeignSize Describes how big in percentage of separetly training and testing set the foreign sets will be.
#' Value of parameter is given in percents. Meaning if percForeignSize = 20, 
#' then Foreign set of testing set will be of size 20% of the training set before adding foreign elements 
#' and similarly in case of testing set.
#' 
#' @return The list consisting of \code{learningSet} and \code{testingSet}.
#' 
#' @examples
#' #Generating sets from files
#' GenerateSets(fromFile = TRUE, 
#'          pathTrain = "InputLearningSet.xlsx", pathTest = "InputTestSet.xlsx", 
#'          pathForeignTrain = "InputForeignSet.xlsx", pathForeignTest = "InputForeignTestSet.xlsx")
#' #GeneratingSets from random data
#' GenerateSets(fromFile = FALSE, 
#'          noClasses = 10, noFeatures = 15, noRepetitionsInClass = 25,
#'          minRand = 0.5, maxRand = 50, distortion = 2.5,
#'          percTestSize = -1, percForeignSize = -1)
GenerateSets <- function(fromFile, pathTrain, pathTest, 
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
    sets = list("learn" = mainDataSet, "test" = LoadXlsxFile(pathTest,startingIndex = dim(mainDataSet)[1]))
  }
  else{
    noTestWords = (percTestSize * noWords) %/% 100
    testIndexes = sample(1:noWords,size = noTestWords)
    sets = list("learn" = mainDataSet[-testIndexes,], "test" = mainDataSet[testIndexes,])                            
  }
  
  if(is.na(match(0,sets$learn[,2])))
    sets$shift = 0
  else{
    sets$shift = -1
    sets$learn[,2] = sets$learn[,2]+1
    sets$test[,2] = sets$test[,2]+1
  }
### Obtaining the foreign train dataset  ================================
  if(fromFile && !missing(pathForeignTrain))
  {
    startingIndex = dim(sets$learn)[1] + dim(sets$test)[1]
    foreignTrain = LoadXlsxFile(pathForeignTrain,startingIndex = startingIndex)
  }
  else
  {
    noForeignTrain = (percForeignSize * dim(sets$learn)[1]) %/% 100
    foreignTrain = array(data = runif(noForeignTrain * (noFeatures+2), minRand, maxRand), dim = c(noForeignTrain,noFeatures+2))
    startingIndex = dim(sets$learn)[1] + dim(sets$test)[1]
    foreignTrain[,1] = (1:noForeignTrain)+startingIndex
  }
  foreignTrain[,2] = 0
  sets$learn = rbind(sets$learn,foreignTrain,deparse.level = 0)
### Obtaining the foreign test dataset  ================================
  if(fromFile && !missing(pathForeignTest))
  {
    foreignTest = LoadXlsxFile(pathForeignTest)
  }
  else
  {
    noForeignTest = (percForeignSize * dim(sets$test)[1]) %/% 100
    foreignTest = array(data = runif(noForeignTrain * (noFeatures+2), minRand, maxRand), dim = c(noForeignTest,noFeatures+2))
    startingIndex = dim(sets$learn)[1] + dim(sets$test)[1]
    foreignTest[,1] = (1:noForeignTest)+startingIndex
  }
  foreignTest[,2] = 0
  sets$test = rbind(sets$test,foreignTest,deparse.level = 0)  
  sets
}

# Generating the learning set ================================
#'Generates learning set from random data. 
#'This function is a subroutine for \code{GenerateSets} function.
#'
#'Parameters are similiar to the ones in \code{GenerateSets} and all are obligatory.
#' @param noClasses A paramter telling how many classes will be in generated set. 
#' @param noFeatures A paramter telling how many features will have elements in learning set. 
#' @param noRepetitionsInClass A paramter telling how many elements of each class will be in the set. 
#' @param noWords A prameter telling how many words should be in the learning set.
#' @param rangeMin A parameter setting minimal value of mean of feature of given class.
#' @param rangeMax A parameter setting maximal value of mean of feature of given class.
#' @param sigma A parateter setting standard deviation from mean of values of features in each class.
#' @return An array with values of features (collumns) for given words (rows). 
#' The first collumn holds an index of word in whole set and second is an expected class of given word,
#' next collumns store values of features.
Generate.LearningSet <- function(noClasses, noRepetitionsInClass, noFeatures, noLearnWords, rangeMin, rangeMax, sigma){
  learningSet = array(,dim = c(noLearnWords, noFeatures+2))
  for (c in 1:noClasses){
    indexesOfClass = (1:noRepetitionsInClass)+((c-1)*noRepetitionsInClass);
    learningSet[indexesOfClass,2] = c
    for(f in (1:noFeatures)+2){
      m = mean(runif(noRepetitionsInClass, min=rangeMin, max=rangeMax)) 
      learningSet[indexesOfClass,f] = m
    }
  }  
  learningSet[,1] = 1:noLearnWords
  learningSet[,c(-1,-2)] = learningSet[,c(-1,-2)] + rnorm(n=noLearnWords*noFeatures, mean=0, sd=sigma)
  learningSet
}

#'Loads a file data into a matrix which can be used in learning sets.
#'
#' @param fileName Name of the file, which will be loaded.
#' @param sheet Number of sheet which will be loaded. First one by default.
#' @param startingIndex Number starting from which the words will be indexed.
#' @return An matrix with index of word in first column, 
#' expected class in second one and values of features in all next ones.
LoadXlsxFile <- function(fileName, sheet = 1, startingIndex =1){
  if(!require(openxlsx))
    install.packages("openxlsx", dependencies = TRUE)
  tab = read.xlsx(fileName, sheet, skipEmptyRows = TRUE, colNames = FALSE)
  WordIndex = (1:dim(tab)[1])+(startingIndex-1)
  tab = cbind(WordIndex ,tab)
  as.matrix(tab, dimnames = NULL)
}

#'CHanges values of features from both sets to values from range <0,1>
#'
#' @param sets List containing $learn and $test set to be normalized.
#' @return Same list as in input but with normalized values.
Normalize <- function(sets){
  noColumns = dim(sets$learn)[2]
  for (i in ((1:noColumns)[c(-1,-2)])){
    mini = min(min(sets$learn[,i]),min(sets$test[,i]));
    maxi = max(max(sets$learn[,i]),max(sets$test[,i]));
    if(maxi == mini)
      mini=0
    sets$learn[,i] = (sets$learn[,i]-mini)/(maxi-mini);
    sets$test[,i] = (sets$test[,i]-mini)/(maxi-mini);
  }
  sets
}

#'Discretization function changing specific values into numbers of symbols
#'
#' @param x Object that need to be changed into symbols
#' @param symbols Number of symbols that will represent the values.
#' @return x with numbers from 1:symbols coresponding to values from input.
ChangeValuesToSymbols <- function(x,symbols){
  newVals = ceiling(x/(1/symbols))
  newVals[x == 0] = 1;
  newVals
}
# AUTOMATA FUNCTIONS -----
#' Creates Transition Table for Automata
#' 
#' @param states Number of states in Automata. It should be equal to number of classes.
#' @param symbols Number of symbols.
#' @param nondeterminism Maximal number of transitions from one state using one symbol. 
#' If nondeterminism is equal to Infinity it is assumed that automata is Fuzzy one and values
#' in TT are real numbers from <0,1> otherwise they are equal to 0 or 1.
#' By defualt nondeterminism = 1.
#' @param rejection Boolean variable detemining of Automata can be in rejecting state.
#' @return An array with values integer or real values from range <0,1>
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

#' Calculates next state after reading symbol from word. Simulates transition function.
#' @param TT The transition table.
#' @param inputSymbol Symbol read from word.
#' @param state State vector in which machine was when it was reading the symbol.
#' @returns Vector of states with chances of being in each of the states.
ComputeNextState <- function(TT,inputSymbol,state){
  newState = vector(mode="numeric", length = dim(TT)[1])
  for(i in 1:length(newState)){
    newState[i] = UseTriangleNorm(TT[i,,as.numeric(inputSymbol)],state)
  }
  newState
}

#' Function used to determine a value of chance of single state.
#' @param row Row from matrix which we use to calculate the norm.
#' @param column Vector of symbols which we use to calculate the sum.
#' @returns A triangular norm for given row and column
UseTriangleNorm <- function(row, column){
  result = vector(mode="numeric",length = length(column))
  for(i in 1:length(column)){
    result[i] = triMin(c(row[i], column[i]))
  }
  triMax(result)
}

#' Subroutine for calculating triangular norm.
#' @param values Values for which we calculate maximum.
#' @return A dingle value, maximum used in triangular norm.
triMax <- function(values){
  #max(values)
  values = atanh(values);
  value = tanh(sum(values))
  if(is.infinite(value))
    value= 0
  value
}

#' Subroutine for calculating triangular norm.
#' @param values Values for which we calculate minimum.
#' @return A dingle value, minimum used in triangular norm.
triMin <- function(values){
  #min(values)
  values = atanh(1-values)
  value = 1 - tanh(sum(values))
  if(is.infinite(value))
    value= 1
  value
}

#' Calculates symbols vector used in fuzzy automata
#' @param value Value of feature to be represented as a vector of chances for given symbol.
#' @param numberOfSymbols Number symbols, length of the result vector.
#' @return A vector with chances for each of the symbols.
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
#' Classifies given word. Simulates computation of Turing Machine for given word.
#' @param TT The transition table.
#' @param word The word to be classified.
#' @param discrete Bolean value telling, if Automata is discrete (TRUE) or fuzzy (FALSE).
#' @return A vector of chances for Turing Machine finishing in given state.
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

#' Calculates the error for given set of words
#' @param TT The transition table.
#' @param words The set of automata words.
#' @param minChance The minimal value of chance to treat it as an accepted.
#' @return The value of error.
CalculateError <- function(TT,words,minChance,discrete){
  error = 0;
  wordsNo = dim(words)[1]
  classesNo = dim(TT)[1]
  for(i in 1:(wordsNo)){
    #cat("#",i," word - ", words[i,], " \n");
    possibleClassification = ClassifyWord(TT,words[i,c(-1,-2)],discrete)
    expectedClass = words[i,2]
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

#' @describeIn CalculateError Does the same calculations as CalculateError.
#' The only difference is that transition table is passed as a vector and must be returned 
#' into 3D array form before performing proper calculations.
#' Only parameters different from CalculateError will be described
#' @param vTT The transition table in a vector form.
#' @param states The number of states in automata.
#' @param symbols The number of symbols in automata.
#' @param boundNonDeterminism The maximal number of destination states in one transition.
#' Infnite vale is treated as a Fuzzy automata.
#' @return The value of error.
CalculateErrorFromVector <- function(vTT,words,states,symbols,minChance,rejecting,boundNonDeterminism, discrete){
  TT = HandlePSOVector(vTT,states,symbols,rejecting,boundNonDeterminism)
  CalculateError(TT,words,minChance,discrete)
}

#' @describeIn CalculateErrorFromVector Transforms vectorized transition table from PSO 
#' into a 3D array form with proper values.
#' @param vTT The transition table in a vector form.
#' @param states The number of states in automata.
#' @param symbols The number of symbols in automata.
#' @param boundNonDeterminism The maximal number of destination states in one transition.
#' Infnite vale is treated as a Fuzzy automata.
#' @param rejecting Boolean variable detemining of Automata can be in rejecting state.
#' @return The 3D array representing transition table.
HandlePSOVector <- function(vTT,states,symbols,rejecting,boundNonDeterminism){
  TT = array(vTT,dim = c(states,states,symbols))
  if(is.infinite(boundNonDeterminism) == FALSE)
    ChangeValuesToZerosAndOnes(TT,rejecting,boundNonDeterminism)
  else
    TT
}

#' @describeIn HandlePSOVector Changes PSO values of transition table into zeros or ones.
#' @param TT The transition table.
#' @param nonDeterminism The maximal number of destination states in one transition.
#' @param rejecting Boolean variable detemining of Automata can be in rejecting state.
#' @return The 3D array representing transition table.
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


# TESTS -----
#' Function counting computation time of any fuction
#' @param fn An expression to be evaluated.
#' @return A list consisting of $time of computation and $fnResults.
TimeTest <- function(fn){
  ptm = proc.time()
  v = fn
  time =  proc.time() - ptm
  list("time" = time, "fnResults" = v)
}
#' Tests if all phases finish if sets are generated.
TestAllPhases.Generated <- function(quiet = FALSE){
  if(quiet)
    trace = 0
  else
    trace = 10
  for (phase in Phases){
    cat("",phase)
    AC2014(phase, inputType = "gen", noClasses = 2, noFeatures = 3, discretization = 4, noRepetitionsInClass = 10, 
           minRand = 0, maxRand = 25, distortion = 0.6, 
           percForeignSize = 15, percTestSize = 20, 
           parallel = "NO", PSOtrace = trace, PSOmaxit = 10)
  }
}
#' Tests if all phases finish if function AC2014 must determine if it should generate or take file data.
TestAllPhases.Autodetected <- function(quiet = FALSE){
  if(quiet)
    trace = 0
  else
    trace = 10
  for (phase in Phases){
    cat("",phase)
    AC2014(phase, noClasses = 2, noFeatures = 3, discretization = 4, noRepetitionsInClass = 10, 
           minRand = 0, maxRand = 25, distortion = 0.6, 
           percForeignSize = 15, percTestSize = 20, 
           parallel = "NO", PSOtrace = trace, PSOmaxit = 10)
  }
}
#' Tests if all phases finish if sets are given in files.
TestAllPhases.File <- function(quiet = FALSE){
  if(quiet)
    trace = 0
  else
    trace = 10
  for (phase in Phases){
    cat("",phase)
    AC2014(phase, inputType = "red", 
           pathTrain = "InputLearningSet.xlsx", pathTest = "InputTestSet.xlsx", 
           pathForeignTrain = "InputForeignSet.xlsx", pathForeignTest = "InputForeignTestSet.xlsx", 
           discretization = 5, parallel = "NO", PSOtrace = trace, PSOmaxit = 10)
  }
}
#' Runs all tests fro all phases.
TestAllPhases.All <- function(quiet = TRUE){
  cat("AutoDetection tests:")
  TestAllPhases.Autodetected(quiet)
  cat("\n Generated tests:")
  TestAllPhases.Generated(quiet)
  cat("\n File tests:")
  TestAllPhases.File(quiet)  
}
# Project running function -----
#' Main function of project. 
#' @returns A list with:
#' $optimalization data holding $time of optimalization using learning set.
#' $efficiency, number of properly clssified words, tested on testing set.
#' $TT transition table generated by PSO algorithm.
AC2014 <- function(phase, inputType = "", 
                   pathTrain, pathTest, 
                   pathForeignTrain, pathForeignTest, 
                   pathOutputClass, pathOutputErr,
                   noClasses = 0, noFeatures = 0, noRepetitionsInClass = 0,
                   minRand = -Inf, maxRand = Inf, distortion = 0,
                   percTestSize = -1, percForeignSize = -1,
                   discretization = 0, boundNonDeterminism = 0,
                   parallel = "NO",
                   PSOtrace, PSOfnscale, PSOmaxit, PSOmaxf,
                   PSOabstol, PSOreltol, PSOREPORT, PSOtrace.stats,
                   PSOs, PSOk, PSOp, PSOw, PSOc.p, PSOc.g, PSOd, PSOv.max, 
                   PSOrand.order, PSOmax.restart, PSOmaxit.stagnate) 
{

### Validate Phase dependent arguments -----
  if(missing(phase) || !(phase %in% Phases))
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
  if(missing(inputType))
    if(missing(pathTrain))
      inputType = "gen"
    else
      inputType = "red"
### Validate parameters for file input =====
  if(inputType == "red")
  {
    if(!missing(noClasses) || !missing(noFeatures) || !missing(noRepetitionsInClass) 
       || !missing(minRand) || !missing(maxRand) || !missing(distortion))
      stop("One of the parameters cannot be declared when inputType is red. Please read user's manual before using the program.")
    if(missing(pathTrain))
      stop("The path to input file must be given in parameter pathTrain!")
    if(missing(pathTest) && missing(percTestSize))
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
    noClasses = length(unique(unique(sets$learn[,2]),unique(sets$test[,2])))
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
    sets$learn[,c(-1,-2)] = ChangeValuesToSymbols(sets$learn[,c(-1,-2)], discretization)
    sets$test[,c(-1,-2)] = ChangeValuesToSymbols(sets$test[,c(-1,-2)], discretization)
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
    if(!missing(PSOmaxit.stagnate))
    {
      control$RG.miniter = PSOmaxit.stagnate
      control$use.RG = TRUE
    }
    control$parallel = "parallelWin"
    control$write2disk = FALSE
  }
  #Common Control
  if(!missing(PSOabstol))
    control$abstol = PSOabstol
  else 
    control$abstol = 0
  if(!missing(PSOreltol))
    control$reltol = PSOreltol
  if(!missing(PSOREPORT))
    control$REPORT = PSOREPORT
  if(!missing(PSOmaxit))
    control$maxit = PSOmaxit
## Running the Automata =====   
  TT = CreateTT(noClasses, discretization, boundNonDeterminism, rejection)
  
  if(parallel == "NO")
  {
    if(require(pso) == FALSE)
      install.packages("pso", dependencies=TRUE)
    tmpResults = TimeTest(psoptim(par = matrix(TT,nrow=1), fn = CalculateErrorFromVector,
                      words = sets$learn, states = noClasses, symbols = discretization,
                      rejecting = rejection, discrete = discrete, boundNonDeterminism = boundNonDeterminism,
                      minChance = minChance, lower = rep(0,length(TT)), upper = rep(1,length(TT)),  
                      control = control))
  }
  else if(parallel == "YES")
  {
    if(require(hydroPSO) == FALSE)
      install.packages("hydroPSO", dependencies=TRUE)
    tmpResults = TimeTest(hydroPSO(par = matrix(TT,nrow=1), fn = CalculateErrorFromVector,
                      words = sets$learn, states = noClasses, symbols = discretization,
                      rejecting = rejection, discrete = discrete, boundNonDeterminism = boundNonDeterminism,
                      minChance = minChance, lower = rep(0,length(TT)), upper = rep(1,length(TT)),  
                      control = control, method = "spso2007" ))
  }
  LearnResults = list("time" = as.vector(tmpResults$time)[3], "PSOresults" = tmpResults$fnResults) 

  TT = HandlePSOVector(vTT = tmpResults$fnResults$par, states = noClasses, symbols = discretization, rejecting = rejection, boundNonDeterminism = boundNonDeterminism)
  efficiency = CalculateEfficiency(TT,words = sets$test,minChance = minChance, discrete = discrete)
  

  if(!missing(pathOutputClass)){
    if(missing(pathTest))
      classes = PrepareCalssification(rbind(sets$learn, sets$test),TT,minChance,discrete,sets$shift)
    else
      classes = PrepareCalssification(sets$test,TT,minChance,discrete,sets$shift)
    GenerateReport(pathOutputClass,classes)
  }

  list("optimalization" = LearnResults, "efficiency" = efficiency, "TT" = TT)
}
# Generating Reports -----
#'Saves a file of classification.
#'@param fileName Path to result file.
#'@param set A matrix with two columns: first index of word, second with class of word.
#'@param sheetName Name of sheet in file.  By default "Classification"
GenerateReport <- function(fileName, set, sheetName = "Classification"){
  require(openxlsx)
  wb = createWorkbook(creator = "Jan Grzybowski");
  addWorksheet(wb,sheetName)
  ord = order(set[,1])
  classification = set[ord,2]
  writeData(wb, sheetName, classification, colNames = FALSE, rowNames = FALSE)
  saveWorkbook(wb,fileName,overwrite = TRUE)
}
#' Prepares a set to be saved in output classification file.
#' @param set A set of words to be classified.
#' @param TT The transition table.
#' @param minChance The minimal value of chance to treat it as an accepted.
#' @param discrete Bolean value telling, if Automata is discrete (TRUE) or fuzzy (FALSE).
#' @param shift Values telling how to shift classes numbers in case when data file had calss with number 0.
#' @return A set ready to be used in GenerateReport function.
PrepareCalssification <- function(set, TT, minChance, discrete, shift){
  words = set
  wordsNo = dim(words)[1]
  classesNo = dim(TT)[1]
  classification = matrix(NA, nrow = wordsNo, ncol = 2)
  for(i in 1:(wordsNo)){
    possibleClassification = ClassifyWord(TT,words[i,c(-1,-2)],discrete)
    expectedClass = words[i,2]
    #If the word should be rejected
    if (expectedClass==0){
      if(sum(possibleClassification<minChance) == length(possibleClassification))
        classification[i,]=c(words[i,1],0)
      else
        classification[i,]=c(words[i,1], order(possibleClassification,decreasing = TRUE)[1])
    }
    #If the word should be accepted in specific class  
    else {
      if (possibleClassification[expectedClass] >= minChance)
        classification[i,]=c(words[i,1], expectedClass)
      else
        classification[i,]=c(words[i,1], order(possibleClassification,decreasing = TRUE)[1])
    }
  }
  classification[,2] = classification[,2]+shift
  classification
}

#' Calculates the percentage of properly classified words.
#' @param TT The transition table.
#' @param words The set of words that will serve as a testing set.
#' @param minChance The minimal value of chance to treat it as an accepted.
#' @param discrete Bolean value telling, if Automata is discrete (TRUE) or fuzzy (FALSE).
#' @return The percentage of properly classified words.
CalculateEfficiency <- function(TT,words,minChance,discrete){
  hits = 0
  wordsNo = dim(words)[1]
  classesNo = dim(TT)[1]
  for(i in 1:(wordsNo)){
    possibleClassification = ClassifyWord(TT,words[i,c(-1,-2)],discrete)
    expectedClass = words[i,2]
    #If the word should be rejected
    if (expectedClass==0){
      if(sum(possibleClassification<minChance) == length(possibleClassification))
        hits = hits+1
    }
    #If the word should be accepted in specific class  
    else {
      if (possibleClassification[expectedClass] >= minChance)
        hits = hits+1
    }
  }
  hits/wordsNo
}

# Phases -----
Phases <- c("a1","a2","a3","a4","a5","a6")
NonRejectingPhases <- c("a1","a3","a5")
RejectingPhases <- c("a2","a4","a6")
DeterministicPhases <- c("a1","a2")
NonDeterministicPhases <- c("a3","a4")
DiscretePhases <- c("a1","a2","a3","a4")
FuzzyPhases <- c("a5","a6")
# Constants -----
# Column.WordIndex <- 1
# Column.ExpectedClass <- 2
# Column.AddtionalColumns <- c(Column.WordIndex, Column.ExpectedClass)
# Column.NoAdditionalColumns <- length(Column.AddtionalColumns)
# Column.Features <- c(-Column.WordIndex, -Column.ExpectedClass)
