# REPORTING -----
Report.noInstances <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.noInstances$name)
  for(i in 1:length(values)){
    noInstances = values[i]
    noTry = match(i, 1:length(values))
    results = AC2014("a6", inputType = "gen", noClasses = 10, noFeatures = 5, 
                     discretization = 5, noRepetitionsInClass = noInstances, 
                     minRand = 0, maxRand = 25, distortion = 1, 
                     percForeignSize = 15, percTestSize = 20, 
                     parallel = "YES", PSOmaxit = 1000)
    
    results = list("value" = noInstances, "time" = as.vector(results$optimalization$time), "error" = results$optimalization$PSOresults$value, "efficiency" = results$efficiency)
    writeData(wb,sheet = Sheets.noInstances$name,results,1,noTry,
              colNames = F,rowNames=F)
  }
  saveWorkbook(wb,"Report.noInstances.xlsx",overwrite = TRUE)
}

Report.noClasses <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.noClasses$name)
  for(i in 1:length(values)){
    noClasses = values[i]
    noTry = match(i, 1:length(values))
    results = AC2014("a6", inputType = "gen", noClasses = noClasses, noFeatures = 3, 
                     discretization = 5, noRepetitionsInClass = 15, 
                     minRand = 0, maxRand = 25, distortion = 1, 
                     percForeignSize = 15, percTestSize = 20, 
                     parallel = "YES", PSOmaxit = 1000)
    
    results = list("value" = noClasses, "time" = as.vector(results$optimalization$time), "error" = results$optimalization$PSOresults$value, "efficiency" = results$efficiency)
    writeData(wb,sheet = Sheets.noClasses$name,results,1,noTry,
              colNames = F, rowNames = F)
  }
  saveWorkbook(wb,"Report.noInstances.xlsx",overwrite = TRUE)
}

Report.discretization <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.discretization$name)
  for(i in 1:length(values)){
    discretization = values[i]
    noTry = match(i, 1:length(values))
    results = AC2014("a6", inputType = "gen", noClasses = 10, noFeatures = 5, 
                     discretization = discretization, noRepetitionsInClass = 15, 
                     minRand = 0, maxRand = 25, distortion = 1, 
                     percForeignSize = 15, percTestSize = 20, 
                     parallel = "YES", PSOmaxit = 10)
    
    results = list("value" = discretization, "time" = as.vector(results$optimalization$time), "error" = results$optimalization$PSOresults$value, "efficiency" = results$efficiency)
    writeData(wb,sheet = Sheets.discretization$name,results,1,noTry,
              colNames = F, rowNames = F)
  }
  saveWorkbook(wb,"Report.discretization.xlsx",overwrite = TRUE)
}

Report.maxit <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.maxit$name)
  for(i in 1:length(values)){
    maxit = values[i]
    noTry = match(i, 1:length(values))
    results = AC2014("a6", inputType = "gen", noClasses = 10, noFeatures = 5, 
                     discretization = 5, noRepetitionsInClass = 15, 
                     minRand = 0, maxRand = 25, distortion = 1, 
                     percForeignSize = 15, percTestSize = 20, 
                     parallel = "YES", PSOmaxit = maxit)
    
    results = list("value" = maxit, "time" = as.vector(results$optimalization$time), "error" = results$optimalization$PSOresults$value, "efficiency" = results$efficiency)
    writeData(wb,sheet = Sheets.maxit$name,results,1,noTry,
              colNames = F, rowNames = F)
  }
  saveWorkbook(wb,"Report.maxit.xlsx",overwrite = TRUE)
}

Report.parallelism <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.parallelism$name)
  for(i in 1:length(values)){
    parallelism = values[i]
    noTry = match(i, 1:length(values))
    results = AC2014("a6", inputType = "gen", noClasses = 10, noFeatures = 5, 
                     discretization = 5, noRepetitionsInClass = 15, 
                     minRand = 0, maxRand = 25, distortion = 1, 
                     percForeignSize = 15, percTestSize = 20, 
                     parallel = "NO", PSOmaxit = 100)
    
    results = list("value" = parallelism, "time" = as.vector(results$optimalization$time), "error" = results$optimalization$PSOresults$value, "efficiency" = results$efficiency)
    writeData(wb,sheet = Sheets.parallelism$name,results,1,noTry,
              colNames = F, rowNames = F)
  }
  saveWorkbook(wb,"Report.parallelism.xlsx",overwrite = TRUE)
}

Report.basicPSOparams <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.basicPSOparams$name)
  for(i in 1:length(values)){
    basicPSOparams = values[i]
    noTry = match(i, 1:length(values))
    results = AC2014("a6", inputType = "gen", noClasses = 10, noFeatures = 5, 
                     discretization = 5, noRepetitionsInClass = 15, 
                     minRand = 0, maxRand = 25, distortion = 1, 
                     percForeignSize = 15, percTestSize = 20, 
                     parallel = "NO", PSOmaxit = 100)
    
    results = list("value" = basicPSOparams, "time" = as.vector(results$optimalization$time), "error" = results$optimalization$PSOresults$value, "efficiency" = results$efficiency)
    writeData(wb,sheet = Sheets.basicPSOparams$name,results,1,noTry,
              colNames = F, rowNames = F)
  }
  saveWorkbook(wb,"Report.basicPSOparams.xlsx",overwrite = TRUE)
}

Report.ForeignElements <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.ForeignElements$name)
  for(i in 1:length(values)){
    ForeignElements = values[i]
    noTry = match(i, 1:length(values))
    results = AC2014("a6", inputType = "gen", noClasses = 10, noFeatures = 5, 
                     discretization = 5, noRepetitionsInClass = 15, 
                     minRand = 0, maxRand = 25, distortion = 1, 
                     percForeignSize = ForeignElements, percTestSize = 20, 
                     parallel = "NO", PSOmaxit = 100)
    
    results = list("value" = ForeignElements, "time" = as.vector(results$optimalization$time), "error" = results$optimalization$PSOresults$value, "efficiency" = results$efficiency)
    writeData(wb,sheet = Sheets.ForeignElements$name,results,1,noTry,
              colNames = F, rowNames = F)
  }
  saveWorkbook(wb,"Report.ForeignElements.xlsx",overwrite = TRUE)
}

Report.nondeterminism <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.nondeterminism$name)
  for(i in 1:length(values)){
    nondeterminism = values[i]
    noTry = match(i, 1:length(values))
    results = AC2014("a4", inputType = "gen", noClasses = 10, noFeatures = 5, 
                     discretization = 5, noRepetitionsInClass = 15, 
                     minRand = 0, maxRand = 25, distortion = 1, boundNonDeterminism = nonDeterminism,
                     percForeignSize = 20, percTestSize = 20, 
                     parallel = "NO", PSOmaxit = 100)
    
    results = list("value" = nondeterminism, "time" = as.vector(results$optimalization$time), "error" = results$optimalization$PSOresults$value, "efficiency" = results$efficiency)
    writeData(wb,sheet = Sheets.nondeterminism$name,results,1,noTry,
              colNames = F, rowNames = F)
  }
  saveWorkbook(wb,"Report.nondeterminism.xlsx",overwrite = TRUE)
}

# CreateFile <- function(fileName, sheet, fn){
#   wb = createWorkbook(creator = "Jan Grzybowski")
#   addWorksheet(wb,sheetName = Sheets.noClasses$name)
#   for(i in 1:length(values)){
#     noClasses = values[i]
#     noTry = match(i, 1:length(values))
#     results = fn
#     writeData(wb,sheet = sheet$name,results,1,noTry,
#               colNames = F, rowNames = F)
#   }
#   saveWorkbook(wb, fileName, overwrite = TRUE)
#   results
# }
# # SaveToFile 
Report.All <- function(){
  Report.noInstances(c(5,10,20,50,100))
  Report.noClasses(c(5,10,20,50))
  Report.maxit(c(25,50,100,1000,10000))
  Report.ForeignElements(c(10,20,50,75))
  Report.discretization(c(3,5,10,15))
  Report.nondeterminism(c(1,2,3,4,5))
}

Sheets.noInstances <- list("name" = "noInstances", page = 1)
Sheets.noClasses <- list("name" = "noClasses", page = 2)
Sheets.discretization <- list("name" = "discretization", page = 3)
Sheets.maxit <- list("name" = "maxit", page = 4)
Sheets.paralellism <- list("name" = "parallelism", page = 5)
Sheets.basicPSOparams <- list("name" = "basicPSOparams", page = 6)
Sheets.ForeignElements <- list("name" = "ForeignElements", page = 7)
Sheets.nonDeterminism <- list("name" = "nonDeterminism", page = 8)


