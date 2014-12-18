# REPORTING -----
Report.noInstances <- function(values){
  wb = createWorkbook(creator = "Jan Grzybowski")
  addWorksheet(wb,sheetName = Sheets.noInstances$name)
  for(i in 1:length(values)){
    noInstances = values[i]
    noTry = match(i, 1:length(values))
    results = TimeTest(AC2014("a6", inputType = "gen", noClasses = 3, noFeatures = 3, 
                            discretization = 4, noRepetitionsInClass = noInstances, 
                            minRand = 0, maxRand = 25, distortion = 1, 
                            percForeignSize = 15, percTestSize = 20, 
                            parallel = "YES", PSOtrace = 1, PSOmaxit = 10))
    
    results = list("value" = noInstances, "time" = as.vector(results$time)[3], "error" = results$fnResults$value)
    writeData(wb,sheet = Sheets.noInstances$name,results,1,noTry,
              colNames = T,rowNames=T)
  }
  saveWorkbook(wb,"Report.noInstances.xlsx",overwrite = TRUE)
}









Sheets.noInstances <- list("name" = "noInstances", page = 1)