pollutantmean <- function(directory, pollutant, id = 1:332){
  
  get3digit <- function(i){
    strId <- if(i > 99){
      
      paste0(i)
      
    }else if(i > 9 && i < 100){
      
      paste0("0",i)
      
    }else{
      
      paste0("00",i)
      
    }
    strId
  }
  
  numberOfElements <- 0
  
  sumPollutant <- 0
  
  for (i in id) {
    
    dataImport <- read.csv(file = paste0(directory,'/',get3digit(i),".csv"))
    
    numberOfElements <- numberOfElements + sum(!is.na(dataImport[pollutant]))
    
    sumPollutant <- sumPollutant + sum(dataImport[pollutant][!is.na(dataImport[pollutant])])
    
  }
  
  sumPollutant/numberOfElements
  
}