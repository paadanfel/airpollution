complete <- function(directory, id = 1:332){
  
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
  
  nobs <- numeric(length(id))
  
  for (i in 1:length(id)) {
    
    dataImport <- read.csv(file = paste0(directory,'/',get3digit(id[i]),".csv"))
    
    nobs[i] <- sum(!is.na(dataImport[,2]))
    
  }
  
  returnData <- data.frame(cbind(id,nobs))
  
  returnData.columns <- c('id', 'nobs')
  
  returnData
  
}