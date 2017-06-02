getwd()
setwd("C:/Users/JUAN M/Documents/GitHub/programacion_actuarial_III/Hospital")

mejor <- function(estado, resultado) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    C <- FALSE
    
    
    if(resultado== "ataque"){
        col <- 11
    }
    else if (resultado== "falla"){
        col <- 17
    }
    else {
        col <- 23
    }
    
    
    a <- matrix(data[,col],4706,1)
    data[,col] <- suppressWarnings(as.numeric(a))
    data[,2] <- as.character(data[,2])
    
    
    b <- data[grep(estado,data$State),]
    Orden <- b[order(b[,col], b[,2], na.last=NA),]
    
    
    if (!((resultado == "ataque")|(resultado == "falla")| (resultado == "neumonia"))){
        stop("resultado inválido")
        
    }
    
    e <- matrix(data[,7], 4706,1)
    
    for (i in 1:length(e))
        if(estado == e[i]){
            C <- TRUE
        }
    if(!C){
        stop("Estado inválido")
    }
    
    Orden[1,2]
}

mejor("TX","neumonia")
mejor("MD","ataque")
mejor("TX","falla")
mejor("BB","ataque")

