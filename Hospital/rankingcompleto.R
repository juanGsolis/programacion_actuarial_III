
getwd()
setwd("C:/Users/JUAN M/Documents/GitHub/programacion_actuarial_III/Hospital")

rankingcompleto<- function(resultado, num){
    
    
    data<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    m<-nrow(data)
    estado<- unique(data[,7])
    hospital<- vector("character", 54)
    
    
    if(resultado == "ataque"){
        col<- 11
    } else if(resultado == "falla"){
        col<- 17
    }else if (resultado=="neumonia"){
        col<- 23
    }else { 
        col<-2
    }
    
    if (col>10){
        for(i in 1:54){
            p<-estado[i]
            vector1<-vector("numeric")
            vector2<-vector("numeric")
            x<-0
            for (n in 1:m){
                if(data[n,7]== p){
                    x<-length(vector1) +1
                    length(vector1)<-x
                    length(vector2)<-x
                    vector1[x]<-data[n,2]
                    vector2[x]<-data[n,col]
                    
                }
                
            }
            
            warnin<-getOption("warn")
            options(warn = -1)
            a<-as(vector2,"numeric")
            options(warn = warnin)
            b<-data.frame(vector1,a, stringsAsFactors = FALSE)
            c<-b[order(a,vector1),]
            
            if(num=="mejor"){
                hospital[i]<-c[1,1]
                
            }else if (num== "peor"){
                casos<- nrow(c[complete.cases(c),])
                hospital[i]<-c[casos,1]
                
            }else{ 
                hospital[i]<-c[num,1]
                
            }
            
        }
        
        r<-data.frame(hospital,estado, stringsAsFactors = FALSE)
        s<-r[order(estado,hospital),]
        s
    }else{
        "resultado invalido"
        
    }
}    

head(rankingcompleto("ataque",20),10)
tail(rankingcompleto("neumonia","peor"),3)
tail(rankingcompleto("neumonia","peor"),3)