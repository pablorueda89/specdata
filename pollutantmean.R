##Specdata Part 1 : pollutant means


pollutantmean<-function(directory,pollutant,id=1:332){
        
        #crear una lista de los archivos .csv del directorio 
        t<-list.files(path="C:/Users/pdrs8/Desktop/R scripts/Quizzes curso 2/specdata/specdata",pattern=".csv",all.files=TRUE)
        #leer cada uno de los archivos csv del directorio
        data_list<- lapply(t, read.csv)
        #unir todos los archivos .csv en un solo data frame
        data_cat<-do.call(rbind, data_list)
        
      
        
        directory<-("C:/Users/pdrs8/Desktop/R scripts/Quizzes curso 2/specdata/specdata")
     

       
         if(pollutant=="sulfate"){
                #subset sulfato        
                sulfato<-data_cat[,c("sulfate","ID")]
                # subset sulfato sin NAs
                good<-complete.cases(sulfato)
                sulfatoclean<-sulfato[good, ]
                sulfatomonitor<-sulfatoclean%>% filter(ID %in% id )
                s1<-mean(sulfatomonitor$sulfate)
                s1
         }
        
        else if(pollutant=="nitrate"){       
                #subset nitratos        
                nitrato<-data_cat[,c("nitrate","ID")]
                # subset nitratos sin NAs
                goodn<-complete.cases(nitrato)
                nitratoclean<-nitrato[goodn, ]
                nitratomonitor<-nitratoclean%>% filter(ID %in% id )
                s2<-mean(nitratomonitor$nitrate)
                s2
        }
        
}
        
        
        
        


##script de prueba
pollutantmean<-function(directory,pollutant,id=1:332){
        
        #crear una lista de los archivos .csv del directorio 
        t<-list.files(path="C:/Users/pdrs8/Desktop/R scripts/Quizzes curso 2/specdata/specdata",pattern=".csv",all.files=TRUE)
        #leer cada uno de los archivos csv del directorio
        data_list<- lapply(t, read.csv)
        #unir todos los archivos .csv en un solo data frame
        data_cat<-do.call(rbind, data_list)
        
        
        
        
        
        
        directory<-("C:/Users/pdrs8/Desktop/R scripts/Quizzes curso 2/specdata/specdata")
        
        
        
        if(pollutant=="sulfate"){
                
                #subset sulfato        
                sulfato<-subset(data_cat,ID==id,select = c("ID","sulfate"))
                # subset sulfato sin NAs
                good<-complete.cases(sulfato)
                sulfatoclean<-sulfato[good, ]
                s1<-mean(sulfatoclean$sulfate)
                s1
        }
        
        else if(pollutant=="nitrate"){
               
                #subset nitratos        
                nitrato<-subset(data_cat,ID==id,select = c("ID","nitrate"))
                # subset nitratos sin NAs
                goodn<-complete.cases(nitrato)
                nitratoclean<-nitrato[goodn, ]   
                s2<-mean(nitratoclean$nitrate)
                s2
        }
        
}


