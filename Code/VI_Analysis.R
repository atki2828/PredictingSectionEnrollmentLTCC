##### Variable Importance
library(dplyr)
library(tidyr)
library(glmnet)
library(randomForest)
######## SECTION LOOK 
setwd("~/Math-533")
library(glmnet)
library(ggplot2)
library(neuralnet)
library(fastDummies)
library(ggthemes)
library(stringr)
library(matrixStats)
Sect <- read.csv('SECTT1.CSV',header = T)
DT <- read.csv('Sec_DT.csv', header = T)


day.combos <-  Vectorize(function(i){combn(colnames(DT[,3:9]),i)},'i') (i=1:7)
col.in = 1
dstore = matrix(0,nrow= nrow(DT), ncol=128)
c.names = character(128)

for( i in length(day.combos):1){
  for(j in 1:ncol(day.combos[[i]])){
    
    if (col.in ==1){
      dstore[,col.in] <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1
    }
    if(col.in==2){
      rprod <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      new.col <- ifelse(dstore[,1] ==1,0,rprod)
      dstore[,col.in] <- new.col
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1   
    }else{
      rprod <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      new.col <- ifelse(rowSums(dstore[,1:col.in-1]) ==1,0,rprod)
      dstore[,col.in] <- new.col
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1
    }
  }
}
df <- as.data.frame(dstore)
colnames(df) <- c.names

DT <- cbind(DT,df)
DT <- DT[,-c(3:9)]  




Enroll <- inner_join(Sect,DT, by= c('SEC_NAME','TERM'))
C4P <- read.csv('C4P.csv', header = T)
C4P <- C4P %>% dplyr::rename('Acad_Year' = 'TERM_REPORTING_YEAR')


Enroll <- left_join(Enroll,C4P,by = c('COURSES_ID' ,'CRS_NAME', "Acad_Year" ))
Enroll$C4AP <- replace_na(Enroll$C4AP,0)

Enroll <- Enroll %>% mutate(Quarter = as.factor(substr(TERM,5,6)))
Enroll$Sec_NO <- as.numeric(substr(Enroll$SEC_NAME,(str_length(Enroll$SEC_NAME)-1),str_length(Enroll$SEC_NAME)))
#Enroll$AvgSecEnrto <- NULL 
#Enroll$Count_PY <-NULL
Enroll$AvgSecEnr3yr <- NULL
#Enroll$SEC_CAPACITY <- NULL
#Enroll <- Enroll %>% mutate(Dept = as.factor(substr(CRS_NAME,1,3)))
#Enroll$CRS_NAME <- str_remove(Enroll$CRS_NAME,"-")

#Enroll$CRS_NAME <- trimws(Enroll$CRS_NAME)
#Enroll <- Enroll %>% mutate(CRS = as.factor(str_remove(CRS_NAME,'CRS_NAME_')))
Enroll$CRS_NAME <- as.factor(Enroll$CRS_NAME)
#Enroll$SEC_NAME <- as.factor(Enroll$SEC_NAME)


Vars = colnames(Enroll[,-c(1,3,4,6,8)])
Enroll <- Enroll[,Vars[c(1,3,4,2,5:length(Vars))]]
Enroll <- dummy_cols(Enroll)
Enroll <- Enroll %>% select_if(substr(colnames(Enroll),1,5) != 'TERM_')


X <- Enroll[,-c(1:3,170)]
X[,1:8] <- scale(X[,1:8],center = T,scale = T)
X <- as.matrix(X)
Y <- Enroll[,3]
Y <- as.matrix(scale(Y,center = T, scale = T))
library(glmnet)
out <- cv.glmnet(X,Y,alpha =1, intercept = F,type.measure = 'mse')
best.lam <- out$lambda.min
lmod <- glmnet(X , Y , alpha = 1 ,lambda = best.lam ,intercept = F)

coeffs <- as.vector(coef(lmod))
coef.df <- data.frame(Predictor = rownames(coef(lmod)),C.value =coeffs )%>% 
           filter(C.value != 0) %>%
           arrange(desc(C.value))


coef.df0 <- data.frame(Predictor = rownames(coef(lmod)),C.value =coeffs )%>% 
  filter(C.value == 0) %>%
  arrange(desc(C.value))

plot(out,)

#### Variable Importance RF 

library(randomForest)
library(fastDummies)
Sect <- read.csv('SECTT1.CSV',header = T)
DT <- read.csv('Sec_DT.csv', header = T)
day.combos <-  Vectorize(function(i){combn(colnames(DT[,3:9]),i)},'i') (i=1:7)
col.in = 1
dstore = matrix(0,nrow= nrow(DT), ncol=128)
c.names = character(128)


for( i in length(day.combos):1){
  for(j in 1:ncol(day.combos[[i]])){
    
    if (col.in ==1){
      dstore[,col.in] <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1
    }
    
    if(col.in==2){
      rprod <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      new.col <- ifelse(dstore[,1] ==1,0,rprod)
      dstore[,col.in] <- new.col
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1   
    }else{
      rprod <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      new.col <- ifelse(rowSums(dstore[,1:col.in-1]) ==1,0,rprod)
      dstore[,col.in] <- new.col
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1
    }
  }
}
df <- as.data.frame(dstore)
colnames(df) <- c.names

DT <- cbind(DT,df)
DT <- DT[,-c(3:9)]  



Enroll <- inner_join(Sect,DT, by= c('SEC_NAME','TERM'))
Enroll <- Enroll %>% mutate(Quarter = as.factor(substr(TERM,5,6))) %>% 
  arrange(CRS_NAME, Acad_Year,Quarter)
Enroll <- Enroll %>% mutate(Dept = substr(CRS_NAME,1,3))

C4P <- read.csv('C4P.csv', header = T)
C4P <- C4P %>% dplyr::rename('Acad_Year' = 'TERM_REPORTING_YEAR')


Enroll <- left_join(Enroll,C4P,by = c('COURSES_ID' ,'CRS_NAME', "Acad_Year" ))
Enroll$C4AP <- replace_na(Enroll$C4AP,0)

new <- dummy_cols(Enroll$CRS_NAME)
colnames(new) <- str_remove(colnames(new),"\\.")
colnames(new) <- str_remove(colnames(new),"-")
colnames(new) <- str_remove(colnames(new),"data_")
Enroll <- cbind(Enroll,new[,-1])
Enroll$Sec_NO <- as.numeric(substr(Enroll$SEC_NAME,(str_length(Enroll$SEC_NAME)-1),str_length(Enroll$SEC_NAME)))
Vars = colnames(Enroll[,-c(1:4,6:8,16)])
Enroll <- Enroll[,Vars]
f <- as.formula(ACTIVE_STUDENT_COUNT~.)
fit.rf <- randomForest(f,data = Enroll ,nodesize = 10, importance= T, ntree=1300 )

rf.VI <- data.frame(Var = rownames(fit.rf$importance), Perc.MSE = fit.rf$importance[,1]) %>%
          filter(Perc.MSE != 0) %>% arrange(desc(Perc.MSE))




###VI XGboost
library(xgboost)
##### Variable Importance
library(dplyr)
library(tidyr)
library(glmnet)
library(randomForest)
######## SECTION LOOK 
setwd("~/Math-533")
library(glmnet)
library(ggplot2)
library(neuralnet)
library(fastDummies)
library(ggthemes)
library(stringr)
library(matrixStats)
Sect <- read.csv('SECTT1.CSV',header = T)
DT <- read.csv('Sec_DT.csv', header = T)


day.combos <-  Vectorize(function(i){combn(colnames(DT[,3:9]),i)},'i') (i=1:7)
col.in = 1
dstore = matrix(0,nrow= nrow(DT), ncol=128)
c.names = character(128)

for( i in length(day.combos):1){
  for(j in 1:ncol(day.combos[[i]])){
    
    if (col.in ==1){
      dstore[,col.in] <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1
    }
    if(col.in==2){
      rprod <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      new.col <- ifelse(dstore[,1] ==1,0,rprod)
      dstore[,col.in] <- new.col
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1   
    }else{
      rprod <- rowProds(as.matrix(DT[,day.combos[[i]][,j]]))
      new.col <- ifelse(rowSums(dstore[,1:col.in-1]) ==1,0,rprod)
      dstore[,col.in] <- new.col
      c.names[col.in] <- paste(substr(day.combos[[i]][,j],1,3),collapse = "")
      col.in = col.in +1
    }
  }
}
df <- as.data.frame(dstore)
colnames(df) <- c.names

DT <- cbind(DT,df)
DT <- DT[,-c(3:9)]  




Enroll <- inner_join(Sect,DT, by= c('SEC_NAME','TERM'))
C4P <- read.csv('C4P.csv', header = T)
C4P <- C4P %>% dplyr::rename('Acad_Year' = 'TERM_REPORTING_YEAR')


Enroll <- left_join(Enroll,C4P,by = c('COURSES_ID' ,'CRS_NAME', "Acad_Year" ))
Enroll$C4AP <- replace_na(Enroll$C4AP,0)

Enroll <- Enroll %>% mutate(Quarter = as.factor(substr(TERM,5,6)))
Enroll$Sec_NO <- as.numeric(substr(Enroll$SEC_NAME,(str_length(Enroll$SEC_NAME)-1),str_length(Enroll$SEC_NAME)))
#Enroll$AvgSecEnrto <- NULL 
#Enroll$Count_PY <-NULL
Enroll$AvgSecEnr3yr <- NULL
#Enroll$SEC_CAPACITY <- NULL
#Enroll <- Enroll %>% mutate(Dept = as.factor(substr(CRS_NAME,1,3)))
#Enroll$CRS_NAME <- str_remove(Enroll$CRS_NAME,"-")

#Enroll$CRS_NAME <- trimws(Enroll$CRS_NAME)
#Enroll <- Enroll %>% mutate(CRS = as.factor(str_remove(CRS_NAME,'CRS_NAME_')))
Enroll$CRS_NAME <- as.factor(Enroll$CRS_NAME)
#Enroll$SEC_NAME <- as.factor(Enroll$SEC_NAME)


Vars = colnames(Enroll[,-c(1,3,4,6,8)])
Enroll <- Enroll[,Vars[c(1,3,4,2,5:length(Vars))]]
Enroll <- dummy_cols(Enroll)
Enroll <- Enroll %>% select_if(substr(colnames(Enroll),1,5) != 'TERM_')


Train.x <- Enroll[,-c(1,2,170)]
f <- as.formula(ACTIVE_STUDENT_COUNT~.)
Train.x <- model.matrix(f, Train.x)[,-1]
Train.y <- Enroll$ACTIVE_STUDENT_COUNT

params <- list(booster = "gbtree", eta=0.4, gamma=0, alpha=4,
               max_depth=8, min_child_weight=1, subsample=1, colsample_bytree=1)

fit.boost <- xgboost(params = params, data=Train.x,label = Train.y,nfold = 10, nrounds = 300)


mat <- xgb.importance(feature_names = colnames(Train.x),model = fit.boost)
xgb.plot.importance(importance_matrix = mat[1:10]) 


rf.VI$Var[1:10] <- c('PYEnroll','AvgEnroll','SSPY','Capacity','C4AP',
                     'SecCount','OnlineED','F2F','Dept','DropsPY')

RFVI.plot <- ggplot(rf.VI[1:10,]  
                    ,aes(Var, y =Perc.MSE  , fill=Var)) + 
                    geom_bar( stat = 'identity')+ ylim(c(0,17))+
                    ylab('Percent Increase MSE')+
                    theme_base() + 
                    geom_text(aes(label=round(Perc.MSE,2)), position=position_dodge(width=0.9), vjust=-0.25)+
                    ggtitle('Top 10 Percent Increse in MSE from Random Forest')
print(RFVI.plot)



