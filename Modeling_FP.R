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
#Enroll <- Enroll %>% filter(Quarter != 'SU')

Train.x <- Enroll[which(!Enroll$TERM %in% c('2018SU','2018FA','2019WI','2019SP')),]
Test.xSU  <- Enroll[which(Enroll$TERM  =='2018SU'),]
Test.xFA  <- Enroll[which(Enroll$TERM  =='2018FA'),]
Test.xWI  <- Enroll[which(Enroll$TERM  =='2019WI'),]
Test.xSP  <- Enroll[which(Enroll$TERM  =='2019SP'),]

Train.x <- Train.x %>% select_if(is.numeric)
Test.xSU <- Test.xSU %>% select_if(is.numeric)
Test.xFA <- Test.xFA %>% select_if(is.numeric)
Test.xWI <- Test.xWI %>% select_if(is.numeric)
Test.xSP <- Test.xSP %>% select_if(is.numeric)

Train.y <- Train.x$ACTIVE_STUDENT_COUNT
Test.ySU <- Test.xSU$ACTIVE_STUDENT_COUNT
Test.yFA <- Test.xFA$ACTIVE_STUDENT_COUNT
Test.yWI <- Test.xWI$ACTIVE_STUDENT_COUNT
Test.ySP <- Test.xSP$ACTIVE_STUDENT_COUNT

colnames(Train.x) <- str_remove(colnames(Train.x), '-')
colnames(Test.xSU) <- str_remove(colnames(Test.xSU), '-')
colnames(Test.xFA) <- str_remove(colnames(Test.xFA), '-')
colnames(Test.xWI) <- str_remove(colnames(Test.xWI), '-')
colnames(Test.xSP) <- str_remove(colnames(Test.xSP), '-')



f <- as.formula(ACTIVE_STUDENT_COUNT~.     )




lm.mod <- lm(f,Train.x) 

predSU <- round(predict(lm.mod ,Test.xSU)) 
predFA <- round(predict(lm.mod ,Test.xFA))
predWI <- round(predict(lm.mod ,Test.xWI)) 
predSP <- round(predict(lm.mod ,Test.xSP))

mse.lmSU <- sum((predSU-Test.ySU)^2)/length(predSU) 
mse.lmFA <- sum((predFA-Test.yFA)^2)/length(predFA) 
mse.lmWI <- sum((predWI-Test.yWI)^2)/length(predWI) 
mse.lmSP <- sum((predSP-Test.ySP)^2)/length(predSP) 

Train.x <- model.matrix(f, Train.x)[,-1] 
Test.xSU <- model.matrix(f, Test.xSU)[,-1] 
Test.xFA <- model.matrix(f, Test.xFA)[,-1] 
Test.xWI <- model.matrix(f, Test.xWI)[,-1] 
Test.xSP <- model.matrix(f, Test.xSP)[,-1] 

#CreateModel
fit.lasso <- glmnet(Train.x, Train.y, family="gaussian", alpha=1,intercept =T) 
glm.net.predictionsSU <- round(predict(fit.lasso ,newx = Test.xSU)) 
glm.net.predictionsFA <- round(predict(fit.lasso ,newx = Test.xFA))
glm.net.predictionsWI <- round(predict(fit.lasso ,newx = Test.xWI))
glm.net.predictionsSP <- round(predict(fit.lasso ,newx = Test.xSP))


msesSU <- apply(glm.net.predictionsSU,2,function(x){sum((x-Test.ySU)^2)/length(Test.ySU)})
msesFA <- apply(glm.net.predictionsFA,2,function(x){sum((x-Test.yFA)^2)/length(Test.yFA)})
msesWI <- apply(glm.net.predictionsWI,2,function(x){sum((x-Test.yWI)^2)/length(Test.yWI)})
msesSP <- apply(glm.net.predictionsSP,2,function(x){sum((x-Test.ySP)^2)/length(Test.ySP)})

mses.las.SU <- msesSU[which.min(msesSU)]
mses.las.FA <- msesFA[which.min(msesFA)]
mses.las.WI <- msesWI[which.min(msesWI)]
mses.las.SP <- msesSP[which.min(msesSP)]

#glm.net.predictions <- round(glm.net.predictions*(mx[1]-mi[1])+mi[1])
Sect <- read.csv('SECTT1.CSV',header = T)
DT <- read.csv('Sec_DT.csv', header = T)
Enroll <- inner_join(Sect,DT, by= c('SEC_NAME','TERM'))
Enroll <- Enroll %>% mutate(Quarter = as.factor(substr(TERM,5,6))) 
Enroll <- Enroll %>% mutate(Dept = as.factor(substr(CRS_NAME,1,3)))


Test.xSU  <- Enroll[which(Enroll$TERM  =='2018SU'),]
Test.xFA  <- Enroll[which(Enroll$TERM  =='2018FA'),]
Test.xWI  <- Enroll[which(Enroll$TERM  =='2019WI'),]
Test.xSP  <- Enroll[which(Enroll$TERM  =='2019SP'),]


check.las.SU <- data.frame(
                Course = Test.xSU$CRS_NAME,
                Prediction = glm.net.predictionsSU[,which.min(msesSU)],
                Actual = Test.ySU,
                Sec.name = Test.xSU$SEC_NAME,
                PY= Test.xSU$Count_PY)
check.las.SU  <- check.las.SU  %>% mutate(Residual = abs(Actual-Prediction) ,
                                          Residual2 =Actual-Prediction)

check.las.FA <- data.frame(
                Course = Test.xFA$CRS_NAME,
                Prediction = glm.net.predictionsFA[,which.min(msesFA)],
                Actual = Test.yFA,
                Sec.name = Test.xFA$SEC_NAME,
                 PY= Test.xFA$Count_PY)
check.las.FA  <- check.las.FA  %>% mutate(Residual = abs(Actual-Prediction) ,
                                          Residual2 =Actual-Prediction)

check.las.WI <- data.frame(
                Course = Test.xWI$CRS_NAME,
                Prediction = glm.net.predictionsWI[,which.min(msesWI)],
                Actual = Test.yWI,
                Sec.name = Test.xWI$SEC_NAME,
                PY= Test.xWI$Count_PY)
check.las.WI  <- check.las.WI  %>% mutate(Residual = abs(Actual-Prediction) ,
                                          Residual2 =Actual-Prediction)


check.las.SP <- data.frame(
                    Course = Test.xSP$CRS_NAME,
                    Prediction = glm.net.predictionsSP[,which.min(msesSP)],
                    Actual = Test.ySP,
                    Sec.name = Test.xSP$SEC_NAME,
                    PY= Test.xSP$Count_PY)
check.las.SP  <- check.las.SP  %>% mutate(Residual = abs(Actual-Prediction) ,
                                          Residual2 =Actual-Prediction)




#hist(glm.net.predictions[,which.min(mses)] - Test.y)
#sum(glm.net.predictions[,which.min(mses)])
#sum(Test.y)
#sum(glm.net.predictions[,which.min(mses)])
#model2 <-lm(Test.y~glm.net.predictions[,which.min(mses)])
#line= model2$coefficients[1] + seq(0,60)*model2$coefficients[2]
#plot(glm.net.predictions[,which.min(mses)],Test.y, ylim = c(0,100),xlim=c(0,100))
#abline(a=0,b=1)
#lines(line)
#print(mses[which.min(mses)])
#coef(fit.lasso)[,which.min(mses)]




fit.plot.lrSU <- ggplot(data = check.las.SU , aes(x=Prediction,y=Actual))+
                geom_point(aes(color= Residual))+ 
                geom_text(data = check.las.SU %>% filter(Residual > quantile(Residual,0.95)),
                aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
                xlim(0,40)+
                ylim(0,40)+
                geom_abline(intercept = 0) +
                theme_base()+
                labs(title=paste(c('2018SU' ,': Lasso Regression Actual Vs Predicted'),collapse=''),
                subtitle = paste('MSE =',as.character(round(msesSU[which.min(msesSU)],2)),collapse = ' '),
                caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.las.SU$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.lrSU)


fit.plot.lrFA <- ggplot(data = check.las.FA , aes(x=Prediction,y=Actual))+
                 geom_point(aes(color= Residual))+ 
                 geom_text(data = check.las.FA %>% filter(Residual > quantile(Residual,0.95)),
                 aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
                 xlim(0,40)+
                 ylim(0,40)+
                 geom_abline(intercept = 0) +
                 theme_base()+
                 labs(title=paste(c('2018FA' ,': Lasso Regression Actual Vs Predicted'),collapse=''),
                 subtitle = paste('MSE =',as.character(round(msesFA[which.min(msesFA)],2)),collapse = ' '),
                 caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.las.FA$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.lrFA)


fit.plot.lrWI <- ggplot(data = check.las.WI , aes(x=Prediction,y=Actual))+
                    geom_point(aes(color= Residual))+ 
                    geom_text(data = check.las.WI %>% filter(Residual > quantile(Residual,0.95)),
                            aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
                    geom_abline(intercept = 0) +
                    xlim(0,40)+
                    ylim(0,40)+
                    theme_base()+
                    labs(title=paste(c('2019WI' ,': Lasso Regression Actual Vs Predicted'),collapse=''),
                    subtitle = paste('MSE =',as.character(round(msesWI[which.min(msesWI)],2)),collapse = ' '),
                    caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                      round(quantile(check.las.WI$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.lrWI)


fit.plot.lrSP <- ggplot(data = check.las.SP , aes(x=Prediction,y=Actual))+
                 geom_point(aes(color= Residual))+ 
                 geom_text(data = check.las.SP %>% filter(Residual > quantile(Residual,0.95)),
                 aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
                 geom_abline(intercept = 0) +
                 xlim(0,40)+
                 ylim(0,40)+
                 theme_base()+
                 labs(title=paste(c('2019SP' ,': Lasso Regression Actual Vs Predicted'),collapse=''),
                 subtitle = paste('MSE =',as.character(round(msesSP[which.min(msesSP)],2)),collapse = ' '),
                 caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.las.SP$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.lrSP)





#### SEction Forest

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


Test.Terms = c('2018SU','2018FA','2019WI','2019SP')

Train.x <- Enroll[which(!Enroll$TERM %in% c('2018SU','2018FA','2019SP', '2019WI') ) ,Vars]
Train.y <-  Enroll[which(!Enroll$TERM %in% c('2018SU','2018FA','2019SP', '2019WI') ) ,]$ACTIVE_STUDENT_COUNT


Test.xSU  <- Enroll[which(Enroll$TERM  =='2018SU'),]
Test.xFA  <- Enroll[which(Enroll$TERM  =='2018FA'),]
Test.xWI  <- Enroll[which(Enroll$TERM  =='2019WI'),]
Test.xSP  <- Enroll[which(Enroll$TERM  =='2019SP'),]

Test.ySU <-  Enroll[which(Enroll$TERM  =='2018SU'),]$ACTIVE_STUDENT_COUNT
Test.yFA <-  Enroll[which(Enroll$TERM  =='2018FA'),]$ACTIVE_STUDENT_COUNT
Test.yWI <-  Enroll[which(Enroll$TERM  =='2019WI'),]$ACTIVE_STUDENT_COUNT
Test.ySP <-  Enroll[which(Enroll$TERM  =='2019SP'),]$ACTIVE_STUDENT_COUNT

    

f <- as.formula(ACTIVE_STUDENT_COUNT~.)
fit.rf <- randomForest(f,data = Train.x ,nodesize = 10, importance= T, ntree=1300 )

rf.predictionsSU <- round(predict(fit.rf , Test.xSU))
rf.predictionsFA <- round(predict(fit.rf , Test.xFA))
rf.predictionsWI <- round(predict(fit.rf , Test.xWI))
rf.predictionsSP <- round(predict(fit.rf , Test.xSP))


msesRFSU <- sum((rf.predictionsSU - Test.ySU)^2)/length(Test.ySU)
msesRFFA <- sum((rf.predictionsFA - Test.yFA)^2)/length(Test.yFA)
msesRFWI <- sum((rf.predictionsWI - Test.yWI)^2)/length(Test.yWI)
msesRFSP <- sum((rf.predictionsSP - Test.ySP)^2)/length(Test.ySP)

Sect <- read.csv('SECTT1.CSV',header = T)
DT <- read.csv('Sec_DT.csv', header = T)
Enroll <- inner_join(Sect,DT, by= c('SEC_NAME','TERM'))
Enroll <- Enroll %>% mutate(Quarter = as.factor(substr(TERM,5,6))) 
Enroll <- Enroll %>% mutate(Dept = as.factor(substr(CRS_NAME,1,3)))


Test.xSU  <- Enroll[which(Enroll$TERM  =='2018SU'),]
Test.xFA  <- Enroll[which(Enroll$TERM  =='2018FA'),]
Test.xWI  <- Enroll[which(Enroll$TERM  =='2019WI'),]
Test.xSP  <- Enroll[which(Enroll$TERM  =='2019SP'),]


check.RF.SU <- data.frame(
                    Course = Test.xSU$CRS_NAME,
                    Prediction = rf.predictionsSU,
                    Actual = Test.ySU,
                    Sec.name = Test.xSU$SEC_NAME,
                    PY= Test.xSU$Count_PY)
check.RF.SU  <- check.RF.SU  %>% mutate(Residual = abs(Actual-Prediction) ,
                                          Residual2 =Actual-Prediction)

check.RF.FA <- data.frame(
                Course = Test.xFA$CRS_NAME,
                Prediction = rf.predictionsFA,
                Actual = Test.yFA,
                Sec.name = Test.xFA$SEC_NAME,
                PY= Test.xFA$Count_PY)
check.RF.FA  <- check.RF.FA  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)


check.RF.WI <- data.frame(
                     Course = Test.xWI$CRS_NAME,
                     Prediction = rf.predictionsWI,
                     Actual = Test.yWI,
                     Sec.name = Test.xWI$SEC_NAME,
                     PY= Test.xWI$Count_PY)
check.RF.WI  <- check.RF.WI  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)


check.RF.SP <- data.frame(
                Course = Test.xSP$CRS_NAME,
                Prediction = rf.predictionsSP,
                Actual = Test.ySP,
                Sec.name = Test.xSP$SEC_NAME,
                PY= Test.xSP$Count_PY)
check.RF.SP  <- check.RF.SP  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)






fit.plot.RFSU <- ggplot(data = check.RF.SU , aes(x=Prediction,y=Actual))+
                    geom_point(aes(color= Residual))+ 
                    geom_text(data = check.RF.SU%>% filter(Residual > quantile(Residual,0.95)),
                    aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
                    geom_abline(intercept = 0) +xlim(0,40)+ ylim(0,40)+
                    theme_base()+
                    labs(title=paste(c('2018SU' ,': Random Forest Actual Vs Predicted'),collapse=''),
                    subtitle = paste('MSE =',as.character(round(msesRFSU,2)),collapse = ' '),
                    caption = paste('Caption: Courses Label on Residuals', paste(c('>',round(quantile(check.RF.SU$Residual,0.95),2),'(Top 5%)'),
                                                                         collapse = ' '),collapse = ' '))

print(fit.plot.RFSU)


fit.plot.RFFA <- ggplot(data = check.RF.FA , aes(x=Prediction,y=Actual))+
                geom_point(aes(color= Residual))+ 
                geom_text(data = check.RF.FA%>% filter(Residual > quantile(Residual,0.95)),
                aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
                geom_abline(intercept = 0) +
                xlim(0,40)+ ylim(0,40)+
                theme_base()+
                labs(title=paste(c('2018FA' ,': Random Forest Actual Vs Predicted'),collapse=''),
                subtitle = paste('MSE =',as.character(round(msesRFFA,2)),collapse = ' '),
                caption = paste('Caption: Courses Label on Residuals', paste(c('>',round(quantile(check.RF.FA$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.RFFA)



fit.plot.RFWI <- ggplot(data = check.RF.WI , aes(x=Prediction,y=Actual))+
                        geom_point(aes(color= Residual))+ 
                        geom_text(data = check.RF.WI%>% filter(Residual > quantile(Residual,0.95)),
                        aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
                        geom_abline(intercept = 0) + xlim(0,40)+ ylim(0,40)+
                        theme_base()+
                        labs(title=paste(c('2019WI' ,': Random Forest Actual Vs Predicted'),collapse=''),
                        subtitle = paste('MSE =',as.character(round(msesRFWI,2)),collapse = ' '),
                        caption = paste('Caption: Courses Label on Residuals', paste(c('>',round(quantile(check.RF.WI$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.RFWI)


fit.plot.RFSP <- ggplot(data = check.RF.SP , aes(x=Prediction,y=Actual))+
                 geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
                 geom_text(data = check.RF.SP%>% filter(Residual > quantile(Residual,0.95)),
                 aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
                 geom_abline(intercept = 0) +
                 theme_base()+
                 labs(title=paste(c('2019SP' ,': Random Forest Actual Vs Predicted'),collapse=''),
                 subtitle = paste('MSE =',as.character(round(msesRFSP,2)),collapse = ' '),
                 caption = paste('Caption: Courses Label on Residuals', paste(c('>',round(quantile(check.RF.SP$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.RFSP)


















#### SEction BOOST
library(xgboost)
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

# smat <- matrix(0,nrow = nrow(DT) , ncol=7*26)
# counter = 1
# for (i in 3:9){
#     for (j in 10:35){
#         smat[,counter]<-as.vector(DT[,i]*DT[,j])
#         vals <- append(vals,paste(c(colnames(DT[,i,drop=F]),colnames(DT[,j,drop=F])),collapse = '_'))
#         counter = counter +1
#     }
# }
# 
# DT2 <- as.data.frame(smat)
# colnames(DT2) <- vals
# DT <- cbind(DT,DT2)


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
#Enroll <- Enroll %>% filter(Quarter != 'SU')

Train.x <- Enroll[which(!Enroll$TERM %in% c('2018SU','2018FA','2019WI','2019SP')),]
Test.xSU  <- Enroll[which(Enroll$TERM  =='2018SU'),]
Test.xFA  <- Enroll[which(Enroll$TERM  =='2018FA'),]
Test.xWI  <- Enroll[which(Enroll$TERM  =='2019WI'),]
Test.xSP  <- Enroll[which(Enroll$TERM  =='2019SP'),]

Train.x <- Train.x %>% select_if(is.numeric)
Test.xSU <- Test.xSU %>% select_if(is.numeric)
Test.xFA <- Test.xFA %>% select_if(is.numeric)
Test.xWI <- Test.xWI %>% select_if(is.numeric)
Test.xSP <- Test.xSP %>% select_if(is.numeric)

Train.y <- Train.x$ACTIVE_STUDENT_COUNT
Test.ySU <- Test.xSU$ACTIVE_STUDENT_COUNT
Test.yFA <- Test.xFA$ACTIVE_STUDENT_COUNT
Test.yWI <- Test.xWI$ACTIVE_STUDENT_COUNT
Test.ySP <- Test.xSP$ACTIVE_STUDENT_COUNT

colnames(Train.x) <- str_remove(colnames(Train.x), '-')
colnames(Test.xSU) <- str_remove(colnames(Test.xSU), '-')
colnames(Test.xFA) <- str_remove(colnames(Test.xFA), '-')
colnames(Test.xWI) <- str_remove(colnames(Test.xWI), '-')
colnames(Test.xSP) <- str_remove(colnames(Test.xSP), '-')

f <- as.formula(ACTIVE_STUDENT_COUNT~.)

Train.x <- model.matrix(f, Train.x)[,-1]
Test.xSU <- model.matrix(f, Test.xSU)[,-1]
Test.xFA <- model.matrix(f, Test.xFA)[,-1]
Test.xWI <- model.matrix(f, Test.xWI)[,-1]
Test.xSP <- model.matrix(f, Test.xSP)[,-1]




xgb_train = xgb.DMatrix(data = Train.x, label = Train.y)
xgb_testSU = xgb.DMatrix(data = Test.xSU, label = Test.ySU)
xgb_testFA = xgb.DMatrix(data = Test.xFA, label = Test.yFA)
xgb_testWI = xgb.DMatrix(data = Test.xWI, label = Test.yWI)
xgb_testSP = xgb.DMatrix(data = Test.xSP, label = Test.ySP)


params <- list(booster = "gbtree", eta=0.4, gamma=0, alpha=4,
               max_depth=8, min_child_weight=1, subsample=1, colsample_bytree=1)

fit.boost <- xgboost(params = params, data=Train.x,label = Train.y,nfold = 10, nrounds = 145)

boost.predictionsSU <- round(predict(fit.boost , Test.xSU))
boost.predictionsFA <- round(predict(fit.boost , Test.xFA))
boost.predictionsWI <- round(predict(fit.boost , Test.xWI))
boost.predictionsSP <- round(predict(fit.boost , Test.xSP))

msesxgSU <- sum((boost.predictionsSU -Test.ySU)^2)/length(Test.ySU)
msesxgFA <- sum((boost.predictionsFA -Test.yFA)^2)/length(Test.yFA)
msesxgWI <- sum((boost.predictionsWI -Test.yWI)^2)/length(Test.yWI)
msesxgSP <- sum((boost.predictionsSP -Test.ySP)^2)/length(Test.ySP)


#glm.net.predictions <- round(glm.net.predictions*(mx[1]-mi[1])+mi[1])
Sect <- read.csv('SECTT1.CSV',header = T)
DT <- read.csv('Sec_DT.csv', header = T)
Enroll <- inner_join(Sect,DT, by= c('SEC_NAME','TERM'))
Enroll <- Enroll %>% mutate(Quarter = as.factor(substr(TERM,5,6))) 
Enroll <- Enroll %>% mutate(Dept = as.factor(substr(CRS_NAME,1,3)))


Test.xSU  <- Enroll[which(Enroll$TERM  =='2018SU'),]
Test.xFA  <- Enroll[which(Enroll$TERM  =='2018FA'),]
Test.xWI  <- Enroll[which(Enroll$TERM  =='2019WI'),]
Test.xSP  <- Enroll[which(Enroll$TERM  =='2019SP'),]


check.xg.SU <- data.frame(
    Course = Test.xSU$CRS_NAME,
    Prediction = boost.predictionsSU,
    Actual = Test.ySU,
    Sec.name = Test.xSU$SEC_NAME,
    PY= Test.xSU$Count_PY)
check.xg.SU  <- check.xg.SU  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)

check.xg.FA<- data.frame(
    Course = Test.xFA$CRS_NAME,
    Prediction = boost.predictionsFA,
    Actual = Test.yFA,
    Sec.name = Test.xFA$SEC_NAME,
    PY= Test.xFA$Count_PY)
check.xg.FA  <- check.xg.FA  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)

check.xg.WI<- data.frame(
    Course = Test.xWI$CRS_NAME,
    Prediction = boost.predictionsWI,
    Actual = Test.yWI,
    Sec.name = Test.xWI$SEC_NAME,
    PY= Test.xWI$Count_PY)
check.xg.WI  <- check.xg.WI  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)

check.xg.SP<- data.frame(
    Course = Test.xSP$CRS_NAME,
    Prediction = boost.predictionsSP,
    Actual = Test.ySP,
    Sec.name = Test.xSP$SEC_NAME,
    PY= Test.xSP$Count_PY)
check.xg.SP  <- check.xg.SP  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)


fit.plot.xgSU <- ggplot(data = check.xg.SU , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ 
    geom_text(data = check.xg.SU %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) + xlim(0,40)+ ylim(0,40)+
    theme_base()+
    labs(title=paste(c('2018SU' ,':XGBoost Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(msesxgSU,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.xg.SU$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.xgSU)



fit.plot.xgFA <- ggplot(data = check.xg.FA , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ 
    geom_text(data = check.xg.SU %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) + xlim(0,40)+ ylim(0,40)+
    theme_base()+
    labs(title=paste(c('2018FA' ,': XGBoost Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(msesxgFA,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.xg.FA$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.xgFA)



fit.plot.xgWI <- ggplot(data = check.xg.WI , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.xg.WI %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2019WI' ,': XGBoost Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(msesxgWI,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.xg.WI$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.xgWI)


fit.plot.xgSP <- ggplot(data = check.xg.SP , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.xg.SP %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2019SP' ,': XGBoost Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(msesxgSP,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.xg.SP$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.xgSP)


#### Section NN
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
colnames(Enroll) <- str_remove(colnames(Enroll),'-')


max.nums <- apply(Enroll[,c(3,4,5,6,7,8,9,10,11,169,171)],2,max)
min.nums <- apply(Enroll[,c(3,4,5,6,7,8,9,10,11,169,171)],2 , min)

max.nums[c(1,2,3,4,5,6,9)] <- max.nums[2]
min.nums[c(1,2,3,4,5,6,9)] <- min.nums[2]


Enroll[,names(max.nums)] <- scale(Enroll[,names(max.nums)],center = min.nums, scale = max.nums-min.nums)

Vars = colnames(Enroll[,-c(1,170)])
Enroll <- Enroll[,Vars]
Enroll$AvgSecEnr3yr <-NULL


Train.x <- Enroll[which(!Enroll$TERM %in% c('2018SU','2018FA','2019WI','2019SP')),]
Test.xSU  <- Enroll[which(Enroll$TERM  =='2018SU'),]
Test.xFA  <- Enroll[which(Enroll$TERM  =='2018FA'),]
Test.xWI  <- Enroll[which(Enroll$TERM  =='2019WI'),]
Test.xSP  <- Enroll[which(Enroll$TERM  =='2019SP'),]

Train.x <- Train.x %>% select_if(is.numeric)
Test.xSU <- Test.xSU %>% select_if(is.numeric)
Test.xFA <- Test.xFA %>% select_if(is.numeric)
Test.xWI <- Test.xWI %>% select_if(is.numeric)
Test.xSP <- Test.xSP %>% select_if(is.numeric)

Train.y <- Train.x$ACTIVE_STUDENT_COUNT
Test.ySU <- Test.xSU$ACTIVE_STUDENT_COUNT
Test.yFA <- Test.xFA$ACTIVE_STUDENT_COUNT
Test.yWI <- Test.xWI$ACTIVE_STUDENT_COUNT
Test.ySP <- Test.xSP$ACTIVE_STUDENT_COUNT


Train.x <- Train.x %>% select_if(is.numeric)
Test.xSU <- Test.xSU %>% select_if(is.numeric)
Test.xFA <- Test.xFA %>% select_if(is.numeric)
Test.xWI <- Test.xWI %>% select_if(is.numeric)
Test.xSP <- Test.xSP %>% select_if(is.numeric)




f <- as.formula(ACTIVE_STUDENT_COUNT~.)


sect.nn <- neuralnet(f,linear.output = T,data = Train.x ,act.fct = 'logistic')


pSU =compute(sect.nn ,Test.xSU)
pFA =compute(sect.nn ,Test.xFA)
pWI =compute(sect.nn ,Test.xWI)
pSP =compute(sect.nn ,Test.xSP)



Sect <- read.csv('SECTT1.CSV',header = T)
DT <- read.csv('Sec_DT.csv', header = T)
Enroll <- inner_join(Sect,DT, by= c('SEC_NAME','TERM'))
Enroll <- Enroll %>% mutate(Quarter = as.factor(substr(TERM,5,6))) 
Enroll <- Enroll %>% mutate(Dept = as.factor(substr(CRS_NAME,1,3)))


Test.xSU  <- Enroll[which(Enroll$TERM  =='2018SU'),]
Test.xFA  <- Enroll[which(Enroll$TERM  =='2018FA'),]
Test.xWI  <- Enroll[which(Enroll$TERM  =='2019WI'),]
Test.xSP  <- Enroll[which(Enroll$TERM  =='2019SP'),]




predictionsSU = round(ifelse(pSU$net.result*(max.nums[2]-min.nums[2])+min.nums[2] >0 ,
                             pSU$net.result*(max.nums[2]-min.nums[2])+min.nums[2],
                             0))

predictionsFA = round(ifelse(pFA$net.result*(max.nums[2]-min.nums[2])+min.nums[2] >0 ,
                             pFA$net.result*(max.nums[2]-min.nums[2])+min.nums[2],
                             0))

predictionsWI = round(ifelse(pWI$net.result*(max.nums[2]-min.nums[2])+min.nums[2] >0 ,
                             pWI$net.result*(max.nums[2]-min.nums[2])+min.nums[2],
                             0))

predictionsSP = round(ifelse(pSP$net.result*(max.nums[2]-min.nums[2])+min.nums[2] >0 ,
                             pSP$net.result*(max.nums[2]-min.nums[2])+min.nums[2],
                             0))


predictionsSU = ifelse(predictionsSU > Test.xSU$SEC_CAPACITY, Test.xSU$SEC_CAPACITY,
                       predictionsSU)

predictionsFA = ifelse(predictionsFA > Test.xFA$SEC_CAPACITY, Test.xFA$SEC_CAPACITY,
                       predictionsFA)

predictionsWI = ifelse(predictionsWI > Test.xWI$SEC_CAPACITY, Test.xWI$SEC_CAPACITY,
                       predictionsWI)

predictionsSP = ifelse(predictionsSP > Test.xSP$SEC_CAPACITY, Test.xSP$SEC_CAPACITY,
                       predictionsSP)


msesSU <- sum((predictionsSU -Test.xSU$ACTIVE_STUDENT_COUNT)^2)/length(predictionsSU)
msesFA <- sum((predictionsFA -Test.xFA$ACTIVE_STUDENT_COUNT)^2)/length(predictionsFA)
msesWI <- sum((predictionsWI -Test.xWI$ACTIVE_STUDENT_COUNT)^2)/length(predictionsWI)
msesSP <- sum((predictionsSP -Test.xSP$ACTIVE_STUDENT_COUNT)^2)/length(predictionsSP)



check.nn.SU <- data.frame(
    Course = Test.xSU$CRS_NAME,
    Prediction = predictionsSU,
    Actual = Test.xSU$ACTIVE_STUDENT_COUNT,
    Sec.name = Test.xSU$SEC_NAME,
    PY= Test.xSU$Count_PY)
check.nn.SU  <- check.nn.SU  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)

check.nn.FA <- data.frame(
    Course = Test.xFA$CRS_NAME,
    Prediction = predictionsFA,
    Actual = Test.xFA$ACTIVE_STUDENT_COUNT,
    Sec.name = Test.xFA$SEC_NAME,
    PY= Test.xFA$Count_PY)
check.nn.FA  <- check.nn.FA  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)


check.nn.WI <- data.frame(
    Course = Test.xWI$CRS_NAME,
    Prediction = predictionsWI,
    Actual = Test.xWI$ACTIVE_STUDENT_COUNT,
    Sec.name = Test.xWI$SEC_NAME,
    PY= Test.xWI$Count_PY)
check.nn.WI  <- check.nn.WI  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)


check.nn.SP <- data.frame(
    Course = Test.xSP$CRS_NAME,
    Prediction = predictionsSP,
    Actual = Test.xSP$ACTIVE_STUDENT_COUNT,
    Sec.name = Test.xSP$SEC_NAME,
    PY= Test.xSP$Count_PY)
check.nn.SP  <- check.nn.SP  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)


fit.plot.nnSU <- ggplot(data = check.nn.SU , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.nn.SU %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2018SU' ,':NN Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(msesSU,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.nn.SU$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))
print(fit.plot.nnSU)



fit.plot.nnFA <- ggplot(data = check.nn.FA , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.nn.SU %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2018FA' ,':NN Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(msesFA,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.nn.FA$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.nnFA)






fit.plot.nnWI <- ggplot(data = check.nn.WI , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.nn.WI %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2019WI' ,':NN Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(msesWI,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.nn.WI$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.nnWI)


fit.plot.nnSP <- ggplot(data = check.nn.SP , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.nn.SP %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2019SP' ,':NN Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(msesSP,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.nn.SP$Residual,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.nnSP)




##### Plot Model Analysis

library(ggpubr)

ggarrange(fit.plot.nnSU, fit.plot.xgSU,fit.plot.RFSU, fit.plot.lrSU + rremove("x.text"), 
          ncol = 2, nrow = 2)

ggarrange(fit.plot.nnFA, fit.plot.xgFA,fit.plot.RFFA, fit.plot.lrFA + rremove("x.text"), 
          ncol = 2, nrow = 2)

ggarrange(fit.plot.nnWI, fit.plot.xgWI,fit.plot.RFWI, fit.plot.lrWI + rremove("x.text"), 
          ncol = 2, nrow = 2)

ggarrange(fit.plot.nnSP, fit.plot.xgSP,fit.plot.RFSP, fit.plot.lrSP + rremove("x.text"), 
          ncol = 2, nrow = 2)

Lass.check <- rbind(check.las.FA,check.las.SU,check.las.SP,check.las.WI)
rf.check <- rbind(check.RF.FA,check.RF.SU,check.RF.SP,check.RF.WI)
xg.check <- rbind(check.xg.FA,check.xg.SU,check.xg.SP,check.xg.WI)
nn.check <- rbind(check.nn.FA,check.nn.SU,check.nn.SP,check.nn.WI)



las.hist <- ggplot(data = Lass.check, aes(Residual2, fill=5)) +
            geom_histogram(binwidth =1,show.legend = FALSE) +
            labs(title='Distribution of Lasso Residuals 2018-2019',
                 subtitle = 'IQR = (-3,3)' )+
            xlab('Residual')+
            theme_base()


quantile(Lass.check$Residual2, c(0.25,0.75))
quantile(rf.check$Residual2, c(0.25,0.75))
quantile(xg.check$Residual2, c(0.25,0.75))
quantile(nn.check$Residual2, c(0.25,0.75))

rf.hist <-  ggplot(data = rf.check, aes(Residual2, fill=5)) +
            geom_histogram(binwidth =1,show.legend = FALSE) +
            labs(title='Distribution of Forest Residuals 2018-2019',
            subtitle = 'IQR = (-3,4)' )+
           
            xlab('Residual')+
            theme_base()

xg.hist <-  ggplot(data = xg.check, aes(Residual2, fill=5)) +
            geom_histogram(binwidth =1,show.legend = FALSE) +
            labs(title='Distribution of XG Boost Residuals 2018-2019',
            subtitle = 'IQR = (-4,3)' )+
            xlab('Residual')+
            theme_base()


nn.hist <- ggplot(data = nn.check, aes(Residual2, fill=5)) +
            geom_histogram(binwidth =1,show.legend = FALSE) +
            labs(title='Distribution of NN Residuals 2018-2019',
            subtitle = 'IQR = (-4,4)' )+
            xlab('Residual')+
            theme_base()


ggarrange( rf.hist,las.hist ,xg.hist,nn.hist+ rremove("x.text"), 
          ncol = 2, nrow = 2)




check.meanSU  <- data.frame(
    Course = Test.xSU$CRS_NAME,
    Prediction = round(rowMeans(cbind(check.las.SU$Prediction,check.RF.SU$Prediction,
                                      check.xg.SU$Prediction))),
    Actual = Test.xSU$ACTIVE_STUDENT_COUNT,
    Sec.name = Test.xSU$SEC_NAME,
    PY= Test.xSU$Count_PY)
check.meanSU  <- check.meanSU  %>% mutate(Residual = abs(Actual-Prediction) ,
                                          Residual2 =Actual-Prediction)

MSE.AVGSU <-  sum((check.meanSU$Prediction -check.meanSU$Actual)^2)/length(check.meanSU$Prediction)



check.meanFA <- data.frame(
                    Course = Test.xFA$CRS_NAME,
                    Prediction = round(rowMeans(cbind(check.las.FA$Prediction,check.RF.FA$Prediction,
                                                      check.xg.FA$Prediction))),
                    Actual = Test.xFA$ACTIVE_STUDENT_COUNT,
                    Sec.name = Test.xFA$SEC_NAME,
                    PY= Test.xFA$Count_PY)
check.meanFA  <- check.meanFA  %>% mutate(Residual = abs(Actual-Prediction) ,
                                        Residual2 =Actual-Prediction)

MSE.AVGFA <-  sum((check.meanFA$Prediction -check.meanFA$Actual)^2)/length(check.meanFA$Prediction)



check.meanWI <- data.frame(
    Course = Test.xWI$CRS_NAME,
    Prediction = round(rowMeans(cbind(check.las.WI$Prediction,check.RF.WI$Prediction,
                                      check.xg.WI$Prediction))),
    Actual = Test.xWI$ACTIVE_STUDENT_COUNT,
    Sec.name = Test.xWI$SEC_NAME,
    PY= Test.xWI$Count_PY)
check.meanWI  <- check.meanWI  %>% mutate(Residual = abs(Actual-Prediction) ,
                                          Residual2 =Actual-Prediction)

MSE.AVGWI <-  sum((check.meanWI$Prediction -check.meanWI$Actual)^2)/length(check.meanWI$Prediction)





check.meanSP <- data.frame(
    Course = Test.xSP$CRS_NAME,
    Prediction = round(rowMeans(cbind(check.las.SP$Prediction,check.RF.SP$Prediction,
                                      check.xg.SP$Prediction))),
    Actual = Test.xSP$ACTIVE_STUDENT_COUNT,
    Sec.name = Test.xSP$SEC_NAME,
    PY= Test.xSP$Count_PY)
check.meanSP  <- check.meanSP  %>% mutate(Residual = abs(Actual-Prediction) ,
                                          Residual2 =Actual-Prediction)

MSE.AVGSP <-  sum((check.meanSP$Prediction -check.meanSP$Actual)^2)/length(check.meanSP$Prediction)







fit.plot.meanSU <- ggplot(data = check.meanSU , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.meanSU %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2018SU' ,':Ensemble Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(MSE.AVGSU,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.meanSU$Residual2,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.meanSU)




fit.plot.meanFA <- ggplot(data = check.meanFA , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.meanFA %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2018FA' ,':Ensemble Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(MSE.AVGFA,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.meanFA$Residual2,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.meanFA)


fit.plot.meanWI <- ggplot(data = check.meanWI , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.meanWI %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2019WI' ,':Ensemble Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(MSE.AVGWI,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.meanWI$Residual2,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.meanWI)



fit.plot.meanSP <- ggplot(data = check.meanSP , aes(x=Prediction,y=Actual))+
    geom_point(aes(color= Residual))+ xlim(0,40)+ ylim(0,40)+
    geom_text(data = check.meanSP %>% filter(Residual > quantile(Residual,0.95)),
              aes(label=Course ), check_overlap = T, nudge_x = T, nudge_y = T) +
    geom_abline(intercept = 0) +
    theme_base()+
    labs(title=paste(c('2019SP' ,':Ensemble Actual Vs Predicted'),collapse=''),
         subtitle = paste('MSE =',as.character(round(MSE.AVGSP,2)),collapse = ' '),
         caption = paste('Caption: Courses Label on Residuals', paste(c('>',
                                                                        round(quantile(check.meanSP$Residual2,0.95),2),'(Top 5%)'),
                                                                      collapse = ' '),collapse = ' '))

print(fit.plot.meanSP)


ggarrange(fit.plot.meanSU, fit.plot.meanFA,fit.plot.meanWI, fit.plot.meanSP + rremove("x.text"), 
          ncol = 2, nrow = 2)



mean.check <- rbind(check.meanFA,check.meanSU,check.meanSP,check.meanWI)

quantile(mean.check$Residual2 , c(0.25,0.75))

mean.hist<- ggplot(data = mean.check, aes(Residual2, fill=5)) +
            geom_histogram(binwidth =1,show.legend = FALSE) +
            labs(title='Distribution of Ensemble Residuals 2018-2019',
                 subtitle = 'IQR = (-3,3)' )+
                xlab('Residual')+
                theme_base()



#### Create Data for dashboard



dp.Store <- character(nrow(Enroll))
for(i in 1:nrow(Enroll)){
    if(all(Enroll[i,47:174] == 0)){
        dp.Store[i] = 'OL'
    }else{  
        for(j in 47:174){
            if(Enroll[i,j] == 1){
                dp.Store[i] = colnames(Enroll[i,j,drop=F])
                break
            }
            
        }
    }
}

Enroll$DayPat = dp.Store

View(Enroll[which(Enroll$MONWEDFRI ==1 ),c('SEC_NAME','DayPat')])

QuarterTerms <- c(rep('2018FA',nrow(check.las.FA)),rep('2018SU',nrow(check.las.SU)),
                  rep('2019SP',nrow(check.las.SP)),rep('2019WI',nrow(check.las.WI)))


Lass.check <- Lass.check %>% rename(Predictionlass =Prediction,
                                    Residuallass = Residual,
                                    Residual2lass=Residual2)


rf.check <- rf.check %>% rename(Predictionrf =Prediction,
                                    Residualrf = Residual,
                                    Residual2rf=Residual2)

xg.check <- xg.check %>% rename(Predictionxg =Prediction,
                                Residualxg = Residual,
                                Residual2xg=Residual2)

mean.check <- mean.check %>%  rename(Predictionmean =Prediction,
                                     Residualmean = Residual,
                                     Residual2mean=Residual2)

Lass.check$Terms <- QuarterTerms
rf.check$Terms <- QuarterTerms
xg.check$Terms <- QuarterTerms
mean.check$Terms <- QuarterTerms

df1 <- inner_join(Lass.check,rf.check ,by =c('Sec.name','Terms','Actual','PY','Course'))

df2 <- inner_join(df1,xg.check, by =c('Sec.name','Terms','Actual','PY','Course'))
df3 <- inner_join(df2,mean.check, by =c('Sec.name','Terms','Actual','PY','Course'))


df3 <- df3 %>% rename(SEC_NAME= Sec.name, TERM=Terms)
Final.df <- inner_join(df3,Enroll, by =c('SEC_NAME' , 'TERM'))


Final.df.w <- Final.df[,c(1:17,192:193)]

write.csv(Final.df.w, file = 'ModelDash.csv',row.names = F)


mat<- coef(fit.lasso)

mat[which(rownames(mat)=="Count_PY"),]

