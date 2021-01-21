#### PCA EDA
library(tidyr)
library(dplyr)
library(matrixStats)
library(ggpubr)
library(ggthemes)

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

C4P <- read.csv('C4P.csv', header = T)
Enroll <- inner_join(Sect,DT, by= c('SEC_NAME','TERM'))
C4P <- C4P %>% dplyr::rename('Acad_Year' = 'TERM_REPORTING_YEAR')


Enroll <- left_join(Enroll,C4P,by = c('COURSES_ID' ,'CRS_NAME', "Acad_Year" ))
Enroll$C4AP <- replace_na(Enroll$C4AP,0)
Enroll <- Enroll %>% mutate(Quarter = as.factor(substr(TERM,5,6)))
Enroll <- Enroll %>% mutate(Dept = as.factor(substr(CRS_NAME,1,3)))

# pca.predictors <- Enroll[,10:17]
# 
#  stem <- c('MAT', 'BIO', 'CHM', 'CIS','GEL' , 'PHY','PHS','GEG')
#  BSN <- c('BSN' , 'ECO','REL')
#  ART <- c('ART','MUS')
#  ACT <- c('WLD' , 'PEF', 'PEH')
#  HUM <- c('ENG','HIS','PSY','REL','THE','POL')
# 
# 
#  PCA.checkBSN <- Enroll %>% filter( TERM == '2019FA') %>% filter(Dept %in% BSN )
#  PCA.checkART <- Enroll %>% filter( TERM == '2019WI') %>% filter(Dept %in% ART )
#  PCA.checkSTEM <- Enroll %>% filter( TERM == '2018FA') %>% filter(Dept %in% stem )
#  PCA.checkHUM <- Enroll %>% filter( TERM == '2019WI')%>% filter(Dept %in% HUM)
# 
#  pca.model <- prcomp(PCA.checkBSN[,c("SEC_CAPACITY" ,"SS_PY" ,"Count_PY","WD_PY","DROP_PY" ,
#                                   "CC_PY","SEC_Count","AvgSecEnr3yr" ,"AvgSecEnrto","C4AP")],
#                      scale. = T,center = T)
# 
#  source("ggbiplot.R")
# 
#   Bs.plot <- ggbiplot(pca.model, labels = PCA.checkBSN$Dept) + ggtitle('Biplot Business  2018FA')
# 
#  pca.model <- prcomp(PCA.checkART[,c("SEC_CAPACITY" ,"SS_PY" ,"Count_PY","WD_PY","DROP_PY" ,
#                                     "SEC_Count","AvgSecEnr3yr" ,"AvgSecEnrto","C4AP")],
#                      scale. = T,center = T)
# 
#  Art.plot <- ggbiplot(pca.model, labels = PCA.checkART$Dept) + ggtitle('Biplot ART 2019WI')
# 
# 
#  pca.model <- prcomp(PCA.checkSTEM[,c("SEC_CAPACITY" ,"SS_PY" ,"Count_PY","WD_PY","DROP_PY" ,
#                                     "CC_PY","SEC_Count","AvgSecEnr3yr" ,"AvgSecEnrto","C4AP")],
#                      scale. = T,center = T)
# 
#  Stem.plot <- ggbiplot(pca.model, labels = PCA.checkSTEM$Dept) + ggtitle('Biplot STEM 2018FA')
# 
#  pca.model <- prcomp(PCA.checkHUM[,c("SEC_CAPACITY" ,"SS_PY" ,"Count_PY","WD_PY","DROP_PY" ,
#                                     "CC_PY","SEC_Count","AvgSecEnr3yr" ,"AvgSecEnrto","C4AP")],
#                      scale. = T,center = T)
# 
# 
#  HUM.plot <-  ggbiplot(pca.model, labels = PCA.checkHUM$Dept) + ggtitle('Biplot Humanities 2019FA')
# 
#  ggarrange(Stem.plot, Art.plot, ncol = 1, nrow = 2)
# 
#  print(Stem.plot)
#  print(Art.plot)
# 
# 


####COrrelations
library(ggcorrplot)
library(corrplot)

num.data <- Enroll[,c("ACTIVE_STUDENT_COUNT","SEC_CAPACITY" ,"SS_PY" ,"Count_PY","WD_PY",
                      "DROP_PY","CC_PY","SEC_Count","AvgSecEnr3yr" ,"AvgSecEnrto","C4AP")]

colnames(num.data) <- c('Enrollments', 'Capacity','SSPY','EnrollPY',
                        'WDPY','DROPPY','CCPY','SecCOUNT','3yrAvg','TotAvg','C4AP')
M = cor(num.data)

corrplot(M,method= "color", addCoef.col = T , number.cex = .55)

res1 <- cor.mtest(num.data, conf.level = .95)
corrplot(M,method = 'color', p.mat = res1$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white")

####box plots

#Enroll$Acad_Year <- as.character(Enroll$Acad_Year)

qbplot <- ggplot(data = Enroll %>% filter(Acad_Year < 2019) , aes(x=Quarter , y=ACTIVE_STUDENT_COUNT )) + 
                   geom_boxplot(aes(fill=Quarter)) +
                   facet_grid(~Acad_Year)+ 
                   ylab('Enrollments Per Section')+
                    ggtitle('Section Enrollments By Term Over Year') +
              
                   theme_base()
  
                    
print(qbplot)

Enroll$CTE <- as.factor(ifelse(Enroll$CTE ==1, 'CTE','Not CTE'))
Enroll$OnlineED <- as.factor(ifelse(Enroll$OnlineED ==1, 'Online','F2F'))

Online.pl <- ggplot(data= Enroll %>% filter(Acad_Year %in% c(2016,2017,2018)), aes(x=ACTIVE_STUDENT_COUNT, fill=OnlineED )) + 
            geom_density(alpha =0.4, position='identity')+
            facet_grid(~Acad_Year)+ 
            scale_fill_manual(values = c('orchid','green4'))+
            xlab('Enrollments Per Section') +
            ggtitle('Section Enrollments by Modality Over Year')+
            theme_base()


print(Online.pl)


Ol.regression <- ggplot(data= Enroll %>% filter(Acad_Year %in% c(2016,2017,2018)), aes( x =AvgSecEnr3yr,y=ACTIVE_STUDENT_COUNT,color=OnlineED  )) + 
                        geom_point() +
                        geom_smooth(method='lm', formula= y~x)+
                        scale_color_manual(values =c('orchid','green4'))+
                        facet_grid(~Acad_Year)+ 
                        theme_base()

print(Ol.regression)


library(dplyr)
Enroll.Depts.avg <- Enroll %>% group_by(Dept) %>% summarize(Enrolls = mean(ACTIVE_STUDENT_COUNT)) %>% arrange(desc(Enrolls))
Enroll.Depts.Tot <- Enroll %>% group_by(Dept) %>% summarize(Enrolls = sum(ACTIVE_STUDENT_COUNT)) %>% arrange(desc(Enrolls))

Enroll.Depts.avg.q <- as.data.frame(Enroll %>% group_by(Dept,Quarter) %>% summarize(Enrolls = mean(ACTIVE_STUDENT_COUNT)) %>% arrange(desc(Enrolls)))
Enroll.Depts.Tot.q <- Enroll %>% group_by(Dept,Quarter) %>% summarize(Enrolls = sum(ACTIVE_STUDENT_COUNT)) %>% arrange(desc(Enrolls))

dept.plot <- ggplot(Enroll.Depts.avg.q %>% filter(Dept %in% c('PEA','MAT','SPA','ENG','CRJ','ART','BIO')) 
                    ,aes(Dept, y = Enrolls , fill=Dept)) + geom_bar( stat = 'identity')+ ylim(c(0,33))+
                      geom_text(aes(label=round(Enrolls)), position=position_dodge(width=0.9), vjust=-0.25)+
                    facet_wrap(~Quarter)   + theme_base() + ggtitle('Average Enrollment By Deparment and Quarter')
print(dept.plot)



Day.pattern <- Enroll %>% filter(MONWED == 1 |MONWEDFRI == 1| TUETHU == 1|MON==1|TUE==1|WED==1|THU==1|FRI==1)

Day.Vec <- character(nrow(Day.pattern))
for(i in 1:nrow(Day.pattern)){
  if(Day.pattern[i,'MONWED'] == 1){Day.Vec[i] <-'MW'}
  else if(Day.pattern[i,'MON'] == 1){Day.Vec[i] <-'MON'}
  else if(Day.pattern[i,'TUE'] == 1){Day.Vec[i] <-'TUE'}
  else if(Day.pattern[i,'WED'] == 1){Day.Vec[i] <-'WED'}
  else if(Day.pattern[i,'THU'] == 1){Day.Vec[i] <-'THU'}
  else if(Day.pattern[i,'FRI'] == 1){Day.Vec[i] <-'FRI'}
  else if(Day.pattern[i,'MONWEDFRI'] == 1){Day.Vec[i] <-'MWF'}

  else {Day.Vec[i] <-'TTH'}
  
  
}

Day.pattern$Pattern <- as.factor(Day.Vec)

Day.pattern <- arrange(Day.pattern,CRS_NAME)





daypate101 <- ggplot(data= Day.pattern %>% filter(Acad_Year %in% c(2016,2017,2018) , 
                    Quarter %in% c('FA','WI','SP')),
                    aes(x=ACTIVE_STUDENT_COUNT, fill=Pattern)) + 
                    geom_density(alpha =0.4, position='identity')+
                    facet_grid(~Acad_Year)+ 
                    #scale_fill_manual(values = c('orchid','green4','blue'))+
                    xlab('Enrollments Per Section') +
                    ggtitle('Section Enrollments by Day Pattern Over Year')+
                    theme_base()


print(daypate101)



qbplot <- ggplot(data= Day.pattern %>% filter(Quarter %in% c('FA','WI','SP')),aes(x=Pattern,y=ACTIVE_STUDENT_COUNT)) + 
             geom_boxplot(aes(fill=Pattern)) +
            facet_grid(~Quarter)+ 
            ylab('Enrollments Per Section')+
            ggtitle('Section Enrollments By DayPattern Over Quarter') +
            theme_base()


print(qbplot)

Enroll$Acad_Year <- s(Enroll$Acad_Year)

EnrollA <- as.data.frame(Enroll %>% group_by(CRS_NAME,Acad_Year,Quarter) %>% 
                           summarise(Enrolls = sum(ACTIVE_STUDENT_COUNT))) %>% filter(Acad_Year !='2019')

EnrollA$Acad_Year <-  as.factor(substr(as.character(EnrollA$Acad_Year),3,4))


p<-ggplot(EnrollA %>% filter(CRS_NAME %in% c('ENG-103','SPA-101','PSY-101','MAT-201','ART-119A','BIO-110'),Quarter %in% c('FA','WI','SP')), 
          aes(x=Acad_Year, y=Enrolls, group=CRS_NAME)) +
          geom_line(aes(color=CRS_NAME))+
          geom_point(aes(color=CRS_NAME))+
          xlab('Academic Year')+
          ylab('Enrollments')+
          facet_grid(~Quarter)+
          ggtitle('Time Trend Enrollments By Course') +
          theme_base()

p


crscount <- Enroll %>% group_by(CRS_NAME)%>% tally() %>% arrange(desc(n))

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

lr.df <- Enroll[,-c(3,4,5,6,7,8,47:174)]
lr.df$DayPat <- as.factor(lr.df$DayPat)
##### Run forward selection

start = Sys.time()
mods <- subset.select(lr.df,Response = 'ACTIVE_STUDENT_COUNT',method = 'Fwd', measure = 'BIC')

tot.time = Sys.time() - start






