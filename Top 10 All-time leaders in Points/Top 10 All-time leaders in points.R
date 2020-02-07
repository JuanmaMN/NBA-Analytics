
# Upload the packages -----------------------------------------------------

library(dplyr)
library(plotly)
library(tidyr)
library(scales)
library(readr)
library(readxl)
library(fmsb)  



# Data --------------------------------------------------------------------

All_Time_Leaders_Points_Regular_Season <- read_excel("NBA_stats.xlsx", 
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

View(All_Time_Leaders_Points_Regular_Season)



# Prepare the data --------------------------------------------------------

## add and index column

All_Time_Leaders_Points_Regular_Season_2 <-All_Time_Leaders_Points_Regular_Season %>% mutate(id = row_number())

## Pass the first column to the number

All_Time_Leaders_Points_Regular_Season_2 <- All_Time_Leaders_Points_Regular_Season[, -(1)]
rownames(All_Time_Leaders_Points_Regular_Season_2) <- All_Time_Leaders_Points_Regular_Season$PLAYER

View(All_Time_Leaders_Points_Regular_Season_2)
colnames(All_Time_Leaders_Points_Regular_Season_2)

NBA_3<-All_Time_Leaders_Points_Regular_Season_2%>% select(3,6,15,14)%>%mutate(names=row.names(All_Time_Leaders_Points_Regular_Season_2))%>%top_n(10,PTS)

colnames(NBA_3)[colnames(NBA_3)=="PTS"] <- "Points"

colnames(NBA_3)[colnames(NBA_3)=="AST"] <- "Assists"

colnames(NBA_3)[colnames(NBA_3)=="REB"] <- "Rebounds"

colnames(NBA_3)[colnames(NBA_3)=="FG%"] <- "Field_Goal_Percentage"


data_points<-NBA_3%>%select(1,5) %>% spread(names,Points)
data_Assists<-NBA_3%>%select(3,5) %>% spread(names,Assists)
data_Rebounds<-NBA_3%>%select(4,5) %>% spread(names,Rebounds)
data_Field_Goal_Percentage<-NBA_3%>%select(2,5) %>% spread(names,Field_Goal_Percentage)



# Add extra data for Radar ------------------------------------------------


data_points <- rbind(rep(45000,10) , rep(15000,10) , data_points)   
data_Assists <- rbind(rep(10000,10) , rep(1000,10) , data_Assists)   
data_Rebounds <- rbind(rep(25000,10) , rep(5000,10) , data_Rebounds)    
data_Field_Goal_Percentage <- rbind(rep(70,10) , rep(30,10) , data_Field_Goal_Percentage)

# Graph -------------------------------------------------------------------




par(mar=c(5.1, 4.1, 4.1, 2.1))

par(mfrow = c(2, 2)) # Create a 2 x 2 plotting matrix





radar<-radarchart( data_points, axistype=1 ,
                   #custom polygon
                   pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(15000,55000,10000), cglwd=0.8,
                   #custom labels
                   vlcex=0.8,
                   title=paste("Total points"),
                   cex.main = 1.5
) 




radar2<-radarchart(data_Assists, axistype=1,
                   #custom polygon
                   pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4 ,
                   #custom the grid
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1000,10000,2000), cglwd=0.8,
                   #custom labels
                   vlcex=0.8,
                   title=paste("Total assists"),
                   cex.main = 1.5
) 


radar3<-radarchart( data_Rebounds, axistype=1,
                    #custom polygon
                    pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
                    #custom the grid
                    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(10000,30000,5000), cglwd=0.8,
                    #custom labels
                    vlcex=0.8,
                    title=paste("Total rebounds"),
                    cex.main = 1.5
) 


radar4<-radarchart(data_Field_Goal_Percentage,axistype=1,
                    #custom polygon
                    pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
                    #custom the grid
                    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(30,70,10), cglwd=0.8,
                    #custom labels
                    vlcex=0.8,
                    title=paste("Field Goal Percentage"),
                    cex.main = 1.5
) 






