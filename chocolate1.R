#Vindya Liyanage
#Data Visualization project on chocolate

#import Libraries
library(ggplot2)  
library(beeswarm)
library(quantmod)
library(reshape2)
library(plyr)
library(scales)
library(viridis)
library(zoo)
library(moments)
library(dplyr)
library(plyr)
library(plotly)
library(streamgraph)
library(sqldf)
library(corrplot)

#Import Data set
chocolate<- read.csv(file = "./chocolate/chocolate.csv",header=T)
taste <- read.csv(file = "./chocolate/chocolate_taste_dataset.csv",header=T)
head(taste)
print(taste)


head(chocolate)
print(chocolate)
summary(chocolate)

#-------------------------chocolates rated >3.5---------------------------------------
# --- #1FDACC --- teal
# -----#F17070 --orange
Ccomp<- chocolate%>% group_by(chocolate$company)
Ccomp1 <- chocolate%>% group_by(chocolate$company) %>% summarise(rating = mean(rating) , comp=company)
print(Ccomp1)
print(count(Ccomp1))

#remove repeated records
raterec <- sqldf('SELECT DISTINCT * FROM Ccomp1')
recomonded <- subset(raterec, rating > 3.7 , 
                  select=c(comp, rating))
print(recomonded)

x <- ggplot(recomonded, aes( x=comp , y=rating) ) + 
  geom_bar(position="dodge", stat="identity",fill="#6699cc") + xlab("Company Name") + ylab("Mean rating") + 
  ggtitle("Chocolate brands which has overrall rating over 3.7 ")  + theme(axis.text.x = element_text(angle = 90))

print(x)
 b <-x + coord_flip()
 print(b)
#---
p <- ggplot(recomonded, aes( x=comp , y=rating) ) + 
  geom_bar(position="dodge", stat="identity",fill="#6699cc") + xlab("Company Name") + ylab("Mean rating") + 
  ggtitle("Chocolate brands which has overrall rating over 3.5 ")  + theme(axis.text.x = element_text(angle = 90)) + 
  geom_bar(data=subset(recomonded, rating==max(rating)), aes(comp, rating),
         fill="#de6a5f", stat="identity")
print(p)

#---------------------------Chocolate companies has overall rating < 2.0-----------------------------------------------
Dissapoint <- subset(raterec, rating < 2.9 , 
                     select=c(comp, rating))
Dissapoint<- within(Dissapoint, rating 
                   <- factor(rating, levels=names(sort(table(rating),
                                 decreasing=TRUE))))
print(Dissapoint)
 
#---------------------rating vs coco percentage----------------


chocolate2 <- within(chocolate, {
  cocoa_butter <- factor(cocoa_butter, labels = c("Contain cocoa butter", "No cocoa butter"))
  sugar <- factor(sugar, labels = c("Contain sugar", "No sugar"))
  counts_of_ingredients <- factor(counts_of_ingredients)
})

p1 <- ggplot(chocolate2) +
  geom_point(aes(x = cocoa_percent, y = rating, colour = counts_of_ingredients)) +
  labs(title = "Rating vs Cocoa Percentage",
       subtitle = "(compared with chocolate containing cocoa butter and sugar)",
       caption = "This shows how rating changes from cocoa percentage and number of ingredients of chocolate bars.They are grouped by containment of sugar and cocoa butter ",
       x = "Cocoa percentage",
       y = "Rating",
       colour = "Number of ingredients")

p1 + theme_gray() # the default

p2 <- p1 + facet_grid(cocoa_butter   ~  sugar)
p2 + theme_bw()

#------------------ chocolates from switzerland and Belgium 2019 ---------
rec2019 <- subset(chocolate, review_date == 2019 , 
                     select=c(company, rating ,company_location,counts_of_ingredients , review_date ,first_taste,second_taste ))

bycountry <- subset(rec2019, company_location == "Switzerland" | company_location == "Belgium" , 
                    select=c(company, rating ,company_location,counts_of_ingredients , review_date , first_taste ))
print(bycountry)
company <- factor(bycountry$company)
ggplot(bycountry, aes( x=company, y=rating , fill=company_location ) ) + 
  geom_bar(position="dodge", stat="identity") + xlab("Company Name") + ylab("Rating") +
  ggtitle("Rating chocolates from swizterland and Belgium in 2019  ")  + theme(axis.text.x = element_text(angle = 90))



#-------------------- first_taste
France <- subset(rec2019, company_location == "France"  , 
              select=c(company, rating ,company_location,counts_of_ingredients , review_date , first_taste , second_taste))
print(France)
ggplot(France, aes( x=company, y=rating , fill=first_taste) ) + 
  geom_bar(position = position_dodge(width = 1), stat="identity")+ scale_color_brewer(palette="Dark2") + xlab("Company Name") + ylab("Rating") +
  ggtitle("Ratings of chocolate companies from france in 2019 according to first taste  ")  + theme(axis.text.x = element_text(angle = 90) ) 
  
  
  #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9" , "#1FDACC" ,"#F17070 ", "#6475AA","#999999", "#E69F00", "#56B4E9" , "#1FDACC" ,"#F17070 ", "#6475AA"))

#----- second taste
France <- subset(rec2019, company_location == "France"  , 
                 select=c(company, rating ,company_location,counts_of_ingredients , review_date , first_taste , second_taste))
print(France)
ggplot(France, aes( x=company, y=rating , fill=second_taste) ) + 
  geom_bar(position = position_dodge(width = 1), stat="identity")+ scale_color_brewer(palette="Dark1") + xlab("Company Name") + ylab("Rating") +
  ggtitle("Ratings of chocolate companies from france in 2019 according to Second taste  ")  + theme(axis.text.x = element_text(angle = 90) ) 



#---------- 
boxplot(rating~cocoa_percent,data=chocolate, main="rating by cocoa percentage", 
        xlab="Cocoa Percentage", ylab="rating")

res2 <- rcorr(as.matrix(Ccomp))

#------------------ chocolates from switzerland and Belgium 2019 ---------
rec2019 <- subset(chocolate, 
                  select=c(company, rating ,company_location,counts_of_ingredients , review_date ,first_taste,second_taste ))

bycountry <- subset(rec2019, company_location == "Chile" | company_location == "Argentina" | company_location == "Czech republic", 
                    select=c(company, rating ,company_location,counts_of_ingredients , review_date , first_taste ))
print(bycountry)
company <- factor(bycountry$company)
ggplot(bycountry, aes( x=company, y=rating , fill=company_location ) ) + 
  geom_bar(position="dodge", stat="identity") + xlab("Company Name") + ylab("Rating") +
  ggtitle("Rating chocolates from Chile and Argentina and Czech Republic ")  + theme(axis.text.x = element_text(angle = 90))



