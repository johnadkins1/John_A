##This project is to determine how many drafted goalies from 2007 thru 2019
##  have delivered at least one league-average season in the NHL (sv%).
## Let's list the league averages below.
##I began data collection on March 18, 2023.
#League average numbers credit to hockey-reference.
library(tidyverse)

svdata <- read.csv("stats.csv", header = T)


svdata[1,11]



svpdata <- svdata[11]


##now that we've extracted the sv% numbers from the read-in, we will need the 
# last 17 records to match the seasons 2007-2023. We are cutting off the drafts
# at 2019, but this is for data availability.

# 22-23:   0.905
# 21-22:   0.907
# 20-21:   0.908
# 19-20:   0.910
# 18-19:   0.910
# 17-18:   0.912
# 16-17:   0.913
# 15-16:   0.915
# 14-15:   0.915
# 13-14:  0.914
# 12-13:  0.912
# 11-12:  0.914
# 10-11:  0.913
# 09-10:  0.911
# 08-09:  0.908
# 07-08:  0.909
# 06-07:  0.905


##Using a pre-prepared Excel sheet, I will craft a data-frame for each year 
# 2007-2019 that lists eligible goalies for this project. 
# 'lg.avg.yN' just indicates whether or not the player(s) listed recorded at least one league average [or better] season at all in their career.
## Statistics to complete the project will be pulled from hockey-reference.com




#2007 Draft_____________________________________________________________________
playername07 <- ("S. Darling")
# totalgoalies07 <- (20)
lg.avg.yN <- ("Yes")
percent07 <- (1/20)*100
oseven <- data.frame(playername07,lg.avg.yN)


#2008 Draft_____________________________________________________________________

playername08 <- c("B. Holtby", "J. Markstrom", "J. Allen", "M. Hutchinson",
                  "A. Lindback", "D. Tokarski", "K. Poulin")
# totalgoalies08 <- (23)
lg.avg.yN08 <- c("Yes", "Yes", "Yes", "Yes", "Yes", "No", "No")
percent08 <- (5/23)*100
oeight <- data.frame(playername08, lg.avg.yN08)


#2009 Draft_____________________________________________________________________

playername09 <- c("D. Kuemper", "R. Lehner", "M. Koskinen", "A. Nilsson")
# totalgoalies09 <- (21)
lg.avg.yN09 <- c("Yes", "Yes", "Yes", "Yes")
percent09 <- (4/21)*100
onine <- data.frame(playername09, lg.avg.yN09)


#2010 Draft_____________________________________________________________________

playername10 <- c("F. Andersen", "P. Grubauer", "P. Mrazek", "J. Campbell", 
                  "L. Domingue", "S. Wedgewood")
# totalgoalies10 <- (21)
lg.avg.yN10 <- c("Yes", "Yes", "Yes", "Yes", "No", "Yes")
percent10 <- (5/21)*100
oten <- data.frame(playername10, lg.avg.yN10)

#2011 Draft_____________________________________________________________________

playername11 <- c("J. Gibson", "J. Binnington", "L. Brossoit", "A. Forsberg")
# totalgoalies11 <- (19)
lg.avg.yN11 <- c("Yes", "Yes", "Yes", "Yes")
percent11 <- (4/19)*100
oeleven <- data.frame(playername11, lg.avg.yN11)

#2012 Draft_____________________________________________________________________

playername12 <- c("A. Vasilevskiy", "C. Hellebuyck", "M. Murray", "L. Ullmark",
                  "J. Korpisalo", "M. Subban")
# totalgoalies12 <- (24)
lg.avg.yN12 <- c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
percent12 <- (6/24)*100
otwelve <- data.frame(playername12, lg.avg.yN12)


#2013 Draft_____________________________________________________________________

playername13 <- c("T. Jarry", "J. Saros", "C. Petersen")
# totalgoalies13 <- (21)
lg.avg.yN13 <- c("Yes", "Yes", "Yes")
percent13 <- (3/21)*100
othirteen <- data.frame(playername13, lg.avg.yN13)



#2014 Draft_____________________________________________________________________

playername14 <- c("T. Demko", "I. Shesterkin", "I. Sorokin", "V. Husso", 
                  "V. Vanecek", "E. Merzlikins", "K. Kahkonen", "A. Nedeljkovic")
# totalgoalies14 <- (21)
lg.avg.yN14 <- c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
percent14 <- (8/21)*100
ofourteen <- data.frame(playername14, lg.avg.yN14)


#2015 Draft_____________________________________________________________________

playername15 <- c("I. Samsonov", "M. Blackwood", "A. Hill", "S. Montembeault",
                  "K. Vejmelka")
# totalgoalies15 <- (24)
lg.avg.yN15 <- c("Yes", "Yes", "Yes", "No", "No")
percent15 <- (3/24)*100
ofifteen <- data.frame(playername15, lg.avg.yN15)


#2016 Draft_____________________________________________________________________

playername16 <- c("C. Hart", "F. Gustavsson")
# totalgoalies16 <- (18)
lg.avg.yN16 <- c("Yes", "Yes")
percent16 <- (2/18)*100
osixteen <- data.frame(playername16, lg.avg.yN16)

#2017 Draft_____________________________________________________________________

playername17 <- c("J. Oettinger", "S. Skinner", "J. Swayman")
# totalgoalies17 <- (21)
lg.avg.yN17 <- c("Yes", "Yes", "Yes")
percent17 <- (3/21)*100
oseventeen <- data.frame(playername17, lg.avg.yN17)


#2018 Draft_____________________________________________________________________

playername18 <- c("A. Schmid")
# totalgoalies18 <- (29)
lg.avg.yN18 <- c("Yes") ## I'm betting on Schmid to emerge above league avg. He's on NJD
percent18 <- (1/29)*100
oeighteen <- data.frame(playername18, lg.avg.yN18)


#2019 Draft_____________________________________________________________________

playername19 <- c("S. Knight", "P. Kotchetkov")
# totalgoalies19 <- (22)
lg.avg.yN19 <- c("Yes", "Yes") ## I'm betting on Kotchetkov to just emerge above lg.avg.
percent19 <- (2/22)*100
onineteen <- data.frame(playername19, lg.avg.yN19)



columnx <- c(percent07, percent08, percent09, percent10, percent11,
percent12, percent13, percent14, percent15, percent16,
percent17, percent18, percent19)

columnY <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013",
             "2014", "2015", "2016", "2017", "2018", "2019")

playerfinal <- data.frame(columnx, columnY)

##Now, let's plot it!!


install.packages("RColorBrewer")
library(RColorBrewer)


barplot(height = playerfinal$columnx, main = "Percent of NHL Goalies drafted (by year) to play >= 1 league avg save% season", names=playerfinal$columnY, col="#69b3a2", horiz=T, las=1)

#Fin











