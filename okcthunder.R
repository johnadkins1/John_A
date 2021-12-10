okc.data <-read.csv(file = "Shots_data$.csv", header = T, sep = ",")
okc.data

##Further analysis below

##NC3: number > 7.8 and sqrt formula output > 23.75
johna.newcol <- sqrt(okc.data$x^2 + okc.data$y^2)
NC3.data <- rep(0, nrow(okc.data))

NC3.data <- ifelse((okc.data$y > 7.8 & johna.newcol  > 23.75),1,0)




##C3: number <= 7.8 & abs(x) > 22.
john.a.newcol <- abs(okc.data$x)
C3.data <- rep(0, nrow(okc.data))

C3.data <- ifelse((okc.data$y<=7.8 & john.a.newcol > 22),1,0)



##2 pointers: neither of the previous ones

Non.three.data <- rep(0, nrow(okc.data))
Non.three.data <- ifelse((NC3.data + C3.data <1), "2PT", 0)


## 3 pointers made: fgmade = 1 and Non.three.data = "-")

Three.point.made <- rep(0, nrow(okc.data))
Three.point.made <- ifelse((okc.data$fgmade == 1 & Non.three.data == 0), "3PM", 0)


##3PM per team
Three.point.made[1:280]
Three.point.made[281:560]

##Shot distribution__________Deliverable #1____________________________________________

##team A two-pointers
sum(Non.three.data[1:280] == "2PT", na.rm=TRUE)
(170/280)*100

##team B two-pointers
sum(Non.three.data[281:560] == "2PT", na.rm=TRUE)
(163/280)*100


##team A non-corner threes
sum(NC3.data[1:280] == 1, na.rm=TRUE)
(90/280)*100

#team B non-corner threes
sum(NC3.data[281:560] == 1, na.rm=TRUE)
(96/280)*100


##team A corner threes
sum(C3.data[1:280] == 1, na.rm=TRUE)
(20/280)*100

##team B corner threes
sum(C3.data[281:560] == 1, na.rm=TRUE)
(21/280)*100


##eFG % _______________________Deliverable #2_________________________________________________



##eFG% for Team A within the 2-point zone.
sum(Non.three.data[1:280] == "2PT", na.rm=TRUE)
sum(okc.data$fgmade[1:280] == 1 & Non.three.data[1:280] == "2PT", na.rm = TRUE)

(68 + (0.5*68))/170
0.6*100

##eFG% for Team B within the 2-point zone.
sum(Non.three.data[281:560] == "2PT", na.rm=TRUE)
sum(okc.data$fgmade[281:560] == 1 & Non.three.data[281:560] == "2PT", na.rm = TRUE)

(75 + (0.5 * 75))/163
0.690184*100


## eFG% for team A within the corner 3 zone.

sum(C3.data[1:280] == 1 & okc.data$fgmade[1:280] == 1, na.rm = TRUE)
sum(C3.data[1:280] == 1, na.rm=TRUE)

(8 + (0.5*8))/20
0.6*100

## eFG% for team B within the corner 3 zone.
sum(C3.data[281:560] == 1 & okc.data$fgmade[281:560] == 1, na.rm = TRUE)
sum(C3.data[281:560] == 1, na.rm=TRUE)

(5 + (0.5*5))/21
0.3571429*100



## eFG% for team A within the non-corner 3 zone.
sum(NC3.data[1:280] == 1, na.rm=TRUE)
sum(NC3.data[1:280] == 1 & okc.data$fgmade[1:280] == 1, na.rm = TRUE)

(29 + (0.5*29))/90
0.4833333*100


## eFG% for team B within the non-corner 3 zone.
sum(NC3.data[281:560] == 1, na.rm=TRUE)
sum(NC3.data[281:560] == 1 & okc.data$fgmade[281:560] == 1, na.rm = TRUE)

(35 + (0.5*35))/96
0.546875*100


## Thank you for the opportunity.






