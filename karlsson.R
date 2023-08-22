##Let's explore which of Karlsson's teammate-specific data points best predict corsi-for, independent
#   of which teammate it is. Data from Natural Stat Trick.

hockeydata <- read.csv('karlsson_team.csv')
hockeydata[1] <- NULL


pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}




str(hockeydata)
summary(hockeydata)
ncol(hockeydata)
colnames(hockeydata)
nrow(hockeydata)
table(hockeydata$CF..With)
prop.table(table(hockeydata$CF..With))


set.seed(5623)
# Splitting the dataset into the Training set and Test set
library(caTools)
split <- sample.split(hockeydata$CF..With, SplitRatio = 0.8:0.2)
training_set <- subset(hockeydata, split ==TRUE) 
testing_set <- subset(hockeydata, split ==FALSE) 


# Feature Scaling.
training_set[-53] <- scale(training_set[-53])
testing_set[-53] <- scale(testing_set[-53])
str(training_set)




install.packages("h2o")
library(h2o)

h2o.init(nthreads = -1)



##      Model 1: 
#       Build with hidden = 5 & 5 nodes; epochs = 50, nfolds = 3, act = ReLu
#       Plot the variable importance

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

model1 <- h2o.deeplearning(y = 'CF..With',
                           training_frame = as.h2o(training_set),
                           activation = "Rectifier",
                           hidden = c(5,5),
                           epochs = 50,
                           nfolds = 3,
                           train_samples_per_iteration = -2,
                           variable_importances = TRUE)



summary(model1)
h2o.varimp_plot(model1)
h2o.performance(model1)



h2o.shutdown()




##According to the plot from line 67, the following three stats were the most 
# important in predicting Corsi-For% when playing with Karlsson (line 83). For context,
# Corey Masisak wrote that "Corsi is the best approximation for puck possession, 
# which analytic studies have shown is a great predictor of future success" (Masisak, 2015).

# All are "'on-ice' metrics, meaning while a player is on the ice:
# Shots-For | Fenwick-For | Goals-For.

# The overall purpose of this project is to mine any kind of insights from Karlsson's 101-point 
# season in 2022-23 in order to predict which defense partner is optimal for his time on PIT. My 
# hypothesis is that Ryan Graves would be most optimal.

# While the analysis up to now was looking at which variables predict another, I will now look at
# the following: Karlsson played 680:22 in all situations in 22-23 with Jaycob Megna, the highest of
# any D. However, let us visualize how Megna and Graves stack up to each other.


defensedata <- read.csv("comparison.csv", header = T)


defensesubset <- subset(defensedata, select = c(Name, SF, FF, GF))
                        
                        
# Load necessary libraries
library(ggplot2)

# Create a bar chart showing on-ice Shots For (SF) for each player
 ggplot(defensesubset, aes(x = Name, y = SF, fill = Name)) +
  geom_bar(stat = "identity") +
  labs(title = "On-ice Shots For (SF) by Player", x = "Player", y = "Shots For") +
  theme_minimal()

 
 # Create a bar chart showing on-ice Goals-For (GF) for each player
 ggplot(defensesubset, aes(x = Name, y = SF, fill = Name)) +
   geom_bar(stat = "identity") +
   labs(title = "On-ice Goals-For (GF) by Player", x = "Player", y = "Shots For") +
   theme_minimal()
 
 # Create a bar chart showing on-ice Fenwick-For (FF) for each player
 ggplot(defensesubset, aes(x = Name, y = SF, fill = Name)) +
   geom_bar(stat = "identity") +
   labs(title = "On-ice Fenwick-For (FF) by Player", x = "Player", y = "Shots For") +
   theme_minimal()                      
 
 
 
#   By clicking through the provided plot code, you can visualize how Ryan Graves 
 #  outdid Jaycob Megna in each of these three on-ice stats. So, my conclusion is that 
 #  Ryan Graves would be fine as a defense partner for Karlsson in 2023-24.
 
 
 
 
 
 
 
 
 
 