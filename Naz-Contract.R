### Load necessary packages ###

suppressMessages(library(ggplot2))
suppressMessages(library(scales))

### Load in Data ###

NBAData <- read.csv("~/Desktop/Projects/Naz-Contract/NBAData.csv")

head(NBAData)
str(NBAData)

# Filter data #

numeric_data <- NBAData[, c("Age", "G", "MP", "FG", "FGA", "FG.", "X3P", 
                            "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "eFG.", 
                            "FT", "FTA", "FT.", "ORB", "DRB", "TRB", 
                            "AST", "STL", "BLK", "TOV", "PF", "PTS")]

numeric_data_clean <- na.omit(numeric_data)

### K Means Clustering ###

scaled_data <- scale(numeric_data_clean)

wss <- numeric(15)
for (k in 1:15) {
  kmeans_model <- kmeans(scaled_data, centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
}

plot(1:15, wss, type = "b", pch = 19, xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares")

kmeans_model <- kmeans(scaled_data, centers = 4, nstart = 25)

numeric_data_clean$Cluster <- kmeans_model$cluster

NBAData$Cluster <- NA  
NBAData[rownames(numeric_data_clean), "Cluster"] <- numeric_data_clean$Cluster

# Find Naz Reid's Data #

naz_reid_data <- NBAData[NBAData$Player == "Naz Reid", ]
naz_reid_data_scaled <- naz_reid_data[, c("Age", "G", "MP", "FG", "FGA", "FG.", "X3P", 
                                          "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "eFG.", 
                                          "FT", "FTA", "FT.", "ORB", "DRB", "TRB", 
                                          "AST", "STL", "BLK", "TOV", "PF", "PTS")]


naz_reid_scaled <- scale(naz_reid_data_scaled, center = attr(scaled_data, "scaled:center"), 
                         scale = attr(scaled_data, "scaled:scale"))


distances <- apply(kmeans_model$centers, 1, function(center) sum((naz_reid_scaled - center)^2))

naz_reid_cluster <- which.min(distances)

naz_reid_cluster

# Find similar players to Naz #

similar_players <- NBAData[NBAData$Cluster == naz_reid_cluster, ]


similar_players <- NBAData[NBAData$Cluster == naz_reid_cluster, ]

similar_players_data <- similar_players[, c("Age", "G", "MP", "FG", "FGA", "FG.", "X3P", 
                                            "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "eFG.", 
                                            "FT", "FTA", "FT.", "ORB", "DRB", "TRB", 
                                            "AST", "STL", "BLK", "TOV", "PF", "PTS")]

similar_players_scaled <- scale(similar_players_data, center = attr(scaled_data, "scaled:center"), 
                                scale = attr(scaled_data, "scaled:scale"))

distances_to_naz <- apply(similar_players_scaled, 1, function(player) sum((player - naz_reid_scaled)^2))

closest_30_players <- similar_players[order(distances_to_naz), ][1:32, ]

print(closest_30_players)


### Load in Contract Data from HoopsHype ###

ContractData <- read.csv("~/Desktop/Projects/Naz-Contract/ContractData.csv")
print(ContractData)

# Find Mean and Standard Deviation #

mean_contract <- mean(ContractData$AVG)
print(mean_contract)
sd_contract <- sd(ContractData$AVG)
print(sd_contract)
n <- nrow(ContractData)
print(n)

### Create 95% Confidence Interval ###

alpha <- 0.05
t_score <- qt(1 - alpha/2, df = n - 1)
print(t_score)

# Calculate standard error #

se_contract <- sd_contract / sqrt(n)
print(se_contract)

# Calculate confidence interval #

lower_bound <- mean_contract - t_score * se_contract
upper_bound <- mean_contract + t_score * se_contract

cat("95% Confidence Interval: (", lower_bound, ",", upper_bound, ")\n")


### Creating Visualizations ###

mean_years <- mean(ContractData$YearsRemaing) # need to create variable for histogram

# Years Remaining Histogram #

ggplot(ContractData, aes(x = YearsRemaing)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_years), color = "red", linetype = "dashed", size = 1) +  # Add vertical line for mean
  labs(title = "Distribution of Years Remaining for Each Player",
       x = "Years Remaining",
       y = "Frequency") +
  theme_minimal() +
  annotate("text", x = mean_years + 0.5, y = max(table(ContractData$YearsRemaing)) * 0.9, 
           label = paste("Average:", round(mean_years, 2)), color = "black", size = 4)  # Add text label for average


# Average Contract Value Histogram #

ggplot(ContractData, aes(x = AVG)) +
  geom_histogram(binwidth = 5000000, fill = "green", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_contract), color = "red", linetype = "dashed", size = 1) +  # Add vertical line for mean
  labs(title = "Histogram of Contract Values", 
       x = "Contract Value ($)", 
       y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma) +
  annotate("text", x = mean_contract * 1.5, y = max(table(ContractData$AVG)) * 4, 
           label = paste("Average:", round(mean_contract, 2)), color = "black", size = 4)
