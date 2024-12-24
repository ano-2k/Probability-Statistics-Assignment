
install.packages("readr")

library(readr)

mydata <- read_csv("D:\\1.UOB\\PS ASSIGNNMENT\\SpaceX Survey .csv")
total_responses <- 100

#Age 

age <- mydata$`What is your age?`
age_freq <- table(age)
age_df <- as.data.frame(age_freq)

percentage <- round((age_df$Freq / total_responses) * 100, 1)

barplot(
  age_df$Freq,
  xlab = "Age Categories",
  ylab = "Number of Participants",
  main = "Age Distribution",
  names.arg = age_df$age,
  col = "orange",    
  border = "black",      
  ylim = c(0, max(age_df$Freq) + 10)  
)
abline(h=0)

text(
  x = barplot(age_df$Freq, plot = FALSE),
  y = age_df$Freq + 0.5,
  labels = paste(percentage, "%"),
  pos = 3,
  cex = 0.8,
  col = "black"         
)


# Gender 
gender <- mydata$`What is your gender?`

gender_freq <- table(gender)
gender_df <- as.data.frame(gender_freq)

percentage <- round((gender_df$Freq / sum(gender_df$Freq)) * 100, 1)

pie(gender_df$Freq,
    labels = paste(gender_df$gender, "(", percentage, "%)", sep = ""),
    col = c("lightblue", "lightgreen"),
    main = "Gender Distribution")


# Familiarity with Space X's Reusable Rocket Technology 

spacex_familiarity <- mydata$`How familiar are you with SpaceX’s reusable rocket technology?`
spacex_familiarity_freq <- table(spacex_familiarity)
spacex_familiarity_df <- as.data.frame(spacex_familiarity_freq)
percentage <- round((spacex_familiarity_df$Freq / sum(spacex_familiarity_df$Freq)) * 100, 1)
pie(spacex_familiarity_df$Freq,
    labels = paste(spacex_familiarity_df$spacex_familiarity, "(", percentage, "%)", sep = ""),
    col = c("lightgreen", "lightcoral","lightblue"),
    main = "Familiarity with SpaceX's Reusable Rocket Technology")



# Which SpaceX missions have you heard of?
missions <- mydata$`Which SpaceX missions have you heard of? (Select all that apply)`
missions_split <- unlist(strsplit(missions, ";"))
mission_freq <- table(missions_split)
mission_df <- as.data.frame(mission_freq)
mission_df$missions_split <- trimws(mission_df$missions_split)
total_responses <- sum(mission_df$Freq)
percentage_mission <- round((mission_df$Freq / total_responses) * 100, 1)
percentage_mission_lbl <- paste(percentage_mission, "%", sep = "")

par(mar = c(5, 6, 4, 2) + 0.1)
bar_positions <- barplot(mission_df$Freq, horiz = FALSE, las = 1, 
                         main = "SpaceX Mission Awareness", 
                         xlab = "Missions", ylab = "Frequency", 
                         cex.main = 0.8, cex.axis = 0.8, 
                         names.arg = mission_df$missions_split, 
                         col = "lightblue", border = "black")

abline(h=0)
text(bar_positions, mission_df$Freq + 1, labels = percentage_mission_lbl, pos = 3, cex = 0.8, col = "black")



# Public Interest in Space Exploration Programs

education_counts <- table(mydata$`Would you like to learn more about space exploration through educational programs?`)
percentages <- round(education_counts / sum(education_counts) * 100)
colors <- c("#4CAF50", "lightblue")
pie(education_counts,
    labels = paste(names(education_counts), percentages, "%"),
    main = "Public Interest in Space Exploration Educational Programs",
    col = colors,
    border = "white",      
    cex = 1.2,             
    radius = 0.8            
)


# RStudio Code for Mean, Variance, Standard Deviation

midpoints <- c(8.5, 21, 29.5, 44.5, 77)
frequencies <- c(7, 48, 29, 13, 3)

mean_age <- sum(midpoints * frequencies) / sum(frequencies)
variance_age <- sum((midpoints - mean_age)^2) / (sum(frequencies)-1)
sd_age <- sqrt(variance_age)
mean_age
variance_age
sd_age



# Gender Distribution in TreeMap
install.packages("treemap")
library(treemap)
gender <- mydata$`What is your gender?`
gender_df <- as.data.frame(table(gender))
colors <- c("#FFC0CB", "#ADD8E6")  
treemap(gender_df,
        index = "gender",
        vSize = "Freq",
        vColor = "Freq",
        draw = TRUE,
        title = "Gender Distribution Based on Survey Responses",
        palette = colors,
        border.col = "black")


# Familiarity with SpaceX's Reusable Rocket Technology

responses <- c(1, 2, 3) 
percentages <- c(14, 56, 30)  
counts <- percentages * 100 / 100
mean_value <- sum(responses * counts) / sum(counts)

all_responses <- rep(responses, times = counts)
median_value <- median(all_responses)
variance_value <- sum((responses - mean_value)^2 * counts) / sum(counts)
std_dev_value <- sqrt(variance_value)
mean_value
median_value
variance_value
std_dev_value




# Scatter plot of Age vs. Safety Perception
age_numeric <- as.numeric(as.factor(mydata$`What is your age?`))
safety_numeric <- as.numeric(as.factor(mydata$`How do you perceive the safety of SpaceX missions compared to traditional government space programs like?`))
plot(
  jitter(age_numeric),
  jitter(safety_numeric),
  xlab = "Age Group",
  ylab = "Perception of Safety",
  main = "Scatter Plot of Age vs. Perception of SpaceX's Mission Safety",
  col = "red",
  pch = 16,
  xaxt = "n",
  yaxt = "n"
)
unique_safety_labels <- levels(as.factor(mydata$`How do you perceive the safety of SpaceX missions compared to traditional government space programs like?`))
axis(2,
     at = 1:length(unique_safety_labels),
     labels = unique_safety_labels
)

unique_age_labels <- levels(as.factor(mydata$`What is your age?`))
axis(1,
     at = 1:length(unique_age_labels),
     labels = unique_age_labels
)



 #Perception of SpaceX Safety Compared to Government Programs by Age Groups

mydata$AgeGroup <- factor(mydata$`What is your age?`,                          levels = c("Under 18", "18 - 24", "25 - 34", "35 - 54", "55 or older"))
mydata$SafetyPerception <- factor(mydata$`How do you perceive the safety of SpaceX missions compared to traditional government space programs like?`)
summary_table <- as.data.frame(table(mydata$AgeGroup, mydata$SafetyPerception))
print(summary_table)
colors <- c("lightgreen", "yellow", "skyblue")  
barplot_counts <- barplot(
  matrix(summary_table$Freq, nrow = length(levels(mydata$SafetyPerception)), byrow = TRUE),
  beside = TRUE,
  col = colors,
  names.arg = levels(mydata$AgeGroup),  
  legend.text = levels(mydata$SafetyPerception),  
  args.legend = list(title = "Safety Perception", x = "topright"),
  xlab = "Age Group",
  ylab = "Count of Responses",
  main = "Perception of SpaceX Safety Compared to Government Programs by Age Groups"
)
text(
  x = barplot_counts,
  y = matrix(summary_table$Freq, nrow = length(levels(mydata$SafetyPerception)), byrow = TRUE),
  label = matrix(summary_table$Freq, nrow = length(levels(mydata$SafetyPerception)), byrow = TRUE),
  pos = 3,   # Position above each bar
  cex = 0.8, # Text size
  col = "black"
)
abline(h=0)



# Z test 
n <- 100             
x <- 96              
p_hat <- x / n       
p0 <- 0.90          
alpha <- 0.05        

std_dev <- sqrt(p0 * (1 - p0) / n)

z <- (p_hat - p0) / std_dev

z_critical <- qnorm(1 - alpha)

if (z > z_critical) {
  decision <- "Reject the null hypothesis"
} else {
  decision <- "Fail to reject the null hypothesis"
}
# Display the results
cat("Sample Proportion (p̂):", p_hat, "\n")
cat("Hypothesized Proportion (p0):", p0, "\n")
cat("Standard Deviation:", std_dev, "\n")
cat("Test Statistic (Z):", z, "\n")
cat("Critical Z-Value:", z_critical, "\n")
cat("Decision:", decision, "\n")



# Perform the chi-square test for Perception of SpaceX Safety Compared to Government Programs by Age Groups

contingency_table <- table(mydata$`What is your age?`,mydata$`How do you perceive the safety of SpaceX missions compared to traditional government space programs like?`)

print(contingency_table)

chi_square_test <- chisq.test(contingency_table)

print(chi_square_test)













