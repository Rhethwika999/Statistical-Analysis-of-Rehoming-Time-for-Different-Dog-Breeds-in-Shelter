getwd()
setwd("C:/Users/HP/Documents/MSC Data Science/Statistics and Learning/CW1")
createsample(201751000)
save(mysample, file = "mysample.RData")
mysample

#checking for null values

any_null_values <- apply(mysample, 2, function(x) any(is.na(x)))
# Display columns with null values
columns_with_null <- names(mysample)[any_null_values]
cat("Columns with null values:", paste(columns_with_null, collapse = ', '))

#To get index of the rows with null values

rows_with_null <- which(apply(mysample, 1, function(x) any(is.na(x))))
cat("Rows with null values:", paste(rows_with_null, collapse = ', '))

write.csv(mysample,"rehome_temp.csv")


#Shape of the df

df_dimensions <- dim(mysample)

# Display the number of rows and columns
cat("Number of rows:", df_dimensions[1], "\n")
cat("Number of columns:", df_dimensions[2], "\n")

mysample_2<-mysample #copy of original df

#Removing index having null values
mysample <- mysample[-rows_with_null, ]

df_dimensions <- dim(mysample)
cat("Number of rows:", df_dimensions[1], "\n")
cat("Number of columns:", df_dimensions[2], "\n")

#Checking for 99999 in Rehomed

rows_with_value_99999 <- which(mysample == 99999, arr.ind = TRUE)[,1]

cat("Row indices with value 99999:", toString(rows_with_value_99999))

#Removing 
mysample <- mysample[-rows_with_value_99999, ]
df_dimensions <- dim(mysample)
cat("Number of rows:", df_dimensions[1], "\n")
cat("Number of columns:", df_dimensions[2], "\n")


install.packages("dplyr")
install.packages("e1071")

library(dplyr)
library(e1071)


#segregating
DF_ROT <- mysample[mysample$Breed == "Rottweiler", ]
DF_GH <- mysample[mysample$Breed == "Greyhound", ]
DF_DOB <- mysample[mysample$Breed == "Dobermann", ]

#grouping by breed
grouped_data <- mysample %>% 
  group_by(Breed)


breed_summaries <- grouped_data %>% 
  summarize(
    mean_health = mean(Health),
    sd_health = sd(Health),
    skewness_health = skewness(Health),
    
    Age_mode = mysample$Age[which.max(tabulate(match(mysample$Age, mysample$Age)))],
    
    mean_Visited = mean(Visited),
    sd_Visited = sd(Visited),
    skewness_Visited = skewness(Visited),
    mean_Rehomed= mean(Rehomed),
    sd_Rehomed = sd(Rehomed),
    skewness_Rehomed = skewness(Rehomed)
    
  )
print(breed_summaries)

# Convert breed_summaries into a new data frame
breed_summary_df <- data.frame(breed_summaries)
print(breed_summary_df)

#Count of fully grown and puppy
grouped_data <- mysample %>%
  group_by(Breed,Age,Rehomed_df_breeds)

count_result <- count(grouped_data)
print(count_result)
count_df <- as.data.frame(count_result)
print(count_df)

#Comparison based on age, breed, count of puppy and fullygrown , mean
result <- mysample %>%
  group_by(Breed, Age) %>%
  summarize(
    mean_Rehomeweeks = mean(Rehomed),
    sd_weeks = sd(Rehomed),
    count = n()
  
  )
count_df <- as.data.frame(result)
print(count_df)

#Comparing breed vs rehomed

selected_columns <- c("Breed", "mean_Rehomed", "sd_Rehomed","skewness_Rehomed")  

# Extract the selected columns from your_data
selected_columns_data <- breed_summary_df[, selected_columns]

# Convert the selected columns into a new data frame
Rehomed_df_breeds <- data.frame(Breed = selected_columns_data[, "Breed"],
                                mean_Rehomed= selected_columns_data[, "mean_Rehomed"],sd_Rehomed = selected_columns_data[, "sd_Rehomed"],
                                skewness_Rehomed = selected_columns_data[, "skewness_Rehomed"])

# View the resulting data frame
print(Rehomed_df_breeds)


# Quantiles
quantiles <- mysample %>%
  group_by(Breed) %>%
  summarize(
    q_0.25_rehomed = quantile(Rehomed, probs = 0.25,type=1), 
    q_0.5_rehomed = quantile(Rehomed, probs = 0.5,type=1),
    q_0.75_rehomed = quantile(Rehomed, probs = 0.75,type=1),
    IQR_rehomed =IQR(Rehomed),
    q_0.25_Health = quantile(Health, probs = 0.25,type=1), 
    q_0.5_Health = quantile(Health, probs = 0.5,type=1),
    q_0.75_Health = quantile(Health, probs = 0.75,type=1),
    IQR_Health = IQR(Health)
  )
quantile_df <- as.data.frame(quantiles)
print(quantile_df)


# Calculate IQR
iqr_value <- IQR(your_data[[selected_column]])
cat("IQR:", iqr_value, "\n")

#Graphical Summaries
boxplot(Rehomed ~ Breed, data = mysample, 
        main = "Boxplot of Rehome time for different breeds",
        xlab = "Breed",
        ylab = "Rehome Weeks")

#outilers
value_to_drop <- -1

# Drop rows with the specified value in column 'B'
mysample <- subset(mysample, Rehomed != value_to_drop)


df_dimensions <- dim(mysample)
cat("Number of rows:", df_dimensions[1], "\n")
cat("Number of columns:", df_dimensions[2], "\n")



#######If continuous

selected_column <- "Rehomed"  # Replace with the actual column name
unique_breeds <- unique(mysample$Breed)

# Create separate plots for each breed
for (i in 1:length(unique_breeds)) {
  breed <- unique_breeds[i]
  subset_data <- mysample[mysample$Breed == breed, ]
  
  # Create a new plot
  par(mfrow = c(1, 1))
  
  # Calculate density
  density_values <- density(subset_data[[selected_column]])
  
  # Plot histogram with density
  hist(subset_data[[selected_column]], main = paste("Histogram for", breed),
       xlab = selected_column, col = "skyblue", border = "black",
       xlim = c(0, max(mysample[[selected_column]])), freq = FALSE)
  
  # Add density curve
  lines(density_values, col = "red", lwd = 2)
}





####if discrete
# Load the libraries
library(dplyr)
library(ggplot2)

# Assuming 'mysample' is your data frame
grouped_data <- mysample %>%
  group_by(Breed)

# Create a histogram for each breed variety
ggplot(grouped_data, aes(x = Rehomed)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~Breed, scales = "free") +
  labs(title = "Histogram of Weeks for Each Breed Variety",
       x = "Weeks",
       y = "Frequency")

#Q-Q plot

# Load necessary library (if not already installed)
install.packages("ggplot2")
library(ggplot2)

ggplot(mysample, aes(sample = Rehomed)) +
  geom_qq() +
  facet_wrap(~Breed, scales = "free") +

  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6),
    strip.text = element_text(size = 8))


#####MODELLING
#Gamma
mysample %>%
  ggplot(aes(x = DF_ROT$Rehomed, fill = Breed)) +
  geom_density(alpha = 0.7) +
  labs(title = "Gamma Distribution of Weeks for Each Breed",
       x = "Rehomed weeks",
       y = "Density") +
  theme_minimal() +
  facet_wrap(~Breed, scales = "free")



#parameter






# Create a list to store separate data frames for each breed
df_list <- list()

# Get unique breeds in the 'breed' column
unique_breeds <- unique(mysample$Breed)

# Loop through each breed and create a new data frame
for (breed in unique_breeds) {
  
  subset_df <- mysample[mysample$Breed == breed, c("Visited",'Rehomed',"Health", 'Breed',"Age","Reason","Returned")]
  
  df_list[[breed]] <- subset_df
}

# Now df_list contains separate data frames for each breed
# You can access them like df_list$Breed1, df_list$Breed2, etc.
df_list["Greyhound"]

#SAMPLE SIZE
r<-nrow(DF_DOB)
print(r)

#Ztest
install.packages("BSDA")

# Load the BSDA package
library(BSDA)
# Assuming df is your data frame and B is the column you want to test
#result_DOB <- z.test(DF_DOB$Rehomed, mu = 27, sigma.x = 74, alternative = "two.sided",conf.level = 0.95)
result_GH <- z.test(DF_GH$Rehomed, mu = 27, sigma.x = 74, alternative = "two.sided",conf.level = 0.95)
result_ROT <- z.test(DF_ROT$Rehomed, mu = 27, sigma.x = 74, alternative = "two.sided",conf.level = 0.95)
print(result_ROT)

# Display the result
print(result)


#T TEST
t.test(DF_DOB$Rehomed, mu = 27, alternative = "two.sided", conf.level = 0.95)


#TWO SAMPLE ZTEST

z_test_result <- z.test(x = DF_DOB$Rehomed, y = DF_GH$Rehomed,mu = 27, sigma.x = 74, alternative = "two.sided",conf.level = 0.95)
print(z_test_result)                    



# Print the result
print(z_test)


#Q-Q PLOT
par(mfrow = c(1, 3), mar = c(5, 4, 2, 1))

qqnorm(DF_GH$Rehomed,main="Greyhound")
qqline(DF_GH$Rehomed)

qqnorm(DF_ROT$Rehomed,main="Rottweiler")
qqline(DF_ROT$Rehomed)

qqnorm(DF_DOB$Rehomed,main="Dobberman")
qqline(DF_DOB$Rehomed)

par(mfrow = c(1, 1))


#K-S Test

mu <- mean(DF_GH$Rehomed)
sigma <- sd(DF_GH$Rehomed)

# Perform the Kolmogorov-Smirnov test:
install.packages("nortest")
library(nortest)
ks.test(x = DF_ROT$Rehomed, y = "pexp", rate = 1) 
ks.test(x = DF_DOB$Rehomed, y = "pnorm", mean  = mu , sd = sigma ) #normal
ks.test(DF_GH$Rehomed, "punif", min = min(DF_ROT$Rehomed), max = max(DF_ROT$Rehomed))

#parameter of doberman for modelling
mean <- mean(DF_DOB$Rehomed)
sd <- sd(DF_DOB$Rehomed)
print(mean)
print(sd)
# Print the mean and standard deviation
cat("Mean:", your_mean, "\n")
cat("Standard Deviation:", your_sd, "\n")

#Confidence Interval

# Perform one-sample z-test for each variable
z_test_var1 <- z.test(DF_GH$Rehomed, mu =27, sigma.x = 74,conf.level = 0.95)
z_test_var2 <- z.test(DF_ROT$Rehomed, mu =27, sigma.x = 74,conf.level = 0.95)
z_test_var3 <- t.test(DF_DOB$Rehomed, mu = 27,conf.level = 0.95)

# Extract means and confidence intervals
mean_var1 <- z_test_var1$estimate
ci_var1 <- z_test_var1$conf.int

mean_var2 <- z_test_var2$estimate
ci_var2 <- z_test_var2$conf.int

mean_var3 <- z_test_var3$estimate
ci_var3 <- z_test_var3$conf.int

# Create a data frame for plotting
plot_data <- data.frame(
  Variable = c("Greyhound", "Rottweiler", "Dobberman"),
  Mean = c(mean_var1, mean_var2, mean_var3),
  LowerCI = c(ci_var1[1], ci_var2[1], ci_var3[1]),
  UpperCI = c(ci_var1[2], ci_var2[2], ci_var3[2])
)

# Plot interval plot
library(ggplot2)

ggplot(plot_data, aes(x = Variable, y = Mean, ymin = LowerCI, ymax = UpperCI)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.2) +
  labs(x = "Breeds", y = "Mean of Rehoming weeks of each breed", title = "Interval Plot showing Confidence Interval of Breeds") +
  theme_minimal()


#Two sample test
result_ROT_DOB = t.test(DF_ROT$Rehomed,DF_DOB$Rehomed)
print (result_ROT_DOB)



# Assuming df is your dataframe with columns x and y
# You need to install and load the 'MASS' package for the gamma distribution
# Install the package if you haven't already
# install.packages("MASS")

# Load the package
library(MASS)
install.packages("MASS")
# Create a gamma plot
gamma_fit <- fitdistr(DF_ROT$Rehomed, "gamma")
gamma_shape <- gamma_fit$estimate["shape"]

# Plot the data points
plot(df$x, df$y, pch = 16, col = "blue", main = "Gamma Plot", xlab = "X", ylab = "Y")

# Add the gamma curve to the plot
curve(dgamma(x, shape = gamma_shape), col = "red", lwd = 2, add = TRUE)

# Add a legend
legend("topright", legend = c("Data Points", "Gamma Curve"), col = c("blue", "red"), lwd = c(1, 2))

