rm(list = ls())

install.packages("readxl")

library(readxl)

getwd()

setwd("C:/Users/user/Documents")

marvel <- read_excel("comic_characters.xlsx")

head(marvel)

head(marvel, 2)

library(ggplot2)


library(ggplot2)

# Assuming your data is named 'marvel'
ggplot(marvel, aes(x = Alignment, y = Universe, color = Alignment)) +
  geom_jitter(position = position_jitter(width = 0.3, height = 0.2), size = 3, alpha = 0.6) +  
  labs(
    title = "Scatterplot: Alignment vs Universe",
    x = "Character Alignment",
    y = "Universe"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Good" = "blue", "Bad" = "red", "Neutral" = "green", "Non-dual" = "purple"))  


library(dplyr)

df_proportions <- marvel %>%
  group_by(Universe, Alignment) %>%
  summarise(Count = n(), .groups = 'drop') %>%  
  group_by(Universe) %>%  
  mutate(Proportion = Count / sum(Count))  

head(df_proportions)

ggplot(df_proportions, aes(x = Universe, y = Proportion, fill = Alignment)) +
  geom_bar(stat = "identity") +  
  labs(
    title = "Proportion of Alignments within Marvel and DC Universes",
    x = "Universe",
    y = "Proportion of Characters"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Good" = "green", "Bad" = "red", "Neutral" = "gray"))


ggplot(marvel, aes(x = Appearances)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.6) +  
  stat_function(fun = dnorm, args = list(mean = mean(marvel$Appearances, na.rm = TRUE), 
                                         sd = sd(marvel$Appearances, na.rm = TRUE)), color = "red", size = 1) +  
  facet_wrap(~ Universe) +  
  labs(
    title = "Histogram of Appearances by Universe with Normal Curve",
    x = "Appearances",
    y = "Density"
  ) +
  theme_minimal()



library(ggplot2)

ggplot(marvel, aes(x = Appearances, y = Universe, fill = Universe)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Boxplot: Character Appearances by Alignment",
    x = "Number of Appearances",
    y = "Universe"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Good" = "blue", "Bad" = "red", "Neutral" = "green"))  



library(ggplot2)
library(dplyr)

# Calculate proportions for normalization
df_normalized <- marvel %>%
  group_by(Alignment, Universe) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Alignment) %>%
  mutate(Proportion = Count / sum(Count) * 100)  


ggplot(df_normalized, aes(x = Alignment, y = Proportion, fill = Universe)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Normalized Stacked Bar Chart: Universe Distribution by Alignment",
    x = "Character Alignment",
    y = "Proportion of Characters (%)",
    fill = "Universe"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Marvel" = "blue", "DC" = "red"))  






library(ggplot2)
marvel$Universe_numeric <- as.numeric(factor(marvel$Universe))

ggplot(marvel, aes(x = Appearances)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(marvel$Appearances, na.rm = TRUE),
                                         sd = sd(marvel$Appearances, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(
    title = "Histogram of Appearances with Normal Curve Overlay",
    x = "Appearances",
    y = "Density"
  ) +
  theme_minimal()
pearson_result <- cor.test(marvel$Appearances, marvel$Universe_numeric, method = "pearson")
print("Pearson's Correlation Test Result:")
print(pearson_result)

# Encode Universe as a numeric variable for correlation
marvel$Universe_numeric <- as.numeric(factor(marvel$Universe))

# Calculate Pearson's r (now Universe_numeric is numeric)
pearson_result <- cor.test(marvel$Appearances, marvel$Universe_numeric, method = "pearson")
print("Pearson's Correlation Test Result:")
print(pearson_result)

# Calculate Spearman's Rho
spearman_result <- cor.test(marvel$Appearances, marvel$Universe_numeric, method = "spearman")
print("Spearman's Correlation Test Result:")
print(spearman_result)





if (!require(reshape2)) {
  install.packages("reshape2")  
}
library(reshape2)
library(ggplot2)

# Select only numeric columns
marvel_numeric <- marvel %>%
  mutate(Universe_numeric = as.numeric(factor(Universe))) %>%  # Encode Universe
  select_if(is.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(marvel_numeric, use = "pairwise.complete.obs", method = "spearman")

# Melt the correlation matrix for heatmap
melted_corr <- melt(correlation_matrix)

# Create a heatmap
ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Correlation Heatmap (Spearman's Rho)",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




library(ggplot2)
library(dplyr)

marvel <- marvel %>%
  mutate(Universe_numeric = as.numeric(factor(Universe)))  
# Wilcoxon/Mann-Whitney U Test
wilcox_test <- wilcox.test(Appearances ~ Universe_numeric, data = marvel, exact = FALSE)
print("Wilcoxon/Mann-Whitney U Test Result:")
print(wilcox_test)

# Visualizing the data to examine the distribution
ggplot(marvel, aes(x = Universe, y = Appearances, fill = Universe)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Boxplot: Appearances by Universe",
    x = "Universe",
    y = "Number of Appearances"
  ) +
  theme_minimal()



library(dplyr)

contingency_table <- table(marvel$Alignment, marvel$Universe)

print("Contingency Table:")
print(contingency_table)

chi_square_result <- chisq.test(contingency_table)

print("Chi-square Test Result:")
print(chi_square_result)

library(ggplot2)
library(reshape2)

# Convert table to data frame for visualization
table_df <- as.data.frame.matrix(contingency_table)

# Add row names as a column manually
table_df$Alignment <- rownames(table_df)
rownames(table_df) <- NULL  

# Reshape data using melt
table_df_melted <- melt(table_df, id.vars = "Alignment", variable.name = "Universe", value.name = "Count")

ggplot(table_df_melted, aes(x = Universe, y = Count, fill = Alignment)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Stacked Bar Chart of Alignment vs Universe",
    x = "Universe",
    y = "Count",
    fill = "Alignment"
  ) +
  theme_minimal()

print(table_df)



# Save the plot as a PNG file
ggsave("alignment_vs_universe.png", width = 10, height = 6, dpi = 300)
