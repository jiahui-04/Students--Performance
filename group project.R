# install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")

# loading libraries
library(dplyr)
library(ggplot2)
library(psych)

# cleaning of dataset
ds <- read.csv("student_performance_dataset.csv")
ds <- na.omit(ds)
ds <- ds %>% distinct(Student_ID, .keep_all = T)

str(ds)
summary(ds)


# log transformations applied
ds$log_Study_Hours_per_Week <- log(ds$Study_Hours_per_Week) 
ds$log_Attendance_Rate <- log(ds$Attendance_Rate)
ds$log_Past_Exam_Scores <- log(ds$Past_Exam_Scores)
shapiro.test(ds$log_Attendance_Rate)
shapiro.test(ds$log_Study_Hours_per_Week)
shapiro.test(ds$log_Past_Exam_Scores)
shapiro.test(ds$Final_Exam_Score)
# p-value < 0.05, use non-parametric test



# wilcoxon signed rank test (past exam scores vs final exam score)
wilcox.test(ds$Past_Exam_Scores, ds$Final_Exam_Score, paired = T)

# plot density distributions between past and final exam scores
# create new dataset scores with different structure to get density plot
scores <- data.frame(Exam = c(rep("Past_Exam_Scores", nrow(ds)), 
                              rep("Final_Exam_Score", nrow(ds))),
                     Score = c(ds$Past_Exam_Scores, ds$Final_Exam_Score))
ggplot(scores, aes(x = Score, fill = Exam)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Past vs Final Exam Scores",
       x = "Score", 
       y = "Density") +
  scale_fill_manual(values = c("lightblue", "darkolivegreen")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  theme(axis.title = element_text(face = "bold", hjust = 0.5)) +
  theme(axis.text = element_text(face = "bold", size = 10)) +
  theme(legend.text = element_text(face = "bold", hjust = 0.5)) +
  theme(legend.title = element_text(face = "bold", hjust = 0.5))




# wilcoxon test of internet access at home, and changes of scores between
# past and final exam scores
# create new column Score_Change (final - past)
ds$Score_Change <- ds$Final_Exam_Score - ds$Past_Exam_Scores
ds$Score_Change
wilcox.test(ds$Score_Change ~ ds$Internet_Access_at_Home)

boxplot(Score_Change ~ Internet_Access_at_Home, data = ds, 
        main = "Score Change by Internet Access",
        ylab = "Final - Past Exam Score", xlab = "Internet Access")



# friedman rank sum test of attendance rate, past and final exam scores
# select relevant variables and create new dataset ds1 with different structure 
ds1 <- data.frame(Student_ID = rep(ds$Student_ID, times = 3),
                  Measurement = c(rep("Past_Exam_Scores", nrow(ds)), 
                                  rep("Attendance_Rate", nrow(ds)), 
                                  rep("Final_Exam_Score", nrow(ds))),
                  Value = c(ds$Past_Exam_Scores, ds$Attendance_Rate, ds$Final_Exam_Score))
friedman.test(Value ~ Measurement | Student_ID, data = ds1)
pairwise.wilcox.test(ds1$Value, ds1$Measurement, p.adjust.method = "none")



# finding correlations between continuous variables
# create new dataset ds2 with continuous variables
ds2 <- ds[, c(3, 4, 5, 9)]
pairs.panels(ds2, 
             method = "spearman",     # variables are not normal
             pch = 16, 
             density = TRUE, 
             ellipses = FALSE,
             hist.col = "darkseagreen",
             cex.cor = 1.5,
             cex.labels = 1.5)



# linear regression
model1 <- lm(Final_Exam_Score ~ Study_Hours_per_Week + Attendance_Rate + Past_Exam_Scores, 
             data = ds2)
summary(model1)


cat_var <- c("Gender", "Parental_Education_Level", "Internet_Access_at_Home")
ds3_cat <- sapply(ds[, cat_var], as.factor)
ds3 <- ds %>%
  select(2:9) %>%
  mutate(Gender = factor(Gender)) %>%
  mutate(Gender = factor(Parental_Education_Level)) %>%
  mutate(Gender = factor(Internet_Access_at_Home))
str(ds3)
model2 <- lm(Final_Exam_Score ~ ., data = ds3)
summary(model2)
# not much diff even with categorical variables
