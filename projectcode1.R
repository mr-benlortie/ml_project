# load ggplot for basic discovery plots
library(ggplot2)

# read the csv file
stroke <- read.csv("C:/Users/blort/OneDrive/Desktop/MSBR70260/Project/stroke.csv", na.strings = "N/A", 
                   header = TRUE, stringsAsFactors = TRUE)

# get an overview of the data frame and variables
summary(stroke)
str(stroke)

# convert binary variables to factors
stroke$hypertension <- as.factor(stroke$hypertension)
stroke$heart_disease <- as.factor(stroke$heart_disease)
stroke$stroke <- as.factor(stroke$stroke)

# remove n/a values
stroke <- na.omit(stroke)

# get an updated overview of the data frame and variables
summary(stroke[, -1])
str(stroke)

# separate data by stroke column
no_stroke <- stroke[stroke$stroke == "0", ]
yes_stroke <- stroke[stroke$stroke == "1", ]

## plots for exploratory viz
plot1 <- ggplot(stroke, aes(ever_married))+
  geom_bar(fill = "indianred", col = "black", alpha = 0.5, width = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Count", title = "Count of Occurrences", subtitle = "Patient Marriage History", 
       caption = "Q: Has a patient ever been married?")
plot1

plot2 <- ggplot(stroke, aes(smoking_status))+
  geom_bar(fill = "indianred", col = "black", alpha = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Count", title = "Count of Occurrences", subtitle = "Patient Smoking Status", 
       caption = "Q: Has a patient ever smoked before?")
plot2

plot3 <- ggplot(stroke, aes(hypertension))+
  geom_bar(fill = "indianred", col = "black", alpha = 0.5, width = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Count", title = "Count of Occurrences", subtitle = "Patient Hypertension", 
       caption = "Q: Has a patient exhibit signs of hypertension?")
plot3

plot4 <- ggplot(stroke, aes(heart_disease))+
  geom_bar(fill = "indianred", col = "black", alpha = 0.5, width = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Count", title = "Count of Occurrences", subtitle = "Patient Heart Disease", 
       caption = "Q: Has a patient exhibit signs or have history of heart disease?")
plot4

plot5 <- ggplot(stroke, aes(age))+
  geom_histogram(bins = 15, fill = "indianred", col = "black", alpha = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  labs(x = "Distribution of Patient Ages", y = "Count", title = "Histogram", subtitle = "Age")
plot5

plot6 <- ggplot(stroke, aes(avg_glucose_level))+
  geom_histogram(bins = 15, fill = "indianred", col = "black", alpha = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  labs(x = "Distribution of Average Patient Glucose Levels", y = "Count", title = "Histogram", subtitle = "Average Glucose Levels")
plot6

plot7 <- ggplot(stroke, aes(bmi))+
  geom_histogram(bins = 25, fill = "indianred", col = "black", alpha = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  labs(x = "Distribution of Patient BMI", y = "Count", title = "Histogram", subtitle = "BMI")
plot7

plot8_mean <- mean(stroke$bmi)

plot8 <- ggplot(stroke, aes(bmi))+
  geom_density(fill = "indianred", col = "black", alpha = 0.5)+
  geom_vline(aes(xintercept = plot8_mean), linetype = "dashed", size = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  labs(x = "BMI", y = "Density", title = "Density Plot", subtitle = "BMI")
plot8

plot9_mean <- mean(log(stroke$bmi + 1))

plot9 <- ggplot(stroke, aes(log(bmi + 1)))+
  geom_density(fill = "indianred", col = "black", alpha = 0.5)+
  geom_vline(aes(xintercept = plot9_mean), linetype = "dashed", size = 0.5)+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())+
  labs(x = "Log of BMI", y = "Density", title = "Density Plot", subtitle = "Log of BMI")
plot9

plot10 <- ggplot(stroke, aes(stroke, fill = hypertension))+
  geom_bar(col = "black", alpha = 0.5)+
  facet_wrap(~ stroke, scales = "free")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Number of Patients", title = "Comparative Columns", subtitle = "Proportion of Patients with Hypertension",
       fill = "Hypertension")+
  scale_fill_manual(values = c("0" = "gray95", "1" = "indianred"),
                    labels = c("0" = "No Hypertension", "1" = "Existing Hypertension"))+
  scale_x_discrete(labels = c("0" = "No Stroke", "1" = "Stroke"))
plot10

plot11 <- ggplot(stroke, aes(stroke, fill = ever_married))+
  geom_bar(col = "black", alpha = 0.5)+
  facet_wrap(~ stroke, scales = "free")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Number of Patients", title = "Comparative Columns", subtitle = "Number of Patients by Marital Status",
       fill = "Ever Married")+
  scale_fill_manual(values = c("No" = "gray95", "Yes" = "indianred"))+
  scale_x_discrete(labels = c("0" = "No Stroke", "1" = "Stroke"))
plot11

plot12 <- ggplot(stroke, aes(stroke, fill = heart_disease))+
  geom_bar(col = "black", alpha = 0.5)+
  facet_wrap(~ stroke, scales = "free")+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Number of Patients", title = "Comparative Columns", subtitle = "Proportion of Patients with Heart Disease",
       fill = "Heart Disease")+
  scale_fill_manual(values = c("0" = "gray95", "1" = "indianred"),
                    labels = c("0" = "No Heart Disease", "1" = "Heart Disease"))+
  scale_x_discrete(labels = c("0" = "No Stroke", "1" = "Stroke"))
plot12