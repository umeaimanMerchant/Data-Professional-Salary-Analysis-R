####### DATA PROFESSIONAL SALARY ANALYSIS ########

# IMPORT LIBRARY
library(tidyverse)
library(psych)
library(dplyr)
library(stringr)

# READ DATA
df <-data.frame(read.csv("glassdoor_jobs.csv",
                         stringsAsFactors = F))

# EXPLORE DATA
head(df)
str(df)
describe(df)

# DATA CLEANING

# grouping job titles 
job_categorization_fn <- function(item) {
  if (grepl("machine learning engineer", tolower(item))) {
    return("Scientist")
  }else if (grepl("engineer", tolower(item))) {
    return("Engineer")
  }else if(grepl("manager", tolower(item)) | grepl("director", tolower(item))) {
    return("Manager")
  }else if (grepl("senior", tolower(item)) | (grepl("sr", tolower(item)))) {
    return("Senior")
  }else if (grepl("analyst", tolower(item)) | (grepl("analytics", tolower(item)))) {
    return("Analyst")
  }else {
    return("Scientist")
  }
}

# get Job.Category from Job.Title
df$Job.Category <- sapply(df$Job.Title, job_categorization_fn)
df$Job.Category <- as.factor(df$Job.Category)
table(df$Job.Category)

df[df["Size"]=="Unknown", "Size"] <--1
df$Size <-as.factor(df$Size)
table(df$Size)

# Traditional (more then 50 yr), Middle-age (50yr), New (5 yr) company
company_type_fn <- function(foundation_yr) {
  if (foundation_yr >= 2015){
    return("New firm")
  }else if (foundation_yr >= 1969) {
    return("Middle-age firm")
  }else {
    return("Traditional firm")
  }
}

df$Company.type <- sapply(df$Founded,company_type_fn)
df$Company.type <- as.factor(df$Company.type)
table(df$Company.type)

df$Sector <- as.factor(df$Sector)
df$Industry <- as.factor(df$Industry)

df[df["Type.of.ownership"]=="Unknown", "Type.of.ownership"] <--1
df$Type.of.ownership <-as.factor(df$Type.of.ownership)


df[df["Revenue"]=="Unknown / Non-Applicable", "Revenue"] <--1
df$Revenue <-as.factor(df$Revenue)
table(df$Revenue)

# if working at headquarters
df$Headquarters.Flag <-df$Headquarters == df$Location

table(df$Rating)
df$Rating <-as.factor(df$Rating)


## Salary estimate
df <- df[!(grepl("Per Hour", df$Salary.Estimate)), ]
df <- df[!df["Salary.Estimate"] == -1, ]

df$salary <- df$Salary.Estimate

df$salary <- gsub("(Glassdoor est.)", "", df$Salary.Estimate)
df$salary <- gsub("Employer Provided Salary", "", df$salary)
tmp <- df %>% separate(salary, into = c("Salary","extra"),sep=" ")
tmp <- tmp %>% separate(Salary,into = c("start","end"),sep="-")
tmp$start <- str_extract(tmp$start, "\\d+")
tmp$end <- str_extract(tmp$end, "\\d+")
tmp$start <- as.integer( tmp$start)
tmp$end <- as.integer(tmp$end)
tmp$Avg.Salary <- (tmp$start + tmp$end)/2
  
df$Avg.Salary <- tmp$Avg.Salary
  
## clear unused variables and columns
df$X <- NULL
df$Company.Name <-NULL
df$Competitors <-NULL

tmp <- NULL
subset_df <-NULL  
  
df$Job.Title <-NULL
df$Salary.Estimate <-NULL
df$Job.Description <-NULL
df$Location <- NULL
df$Headquarters <-NULL
df$Founded <-NULL
df$salary <-NULL

# SAVE CLEANED DATA
write.csv(df, "salary_dataset_cleaned.csv", row.names = FALSE)
  
# VISUALIZATION

bar <-ggplot(data=df, aes(x=Job.Category, fill=Sector))
bar + geom_bar() + theme_gray() +
  labs(title = "Bar chart", y = "Counts") 


box <- ggplot(data=df, aes(x=Company.type,y=Avg.Salary))
box + geom_boxplot(outlier.color="black",outlier.shape = 4) +
  geom_jitter(width = 0.2, aes(color=Headquarters.Flag)) +
  labs(title="Box chart",
       x = "Type of Firm") +
  theme_light() +
  coord_flip()


hist <- ggplot(data=df, aes(x=Avg.Salary))
hist + geom_histogram(binwidth = 10,alpha=0.7,
                      fill="darkslategray4",
                      color = "darkslategray") +
  ggtitle("Salary distribution") +
  labs(y="", x="Average Salary") +
  theme_minimal()


sc <- ggplot(df, aes(Avg.Salary,Rating))
sc + geom_point(aes(color=Company.type), shape=21, fill="white",
                size=2,stroke=2) + theme_light() +
  labs(title="Scatter plot")


## HYPOTHESIS TESTING

# H0 - avg salary doesn't change if work location as headquarters
ind.t.res <- t.test(Avg.Salary~Headquarters.Flag, data=df,mu=5)
print(ind.t.res)

# RESULT- Fail to reject null hypothesis - p-value = 0.9952

# H0 - avg salary for traditional and middle level firm is similar
subset_df <- df[!(df$Company.type=="New firm"),]
ind.t.res <- t.test(Avg.Salary~Company.type, data=subset_df,mu=5)
print(ind.t.res)

# RESULT- Fail to reject null hypothesis - p-value = 0.2429

