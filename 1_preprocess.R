# jonashaslbeck@gmail.com; March 25, 2024

# ----------------------------------------------------
# -------- What is happening here? -------------------
# ----------------------------------------------------

# Load data from original Ethica files and save as RDS
# including the relevant variables

# ----------------------------------------------------
# -------- Loading Packages --------------------------
# ----------------------------------------------------

# Data wrangling
library(plyr)
library(readr)


# ----------------------------------------------------
# -------- Loading Data: Baseline Questionnaire ------
# ----------------------------------------------------

data_baseline <- read_csv("Data//Baseline_questionnaire.csv")

# ----- BSI -----
ind_BSI <- 63:115
# Compute BSI for each person
out_BSI <- ddply(data_baseline, .(Name), function(x) {
  BSI_i <- sum(x[, ind_BSI])
  return(BSI_i)
})
colnames(out_BSI) <- c("Names", "BSI")
# Save RDS file: subjID + BSI
saveRDS(out_BSI, "Files/data_BSI.RDS")

# ----- DASS -----
ind_Stress <- c(116,121,123,126,127,129,133)
ind_Anxiety <- c(117,119,122,124,130,134,135)
ind_Dep <- c(118,120,125,128,131,132,136)
# Compute DASS subscales for each person
out_DASS <- ddply(data_baseline, .(Name), function(x) {
  Stress_i <- sum(x[, ind_Stress])
  Anxiety_i <- sum(x[, ind_Anxiety])
  Dep_i <- sum(x[, ind_Dep])
  return(c(Stress_i, Anxiety_i, Dep_i))
})
colnames(out_DASS)[2:4] <- c("DASS_S", "DASS_A", "DASS_D")
saveRDS(out_DASS, "Files/data_DASS.RDS")


# ----------------------------------------------------
# -------- Loading Data: ESM Questionnaire -----------
# ----------------------------------------------------

# Data Part 1: momentary from occasions with only measurements
data_Lik_raw <- read_csv("Data/Momentary_survey(L).csv")
data_VAS_raw <- read_csv("Data/Momentary_survey(V).csv")

# Data Part 2: momentary from morning
data_Lik_raw_morning <- read_csv("Data/Morning_survey(L).csv")
data_VAS_raw_morning <- read_csv("Data/Morning_survey(V).csv")

# Data Part 3: momentary from evening
data_Lik_raw_evening <- read_csv("Data/Evening_survey(L).csv")
data_VAS_raw_evening <- read_csv("Data/Evening_survey(V).csv")

# choose variables
sel_init <- c(1, # id
              5, # response time
              7, # duration
              10, 16, 20:22, # positive valence
              9, 11:15, 17:19) # negative valence

# Check whether same data structure
colnames(data_Lik_raw)[sel_init]
colnames(data_Lik_raw_morning)[sel_init]
colnames(data_Lik_raw_evening)[sel_init]

# Combine data within respobse scale
data_Lik_cmb <- rbind(data_Lik_raw[, sel_init],
                      data_Lik_raw_morning[, sel_init],
                      data_Lik_raw_evening[, sel_init])
colnames(data_VAS_raw_evening) <- colnames(data_VAS_raw_morning) <- colnames(data_VAS_raw)
data_VAS_cmb <- rbind(data_VAS_raw[, sel_init],
                      data_VAS_raw_morning[, sel_init],
                      data_VAS_raw_evening[, sel_init])

# Make Time Char vector a time object
data_Lik_cmb$`Response Time` <- as.Date(data_Lik_cmb$`Response Time`)
data_VAS_cmb$`Response Time` <- as.Date(data_VAS_cmb$`Response Time`)

# Add Vector indicating type of survey (morning, day, evening)
data_Lik_cmb$SurveyType <- c(rep(2, nrow(data_Lik_raw)),
                             rep(1, nrow(data_Lik_raw_morning)),
                             rep(3, nrow(data_Lik_raw_evening)))
data_VAS_cmb$SurveyType <- c(rep(2, nrow(data_VAS_raw)),
                             rep(1, nrow(data_VAS_raw_morning)),
                             rep(3, nrow(data_VAS_raw_evening)))

N_Lik_raw <- length(unique(data_Lik_cmb$Name))
N_VAS_raw <- length(unique(data_VAS_cmb$Name))
N_Lik_raw
N_VAS_raw

N_VAS_raw+N_Lik_raw


# ----- Format Column Names ------

# choose variables
sel <- c(4:17) # Those are the variables
p <- length(sel)

# Get variable names
names <- c("Name", "RespTime", "Duration", "Happy", "Energetic", "Look Forward",
           "Satisfied (S)", "Satisfied (B)", "Sad", "Guilty", "Ashamed",
           "Disgusted", "Anxiousx", "Irritated", "Discomfort", "Lonely",
           "Stressed", "SurveyTypxe")
# Apply shorter col names to combined data
colnames(data_Lik_cmb) <- names
colnames(data_VAS_cmb) <- names

# ------ Save Data ------
saveRDS(data_Lik_cmb, "Files/data_ESM_Lik.RDS")
saveRDS(data_VAS_cmb, "Files/data_ESM_VAS.RDS")



# ----------------------------------------------------
# -------- Loading Data: Post Questionnaire ----------
# ----------------------------------------------------

data_eval <- read_csv("Data/Evaluation_of_study.csv")
length(unique(data_eval$Name)) 

Post_questions <- c("How did you experience filling out all notification surveys?",
                    "How burdensome was answering the notification surveys?",
                    "How was the frequency of the surveys (i.e. the number of times you were asked to fill in a survey every day)?",
                    "Were the questions clear?",
                    "How difficult was it for you to know the answers to the questions?",
                    "How influential was your participation in this study on your day-to-day life?")

# Subset relevant questions
data_eval_ss <- as.data.frame(cbind(data_eval$Name,
                                    data_eval$`[2_SAQ] How did you experience filling out all notificatio`,
                                    data_eval$`[3_SAQ] How burdensome was answering the notification surv`,
                                    data_eval$`[4_SAQ] How was the frequency of the surveys (i.e. the num`,
                                    data_eval$`[5_SAQ] Were the questions clear?`,
                                    data_eval$`[6_SAQ] How difficult was it for you to know the answers t`,
                                    data_eval$`[7_SAQ] How influential was your participation in this stu`))
colnames(data_eval_ss) <- c("Name",
                            "ExpNotif",
                            "Burden",
                            "Freq",
                            "Clear",
                            "KnowAnswer",
                            "Infl")
data_eval_ss$Group <- NA
data_eval_ss$Group[data_eval_ss$Name %in% data_Lik_cmb$Name] <- "Likert"
data_eval_ss$Group[data_eval_ss$Name %in% data_VAS_cmb$Name] <- "VAS"

saveRDS(data_eval_ss, "Files/data_PostQuestions.RDS")

# How many of those were actually included in the analysis?
data_eval_ss_noNA <- na.omit(data_eval_ss)
nrow(data_eval_ss_noNA)
table(data_eval_ss_noNA$Group)







