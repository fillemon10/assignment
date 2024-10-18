library(rethinking)

d <- read.csv("https://www.torkar.se/data-slc.csv")

d$GENDER_bin <- ifelse(d$GENDER == 'M', 1, 0)
d$LUNG_CANCER_bin <- ifelse(d$LUNG_CANCER == 'YES', 1, 0)

list_of_varibles <- c('SMOKING', 'YELLOW_FINGERS', 'ANXIETY', 'PEER_PRESSURE', 
                    'CHRONIC.DISEASE', 'FATIGUE', 'ALLERGY', 'WHEEZING', 
                    'ALCOHOL.CONSUMING', 'COUGHING', 'SHORTNESS.OF.BREATH', 
                    'SWALLOWING.DIFFICULTY', 'CHEST.PAIN')

for (var in list_of_varibles) {
  d[[paste0(var, '_bin')]] <- ifelse(d[[var]] == 2, 1, 0)
}

d$AGE_std <- standardize(d$AGE)

new_columns <- c('GENDER_bin', 'LUNG_CANCER_bin', paste0(vars_to_binary, '_bin'), 'AGE_std')
d <- d[, new_columns]

colnames(d) <- gsub("\\.", "_", colnames(d))

summary(d)