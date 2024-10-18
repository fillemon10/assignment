library(dagitty)
library(ggdag)
library(ggplot2)

smoking_ca_dag <- dagify(
  LUNG_CANCER ~ SMOKING + AGE,
  SMOKING ~ PEER_PRESSURE + ALCOHOL_CONSUMING + AGE,
  COUGHING ~ LUNG_CANCER + SMOKING,
  FATIGUE ~ LUNG_CANCER + CHRONIC_DISEASE,
  YELLOW_FINGERS ~ SMOKING,
  ALCOHOL_CONSUMING ~ AGE + PEER_PRESSURE,
  WHEEZING ~ SMOKING + LUNG_CANCER,
  labels = c(
    "LUNG_CANCER" = "Lung\n Cancer",
    "SMOKING" = "Smoking",
    "PEER_PRESSURE" = "Peer\n Pressure",
    "ALCOHOL_CONSUMING" = "Alcohol\n Consuming",
    "COUGHING" = "Coughing",
    "YELLOW_FINGERS" = "Yellow\n Fingers",
    "AGE" = "Age",
    "CHRONIC_DISEASE" = "Chronic\n Disease",
    "FATIGUE" = "Fatigue",
    "WHEEZING" = "Wheezing"
  ),
  exposure = "SMOKING",
  outcome = "LUNG_CANCER"
)

dag_plot <- ggdag(smoking_ca_dag, text = FALSE, use_labels = "label") +
  theme_dag()

ggsave("smoking_ca_dag.png", dag_plot, width = 10, height = 10, dpi = 300)

library(rethinking)

# Data preparation remains the same
d <- read.csv("https://www.torkar.se/data-slc.csv")

d$GENDER_bin <- ifelse(d$GENDER == 'M', 1, 0)
d$LUNG_CANCER_bin <- ifelse(d$LUNG_CANCER == 'YES', 1, 0)

vars_to_binary <- c('SMOKING', 'YELLOW_FINGERS', 'ANXIETY', 'PEER_PRESSURE', 
                    'CHRONIC.DISEASE', 'FATIGUE', 'ALLERGY', 'WHEEZING', 
                    'ALCOHOL.CONSUMING', 'COUGHING', 'SHORTNESS.OF.BREATH', 
                    'SWALLOWING.DIFFICULTY', 'CHEST.PAIN')

for (var in vars_to_binary) {
  d[[paste0(var, '_bin')]] <- ifelse(d[[var]] == 2, 1, 0)
}

d$AGE_std <- standardize(d$AGE)

# Select only the new binary columns and the standardized AGE column
new_columns <- c('GENDER_bin', 'LUNG_CANCER_bin', paste0(vars_to_binary, '_bin'), 'AGE_std')
d <- d[, new_columns]
# Replace '.' with '_' in column names
colnames(d) <- gsub("\\.", "_", colnames(d))

# Define the model
model1_prior <- ulam(
  alist(
    LUNG_CANCER_bin ~ dbinom(1, p),
    logit(p) <- a + b_smoke * SMOKING_bin + b_age * AGE_std + b_alcohol * ALCOHOL_CONSUMING_bin + b_peer * PEER_PRESSURE_bin,
    a ~ dnorm(0, 1.5),
    c(b_smoke, b_age, b_alcohol, b_peer) ~ dnorm(0, 0.5)
  ),
  data = d,
  chains = 1,
  iter = 1000,
  sample = "prior"
)

# Check the model's generated Stan code
stancode(model1_prior)

# Extract prior samples
prior_samples <- extract.prior(model1_prior, n = 1000)

# Compute prior predictive probabilities
set.seed(123)
simulated_p <- inv_logit(prior_samples$a + 
                           prior_samples$b_smoke * d$SMOKING_bin + 
                           prior_samples$b_age * d$AGE_std + 
                           prior_samples$b_alcohol * d$ALCOHOL_CONSUMING_bin + 
                           prior_samples$b_peer * d$PEER_PRESSURE_bin)

# Plot the prior predictive distribution
hist(simulated_p, breaks = 50, main = "Prior Predictive Distribution", 
     xlab = "Probability of Lung Cancer")

# Save the plot
dev.copy(png, "prior_predictive.png")
dev.off()

# Fitting Model 1
fit_model1 <- ulam(
  alist(
    LUNG_CANCER_bin ~ dbinom(1, p),
    logit(p) <- a + b_smoke * SMOKING_bin + b_age * AGE_std + b_alcohol * ALCOHOL_CONSUMING_bin + b_peer * PEER_PRESSURE_bin,
    a ~ dnorm(0, 1.5),
    c(b_smoke, b_age, b_alcohol, b_peer) ~ dnorm(0, 0.5)
  ),
  data = d,
  chains = 4,
  cores = 4,
  iter = 2000
)
  fit_model3 <- ulam(
    alist(
      LUNG_CANCER_bin ~ dbinom(1, p),
      logit(p) <- a +
        b_smoke * SMOKING_bin +
        b_age * AGE_std +
        b_alcohol * ALCOHOL_CONSUMING_bin +
        b_peer * PEER_PRESSURE_bin +
        b_cough * COUGHING_bin +
        b_wheeze * WHEEZING_bin +
        b_fatigue * FATIGUE_bin +
        b_yellow * YELLOW_FINGERS_bin,
      a ~ dnorm(0, 1.5),
      c(b_smoke, b_age, b_alcohol, b_peer,
        b_cough, b_wheeze, b_fatigue, b_yellow) ~ dnorm(0, 0.5)
    ),
    data = d,
    chains = 4,
    cores = 4,
    iter = 2000
  )
  # Trace plots for selected parameters in updated Model 3
  traceplot(fit_model3, pars = c("b_smoke", "b_age", "b_cough", "b_wheeze"))
  
  # Save the trace plots
  dev.copy(png, "trace_plots.png")
  dev.off()

  # Simulate posterior predictive data
  ppc <- sim(fit_model3, data = d, n = 1000)
  
  # Plotting posterior predictive distribution
  dens(ppc, col = "blue", lwd = 2, xlab = "Lung Cancer Probability", main = "Posterior Predictive Check")
  abline(v = mean(d$Lung_Cancer_bin), col = "red", lwd = 2)
  
  # Save the plot
  dev.copy(png, "posterior_predictive.png")
  dev.off()
  
model3_prior <- ulam(
  alist(
    LUNG_CANCER_bin ~ dbinom(1, p),
    logit(p) <- a +
      b_smoke * SMOKING_bin +
      b_age * AGE_std +
      b_alcohol * ALCOHOL_CONSUMING_bin +
      b_peer * PEER_PRESSURE_bin +
      b_cough * COUGHING_bin +
      b_wheeze * WHEEZING_bin +
      b_fatigue * FATIGUE_bin +
      b_yellow * YELLOW_FINGERS_bin,
    a ~ dnorm(0, 1.5),
    c(b_smoke, b_age, b_alcohol, b_peer,
      b_cough, b_wheeze, b_fatigue, b_yellow) ~ dnorm(0, 0.5)
  ),
  data = d,
  chains = 1,
  iter = 1000,
  sample = "prior"
)

# Extract prior samples
prior_samples <- extract.prior(model3_prior, n = 1000)

# Compute prior predictive probabilities
set.seed(123)
simulated_p <- inv_logit(prior_samples$a +
                           prior_samples$b_smoke * d$SMOKING_bin +
                           prior_samples$b_age * d$AGE_std +
                           prior_samples$b_alcohol * d$ALCOHOL_CONSUMING_bin +
                           prior_samples$b_peer * d$PEER_PRESSURE_bin +
                           prior_samples$b_cough * d$COUGHING_bin +
                           prior_samples$b_wheeze * d$WHEEZING_bin +
                           prior_samples$b_fatigue * d$FATIGUE_bin +
                           prior_samples$b_yellow * d$YELLOW_FINGERS_bin)

# Plot the prior predictive distribution
hist(simulated_p, breaks = 50, main = "Prior Predictive Distribution for Model 3",
     xlab = "Predicted Lung Cancer Probability", col = "skyblue")

# Save the plot
dev.copy(png, "prior_predictive_model3.png")
dev.off()


# Trace plots for selected parameters in updated Model 3
traceplot(fit_model3, pars = c("b_smoke", "b_age", "b_cough", "b_wheeze"))

# Save the trace plots
dev.copy(png, "trace_plots.png")
dev.off()

# Summarize the fit with diagnostics
library(rethinking)
summary_stats <- precis(fit_model3, depth = 2)
print(summary_stats)

# Extract the relevant information
rhat_values <- summary_stats@output$Rhat
neff_values <- summary_stats@output$n_eff
parameters <- rownames(summary_stats@output)

# Create a data frame
diagnostics_table <- data.frame(
  Parameter = parameters,
  Rhat = rhat_values,
  neff = neff_values
)

# View the table
print(diagnostics_table)
# Generate posterior predictive simulations
set.seed(123)
post <- extract.samples(fit_model3)

# Number of simulations
n_sims <- 1000

# Initialize matrix to store simulations
ppc_matrix <- matrix(NA, nrow = n_sims, ncol = nrow(d))

for (i in 1:n_sims) {
  # Sample parameters from the posterior
  a <- post$a[i]
  b_smoke <- post$b_smoke[i]
  b_age <- post$b_age[i]
  b_alcohol <- post$b_alcohol[i]
  b_peer <- post$b_peer[i]
  b_cough <- post$b_cough[i]
  b_wheeze <- post$b_wheeze[i]
  b_fatigue <- post$b_fatigue[i]
  b_yellow <- post$b_yellow[i]
  
  # Compute linear predictor
  logit_p <- a +
    b_smoke * d$SMOKING_bin +
    b_age * d$Age_std +
    b_alcohol * d$ALCOHOL.CONSUMING_bin +
    b_peer * d$PEER_PRESSURE_bin +
    b_cough * d$COUGHING_bin +
    b_wheeze * d$WHEEZING_bin +
    b_fatigue * d$FATIGUE_bin +
    b_yellow * d$YELLOW_FINGERS_bin
  
  # Convert to probability
  p <- inv_logit(logit_p)
  
  # Simulate outcome
  y_sim <- rbinom(n = nrow(d), size = 1, prob = p)
  
  # Store simulation
  ppc_matrix[i, ] <- y_sim
}
# Calculate proportion of positive cases in observed data
prop_observed <- mean(d$Lung_Cancer_bin)

# Calculate proportion of positive cases in simulations
prop_simulated <- rowMeans(ppc_matrix)

# Plot the distribution of simulated proportions
hist(prop_simulated, breaks = 30, col = "lightblue",
     xlab = "Proportion of Lung Cancer Cases",
     main = "Posterior Predictive Check for Model 3")
abline(v = prop_observed, col = "red", lwd = 2)
legend("topright", legend = c("Observed Proportion"), col = "red", lwd = 2)
