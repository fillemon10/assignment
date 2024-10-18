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
