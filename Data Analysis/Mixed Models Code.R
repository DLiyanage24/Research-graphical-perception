library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)

# CONNECT TO DATABASE
con <- dbConnect(SQLite(), "data/app_data.sqlite")

# READ TABLES
participants <- dbReadTable(con, "participants")
trial_plan <- dbReadTable(con, "trial_plan")
text_summary <- dbReadTable(con, "text_summary")
highlighted_regions <- dbReadTable(con, "highlighted_regions")
recordings <- dbReadTable(con, "recordings")
trial_timing <- dbReadTable(con, "trial_timing")


# KEEP 500 COMPLETED PARTICIPANTS
# AFTER 2026-05-07
completed_participants <- participants %>%
  mutate(
    started_at = as.POSIXct(started_at)
  ) %>%
  filter(
    completed == 1,
    started_at >= as.POSIXct("2026-05-07")
  ) %>%
  arrange(started_at) %>%
  slice(-(1:6)) %>%
  slice(1:500)

completed_ids <- completed_participants %>%
  select(participant_id, session_id)

# FILTER TABLES
trial_plan_clean <- trial_plan %>%
  semi_join(completed_ids,
            by = c("participant_id", "session_id"))

text_clean <- text_summary %>%
  semi_join(completed_ids,
            by = c("participant_id", "session_id"))

highlight_clean <- highlighted_regions %>%
  semi_join(completed_ids,
            by = c("participant_id", "session_id"))

recordings_clean <- recordings %>%
  semi_join(completed_ids,
            by = c("participant_id", "session_id"))

timing_clean <- trial_timing %>%
  semi_join(
    completed_ids,
    by = c("participant_id", "session_id")
  )


# CREATE ACCURACY DATA

# Text accuracy
text_acc <- text_clean %>%
  left_join(
    trial_plan_clean,
    by = c("participant_id", "session_id", "trial_n", "lineup_file")
  ) %>%
  filter(method == "Text") %>%
  mutate(
    correct = ifelse(plotIndex == target_plotIndex, 1, 0)
  ) %>%
  select(
    participant_id, session_id, trial_n,
    dataset_id, plot_type, method, correct
  )

# Highlight accuracy
highlight_acc <- highlight_clean %>%
  left_join(
    trial_plan_clean,
    by = c("participant_id", "session_id", "trial_n")
  ) %>%
  filter(method == "Highlight") %>%
  mutate(
    correct = ifelse(plotIndex == target_plotIndex, 1, 0)
  ) %>%
  select(
    participant_id, session_id, trial_n,
    dataset_id, plot_type, method, correct
  )

# Talk accuracy
talk_acc <- recordings_clean %>%
  left_join(
    trial_plan_clean,
    by = c("participant_id", "session_id", "trial_n")
  ) %>%
  filter(method == "Talk") %>%
  mutate(
    correct = ifelse(sel1 == target_plotIndex, 1, 0)
  ) %>%
  select(
    participant_id, session_id, trial_n,
    dataset_id, plot_type, method, correct
  )

# Combine methods
all_accuracy <- bind_rows(
  text_acc,
  highlight_acc,
  talk_acc
)

# Remove possible duplicates
all_accuracy_unique <- all_accuracy %>%
  group_by(
    participant_id,
    session_id,
    trial_n,
    dataset_id,
    plot_type,
    method
  ) %>%
  summarise(
    correct = max(correct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    method = factor(method),
    plot_type = factor(plot_type),
    participant_id = factor(participant_id),
    dataset_id = factor(dataset_id),
    trial_n = as.numeric(trial_n)
  )

# ACCURACY MODEL
# Binary outcome: correct = 1/0


#Main effects only model
acc_model <- glmer(
  correct ~ method + plot_type + trial_n +
    (1 | participant_id) +
    (1 | dataset_id),
  family = binomial,
  data = all_accuracy_unique,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(acc_model)


exp(fixef(acc_model))

exp(confint(
  acc_model,
  parm = "beta_",
  method = "Wald"
))



# Model with interactions
acc_model2 <- glmer(
  correct ~
    method * plot_type + trial_n +
    (1 | participant_id) +
    (1 | dataset_id),
  data = all_accuracy_unique,
  family = binomial
)

summary(acc_model2)



#Compare the 2 models
anova(acc_model, acc_model2, test = "Chisq")

#check this model also
acc_model_randomSlope <- glmer(
  correct ~
    method +
    plot_type +
    trial_n +
    (1 + method | participant_id) +
    (1 | dataset_id),
  data = all_accuracy_unique,
  family = binomial,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

anova(acc_model, acc_model_randomSlope)
AIC(acc_model, acc_model_randomSlope)
library(performance)
check_singularity(acc_model_randomSlope)

#Go with the simpler model


####################################################################

# CREATE RESPONSE TIME DATA

rt_all <- timing_clean %>%
  left_join(
    trial_plan_clean %>%
      select(
        participant_id,
        session_id,
        trial_n,
        dataset_id,
        plot_type,
        method
      ),
    by = c(
      "participant_id",
      "session_id",
      "trial_n"
    )
  ) %>%
  mutate(
    method = coalesce(method.y, method.x),
    response_time_min = duration_sec / 60
  ) %>%
  select(
    participant_id,
    session_id,
    trial_n,
    dataset_id,
    plot_type,
    method,
    response_time_min
  ) %>%
  filter(
    !is.na(response_time_min),
    is.finite(response_time_min),
    response_time_min > 0,
    !is.na(method),
    !is.na(plot_type),
    !is.na(dataset_id)
  ) %>%
  mutate(
    log_response_time = log(response_time_min),
    
    # Set reference categories explicitly
    method = factor(
      method,
      levels = c("Highlight", "Talk", "Text")
    ),
    
    plot_type = factor(
      plot_type,
      levels = c("box", "scatter")
    ),
    
    participant_id = factor(participant_id),
    dataset_id = factor(dataset_id),
    trial_n = as.numeric(trial_n)
  )


# Main-effects model
rt_model_main <- lmer(
  log_response_time ~
    method +
    plot_type +
    trial_n +
    (1 | participant_id) +
    (1 | dataset_id),
  data = rt_all,
  REML = FALSE
)
summary(rt_model_main)

#Percentage-change
rt_main_effects <- coef(summary(rt_model_main)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  mutate(
    time_ratio = exp(Estimate),
    percent_change = 100 * (time_ratio - 1)
  )

rt_main_effects


# Interaction model
rt_model_interaction <- lmer(
  log_response_time ~
    method * plot_type +
    trial_n +
    (1 | participant_id) +
    (1 | dataset_id),
  data = rt_all,
  REML = FALSE
)

summary(rt_model_interaction)

# Model comparison
anova(rt_model_main, rt_model_interaction)
AIC(rt_model_main, rt_model_interaction)

# Percentage changes
100 * (exp(fixef(rt_model_main)) - 1)

#After model selection, refit the final model using REML
rt_model_final <- lmer(
  log_response_time ~
    method +
    plot_type +
    trial_n +
    (1 | participant_id) +
    (1 | dataset_id),
  data = rt_all,
  REML = TRUE
)

summary(rt_model_final)

#pairwise comparisons among all methods
library(emmeans)

rt_method_emm <- emmeans(
  rt_model_final,
  ~ method
)

summary(
  pairs(
    rt_method_emm,
    adjust = "tukey"
  ), infer =c(TRUE, TRUE))


#model adjusted response times 
rt_method_estimates <- summary(
  rt_method_emm,
  infer = TRUE
) %>%
  as.data.frame() %>%
  mutate(
    estimated_time_min = exp(emmean),
    lower_time_min = exp(asymp.LCL),
    upper_time_min = exp(asymp.UCL)
  )

rt_method_estimates



# MODEL DIAGNOSTICS

library(performance)
check_singularity(rt_model_final)

par(mfrow = c(1, 2))
plot(
  fitted(rt_model_main),
  resid(rt_model_main),
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs fitted"
)

abline(h = 0, lty = 2)

qqnorm(
  resid(rt_model_main),
  main = "Normal Q-Q plot"
)

qqline(resid(rt_model_main))

par(mfrow = c(1, 1))



################################################################
#Relationship between accuracy and response time

# COMBINE ACCURACY AND RESPONSE TIME
accuracy_rt <- all_accuracy_unique %>%
  select(
    participant_id,
    session_id,
    trial_n,
    dataset_id,
    plot_type,
    method,
    correct
  ) %>%
  inner_join(
    rt_all %>%
      select(
        participant_id,
        session_id,
        trial_n,
        dataset_id,
        plot_type,
        method,
        response_time_min,
        log_response_time
      ),
    by = c(
      "participant_id",
      "session_id",
      "trial_n",
      "dataset_id",
      "plot_type",
      "method"
    )
  ) %>%
  filter(
    !is.na(correct),
    !is.na(log_response_time),
    is.finite(log_response_time)
  ) %>%
  mutate(
    correct = as.integer(correct),
    
    method = factor(
      method,
      levels = c("Highlight", "Talk", "Text")
    ),
    
    plot_type = factor(
      plot_type,
      levels = c("box", "scatter")
    ),
    
    participant_id = factor(participant_id),
    dataset_id = factor(dataset_id),
    trial_n = as.numeric(trial_n)
  )

# check if one row per trial
nrow(accuracy_rt)

table(accuracy_rt$correct)
summary(accuracy_rt$response_time_min)



# BASIC ACCURACY–RESPONSE TIME MODEL
acc_rt_model_basic <- glmer(
  correct ~
    log_response_time +
    method +
    plot_type +
    trial_n +
    (1 | participant_id) +
    (1 | dataset_id),
  data = accuracy_rt,
  family = binomial(link = "logit"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(acc_rt_model_basic)

#This basic model combines two different questions:
#Are participants who are generally slower more accurate?
#Is a participant more accurate when they spend longer than usual on a particular trial?

# -------------------------------------------------
# DECOMPOSE RESPONSE TIME
# Separate between-participant and within-participant response time

accuracy_rt <- accuracy_rt %>%
  group_by(
    participant_id,
    session_id
  ) %>%
  mutate(
    # Participant's average log response time
    participant_mean_log_rt =
      mean(log_response_time, na.rm = TRUE),
    
    # Difference between this trial and the
    # participant's usual response time
    within_participant_log_rt =
      log_response_time - participant_mean_log_rt
  ) %>%
  ungroup() %>%
  mutate(
    # Center the participant means around the
    # overall participant mean
    participant_mean_log_rt_c =
      participant_mean_log_rt -
      mean(participant_mean_log_rt, na.rm = TRUE),
    
    # Center trial number so the intercept is more meaningful
    trial_n_c =
      trial_n - mean(trial_n, na.rm = TRUE)
  )


#ACCURACY–RESPONSE TIME MODEL
acc_rt_model_main <- glmer(
  correct ~
    participant_mean_log_rt_c +  #between-participant effect- Are participants who are generally slower across the study more or less accurate than participants who are generally faster?
    within_participant_log_rt + #within-participant effect- When a participant spends longer than their usual time on a particular trial, are they more likely to answer correctly?
    method +
    plot_type +
    trial_n_c +
    (1 | participant_id) +
    (1 | dataset_id),
  data = accuracy_rt,
  family = binomial(link = "logit"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(acc_rt_model_main)


# Odds ratios
acc_rt_odds_ratios <- data.frame(
  term = names(fixef(acc_rt_model_main)),
  estimate = fixef(acc_rt_model_main),
  odds_ratio = exp(fixef(acc_rt_model_main))
)

acc_rt_odds_ratios

# Confidence intervals
acc_rt_ci <- confint(
  acc_rt_model_main,
  parm = "beta_",
  method = "Wald"
)

acc_rt_results <- data.frame(
  term = names(fixef(acc_rt_model_main)),
  estimate = fixef(acc_rt_model_main),
  odds_ratio = exp(fixef(acc_rt_model_main)),
  lower_or = exp(acc_rt_ci[, 1]),
  upper_or = exp(acc_rt_ci[, 2])
)

acc_rt_results


# RESPONSE TIME × METHOD INTERACTION
acc_rt_model_interaction <- glmer(
  correct ~
    participant_mean_log_rt_c +
    within_participant_log_rt * method +
    plot_type +
    trial_n_c +
    (1 | participant_id) +
    (1 | dataset_id),
  data = accuracy_rt,
  family = binomial(link = "logit"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(acc_rt_model_interaction)
# Interaction terms are not significant
