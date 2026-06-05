library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)


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



# TEXT ACCURACY
text_acc <- text_clean %>%
  
  left_join(
    trial_plan_clean,
    by = c(
      "participant_id",
      "session_id",
      "trial_n",
      "lineup_file"
    )
  ) %>%
  
  filter(method == "Text") %>%
  
  mutate(
    correct =
      ifelse(plotIndex == target_plotIndex, 1, 0)
  ) %>%
  
  select(
    participant_id,
    session_id,
    dataset_id,
    plot_type,
    method,
    correct
  )


# HIGHLIGHT ACCURACY
highlight_acc <- highlight_clean %>%
  
  left_join(
    trial_plan_clean,
    by = c(
      "participant_id",
      "session_id",
      "trial_n"
    )
  ) %>%
  
  filter(method == "Highlight") %>%
  
  mutate(
    correct =
      ifelse(plotIndex == target_plotIndex, 1, 0)
  ) %>%
  
  select(
    participant_id,
    session_id,
    dataset_id,
    plot_type,
    method,
    correct
  )


# TALK ACCURACY
talk_acc <- recordings_clean %>%
  
  left_join(
    trial_plan_clean,
    by = c(
      "participant_id",
      "session_id",
      "trial_n"
    )
  ) %>%
  
  filter(method == "Talk") %>%
  
  mutate(
    correct =
      ifelse(sel1 == target_plotIndex, 1, 0)
  ) %>%
  
  select(
    participant_id,
    session_id,
    dataset_id,
    plot_type,
    method,
    correct
  )


# COMBINE ALL METHODS
all_accuracy <- bind_rows(
  text_acc,
  highlight_acc,
  talk_acc
)

# REMOVE DUPLICATES
# all_accuracy_unique <- all_accuracy %>%
#   
#   group_by(
#     participant_id,
#     session_id,
#     dataset_id,
#     plot_type,
#     method
#   ) %>%
#   
#   summarise(
#     correct = max(correct, na.rm = TRUE),
#     .groups = "drop"
#   )



all_accuracy_unique <- all_accuracy %>%
  group_by(
    participant_id,
    session_id,
    dataset_id,
    plot_type,
    method
  ) %>%
  summarise(
    correct = max(correct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    trial_plan_clean %>%
      select(participant_id, session_id, dataset_id, plot_type, method, trial_n),
    by = c("participant_id", "session_id", "dataset_id", "plot_type", "method")
  )



# OVERALL ACCURACY
overall_accuracy <- all_accuracy_unique %>%
  
  summarise(
    n = n(),
    
    accuracy =
      mean(correct, na.rm = TRUE),
    
    se =
      sqrt(accuracy * (1 - accuracy) / n),
    
    lower =
      accuracy - 1.96 * se,
    
    upper =
      accuracy + 1.96 * se
  )

overall_accuracy


# OVERALL ACCURACY PLOT
ggplot(overall_accuracy,
       aes(x = "Overall",
           y = accuracy)) +
  
  geom_col(width = 0.5,
           fill = "steelblue") +
  
  geom_errorbar(
    aes(ymin = lower,
        ymax = upper),
    width = 0.15
  ) +
  
  ylim(0, 1) +
  
  labs(
    title = "Overall Accuracy",
    x = "",
    y = "Accuracy"
  ) +
  
  theme_minimal()


# ACCURACY BY METHOD
accuracy_by_method <- all_accuracy_unique %>%
  
  group_by(method) %>%
  
  summarise(
    n = n(),
    
    accuracy =
      mean(correct, na.rm = TRUE),
    
    se =
      sqrt(accuracy * (1 - accuracy) / n),
    
    lower =
      accuracy - 1.96 * se,
    
    upper =
      accuracy + 1.96 * se,
    
    .groups = "drop"
  )

accuracy_by_method


# METHOD ACCURACY PLOT
ggplot(accuracy_by_method,
       aes(x = method,
           y = accuracy)) +
  
  geom_col(width = 0.6,
           fill = "steelblue") +
  
  geom_errorbar(
    aes(ymin = lower,
        ymax = upper),
    width = 0.2
  ) +
  
  ylim(0, 1) +
  
  labs(
    title = "Accuracy by Method",
    x = "Method",
    y = "Accuracy"
  ) +
  
  theme_minimal()


# ACCURACY BY PLOT TYPE
accuracy_by_plottype <- all_accuracy_unique %>%
  
  group_by(plot_type) %>%
  
  summarise(
    n = n(),
    
    accuracy =
      mean(correct, na.rm = TRUE),
    
    se =
      sqrt(accuracy * (1 - accuracy) / n),
    
    lower =
      accuracy - 1.96 * se,
    
    upper =
      accuracy + 1.96 * se,
    
    .groups = "drop"
  )

accuracy_by_plottype


# PLOT TYPE ACCURACY PLOT
ggplot(accuracy_by_plottype,
       aes(x = plot_type,
           y = accuracy)) +
  
  geom_col(width = 0.6,
           fill = "steelblue") +
  
  geom_errorbar(
    aes(ymin = lower,
        ymax = upper),
    width = 0.2
  ) +
  
  ylim(0, 1) +
  
  labs(
    title = "Accuracy by Plot Type",
    x = "Plot Type",
    y = "Accuracy"
  ) +
  
  theme_minimal()




# ACCURACY BY METHOD AND PLOT TYPE
accuracy_method_plot <- all_accuracy_unique %>%
  
  group_by(plot_type, method) %>%
  
  summarise(
    n = n(),
    
    accuracy =
      mean(correct, na.rm = TRUE),
    
    se =
      sqrt(accuracy * (1 - accuracy) / n),
    
    lower =
      accuracy - 1.96 * se,
    
    upper =
      accuracy + 1.96 * se,
    
    .groups = "drop"
  )

accuracy_method_plot

#Plot
ggplot(accuracy_method_plot,
       aes(x = method,
           y = accuracy,
           fill = plot_type)) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(ymin = lower,
        ymax = upper),
    
    position = position_dodge(width = 0.8),
    
    width = 0.2
  ) +
  
  ylim(0, 1) +
  
  labs(
    title = "Accuracy by Method and Plot Type",
    x = "Method",
    y = "Accuracy",
    fill = "Plot Type"
  ) +
  
  theme_minimal()


#Accuracy by trial number
accuracy_by_trial <- all_accuracy_unique %>%
  group_by(trial_n) %>%
  summarise(
    n = n(),
    accuracy = mean(correct, na.rm = TRUE),
    se = sqrt(accuracy * (1 - accuracy) / n),
    lower = accuracy - 1.96 * se,
    upper = accuracy + 1.96 * se,
    .groups = "drop"
  )

accuracy_by_trial

ggplot(accuracy_by_trial,
       aes(x = trial_n, y = accuracy)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = 1:12
  ) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  ylim(0, 1) +
  labs(
    title = "Accuracy by Trial Number",
    x = "Trial Number",
    y = "Accuracy"
  ) +
  theme_minimal()




##################################################

# RESPONSE TIME Analysis


rt_all <- timing_clean %>%
  left_join(
    trial_plan_clean %>%
      select(participant_id, session_id, trial_n, dataset_id, plot_type, method),
    by = c("participant_id", "session_id", "trial_n")
  ) %>%
  mutate(
    response_time_min = duration_sec / 60,
    method = coalesce(method.y, method.x)
  ) %>%
  select(
    participant_id, session_id, trial_n,
    dataset_id, plot_type, method,
    response_time_min
  ) %>%
  filter(
    !is.na(response_time_min),
    response_time_min >= 0
  )


# Total time per participant
rt_by_participant <- rt_all %>%
  group_by(participant_id, session_id) %>%
  summarise(
    n_trials = n(),
    total_time_min = sum(response_time_min, na.rm = TRUE),
    mean_time_per_trial_min = mean(response_time_min, na.rm = TRUE),
    median_time_per_trial_min = median(response_time_min, na.rm = TRUE),
    .groups = "drop"
  )
overall_participant_time <- rt_by_participant %>%
  summarise(
    n_participants = n(),
    mean_total_time_min = mean(total_time_min, na.rm = TRUE),
    median_total_time_min = median(total_time_min, na.rm = TRUE),
    sd_total_time_min = sd(total_time_min, na.rm = TRUE),
    iqr_total_time_min = IQR(total_time_min, na.rm = TRUE)
  )

overall_participant_time



# Overall response time per trial
overall_rt <- rt_all %>%
  summarise(
    n = n(),
    mean_time_min = mean(response_time_min, na.rm = TRUE),
    median_time_min = median(response_time_min, na.rm = TRUE),
    sd_time_min = sd(response_time_min, na.rm = TRUE),
    iqr_time_min = IQR(response_time_min, na.rm = TRUE)
  )

overall_rt

# Response time by method
rt_by_method <- rt_all %>%
  group_by(method) %>%
  summarise(
    n = n(),
    mean_time_min = mean(response_time_min, na.rm = TRUE),
    median_time_min = median(response_time_min, na.rm = TRUE),
    sd_time_min = sd(response_time_min, na.rm = TRUE),
    iqr_time_min = IQR(response_time_min, na.rm = TRUE),
    .groups = "drop"
  )

rt_by_method

# Response time by plot type
rt_by_plot_type <- rt_all %>%
  group_by(plot_type) %>%
  summarise(
    n = n(),
    mean_time_min = mean(response_time_min, na.rm = TRUE),
    median_time_min = median(response_time_min, na.rm = TRUE),
    sd_time_min = sd(response_time_min, na.rm = TRUE),
    iqr_time_min = IQR(response_time_min, na.rm = TRUE),
    .groups = "drop"
  )

rt_by_plot_type


# Response time by method and plot type
rt_by_method_plot <- rt_all %>%
  group_by(method, plot_type) %>%
  summarise(
    n = n(),
    mean_time_min = mean(response_time_min, na.rm = TRUE),
    median_time_min = median(response_time_min, na.rm = TRUE),
    sd_time_min = sd(response_time_min, na.rm = TRUE),
    iqr_time_min = IQR(response_time_min, na.rm = TRUE),
    .groups = "drop"
  )

rt_by_method_plot




ggplot(rt_all, aes(x = method, y = response_time_min)) +
  geom_boxplot() +
  facet_wrap(~ plot_type) +
  labs(
    x = "Method",
    y = "Response time (minutes)",
    title = "Response time by method and plot type"
  ) +
  theme_bw()


# Response time by trial number
rt_by_trial <- rt_all %>%
group_by(trial_n) %>%
  summarise(
    n = n(),
    mean_time_min = mean(response_time_min, na.rm = TRUE),
    median_time_min = median(response_time_min, na.rm = TRUE),
    sd_time_min = sd(response_time_min, na.rm = TRUE),
    iqr_time_min = IQR(response_time_min, na.rm = TRUE),
    .groups = "drop"
  )

rt_by_trial
ggplot(rt_by_trial,
       aes(x = trial_n, y = mean_time_min)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = 1:12
  ) +
  labs(
    title = "Mean Response Time by Trial Number",
    x = "Trial Number",
    y = "Mean Response Time (minutes)"
  ) +
  theme_minimal()


############################################


#Participant-level accuracy distribution
participant_accuracy <- all_accuracy_unique %>%
  group_by(participant_id, session_id) %>%
  summarise(
    n_trials = n(),
    n_correct = sum(correct, na.rm = TRUE),
    accuracy = mean(correct, na.rm = TRUE),
    .groups = "drop"
  )

participant_accuracy_summary <- participant_accuracy %>%
  summarise(
    n_participants = n(),
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    median_accuracy = median(accuracy, na.rm = TRUE),
    min_accuracy = min(accuracy, na.rm = TRUE),
    max_accuracy = max(accuracy, na.rm = TRUE),
    sd_accuracy = sd(accuracy, na.rm = TRUE),
    iqr_accuracy = IQR(accuracy, na.rm = TRUE)
  )

participant_accuracy_summary
ggplot(participant_accuracy,
       aes(x = accuracy)) +
  geom_histogram(binwidth = 1/12, boundary = 0) +
  labs(
    title = "Participant-Level Accuracy Distribution",
    x = "Participant Accuracy",
    y = "Number of Participants"
  ) +
  theme_minimal()




#Accuracy vs response time
participant_rt <- rt_all %>%
  group_by(participant_id, session_id) %>%
  summarise(
    mean_rt_min = mean(response_time_min, na.rm = TRUE),
    median_rt_min = median(response_time_min, na.rm = TRUE),
    total_time_min = sum(response_time_min, na.rm = TRUE),
    .groups = "drop"
  )

participant_acc_rt <- participant_accuracy %>%
  left_join(
    participant_rt,
    by = c("participant_id", "session_id")
  )

participant_acc_rt
ggplot(participant_acc_rt,
       aes(x = mean_rt_min, y = accuracy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Accuracy vs Response Time",
    x = "Mean response time per trial (minutes)",
    y = "Accuracy"
  ) +
  theme_minimal()

cor(
  participant_acc_rt$accuracy,
  participant_acc_rt$mean_rt_min,
  use = "complete.obs"
)

#Same dataset, same participant, three methods
method_compare <- all_accuracy_unique %>%
  select(
    participant_id,
    session_id,
    dataset_id,
    plot_type,
    method,
    correct
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = correct
  )

method_compare
method_patterns <- method_compare %>%
  count(Talk, Highlight, Text) %>%
  arrange(desc(n))

method_patterns
method_patterns <- method_patterns %>%
  mutate(
    percent = n / sum(n) * 100
  )

method_patterns


method_compare %>%
  summarise(
    talk_acc = mean(Talk),
    highlight_acc = mean(Highlight),
    text_acc = mean(Text),
    
    any_correct = mean(Talk == 1 | Highlight == 1 | Text == 1),
    
    all_correct = mean(Talk == 1 & Highlight == 1 & Text == 1),
    
    all_wrong = mean(Talk == 0 & Highlight == 0 & Text == 0)
  )



