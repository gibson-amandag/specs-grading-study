set_sum_contrasts()

# Raw ---------------------------------------------------------------------

responses_df <- responses_df_raw
responses_post_df <- responses_post_df_raw
demographics_df <- demographics_df_raw
questions_df <- questions_df_raw
scales_df <- scales_df_raw
phraseTable_df <- phraseTable_df_raw
lab_df <- lab_df_raw


# Make Factors ------------------------------------------------------------

responses_df <- responses_df %>%
  makeFactors(
    c("ID", "Time")
  )

responses_post_df <- responses_post_df %>%
  makeFactors(
    c("ID", "Time")
  )

demographics_df <- demographics_df %>%
  makeFactors(
    c("ID")
  )

lab_df <- lab_df %>%
  makeFactors(
    c("ID")
  )

questions_df <- questions_df %>%
  makeFactors(
    c("scale")
  )

scales_df <- scales_df %>%
  makeFactors(
    c("scale", "qType")
  )

phraseTable_df <- phraseTable_df %>%
  makeFactors(
    c("qType")
  )


# Combine -----------------------------------------------------------------

questions_df <- questions_df %>%
  left_join(
    scales_df
    , by = "scale"
  )

duplicatePostIDs <- responses_df %>%
  filter(
    Time == "post"
  ) %>%
  filter(
    ID %in% responses_post_df$ID
  ) %>%
  pull(ID)


combinedResponses <- responses_df %>%
  filter(
    !(Time == "post" & (ID %in% duplicatePostIDs))
  ) %>%
  left_join(
    demographics_df %>%
      select(
        -Name
      )
    , by = "ID"
  ) %>%
  mutate(
    Q11 = as.character(Q11)
  ) %>%
  bind_rows(
    responses_post_df
  ) %>%
  rowwise() %>%
  mutate(
    Q2 = convertNumToPhrase(Q2, getQType("Q2", numOrName = "num"))
  ) %>%
  rename(
    sectionID = Q2
  ) %>%
  mutate(
    sectionID = factor(sectionID, levels = c("RU02", "RU03"))
    , Time = factor(Time, levels = c("pre", "post"))
  ) %>%
  left_join(
    lab_df
    , by = "ID"
  )




# Convert to phrase -------------------------------------------------------

combinedResponses_phrases <- combinedResponses %>%
  mutate(
    across(
      starts_with("Q"),
      ~ sapply(.x, convertNumToPhrase, questionType = getQType(cur_column(), numOrName = "num"))
    )
  )


# Update question names ---------------------------------------------------

questions_df <- questions_df %>%
  group_by(
    scale
  ) %>%
  mutate(
    qName = case_when(
      questionID == "Q10_5_TEXT" ~ "Gender_2"
      , questionID == "Q11" ~ "Age_1"
      , TRUE ~ paste0(scale, "_", row_number())
    ) 
  ) %>%
  relocate(
    qName, scale, .before = "questionText"
  ) %>%
  ungroup()

questionName_lookup <- questions_df %>%
  select(qName, questionID) %>%
  deframe()

combinedResponses <- combinedResponses %>%
  rename(
    any_of(questionName_lookup)
  )

combinedResponses_phrases <- combinedResponses_phrases %>%
  rename(
    any_of(questionName_lookup)
  )


# Scale scores ------------------------------------------------------------

# Sum is _score, excludes NA
# Average is _score_byAvg, allows NA, calculates from the other scale measures
# Have to exclude newly created _score column, though

revCodedQs <- questions_df %>%
  filter(reverseCoded == TRUE) %>%
  pull(qName)

scaleScores <- combinedResponses %>%
  mutate(
    across(
      all_of(revCodedQs),
      ~ 6 - .x ## Reverse coded, all on a 5-point scale right now
    )
  ) %>%
  mutate(
    Fixed_score = rowSums(across(starts_with("Fixed_")), na.rm=FALSE)
    , Fixed_score_byAvg = rowMeans(across(starts_with("Fixed_") & !ends_with("_score")), na.rm = TRUE)
    , Growth_score = rowSums(across(starts_with("Growth_")), na.rm=FALSE)
    , Growth_score_byAvg = rowMeans(across(starts_with("Growth_")& !ends_with("_score")), na.rm = TRUE)
    , SciEfficacy_score = rowSums(across(starts_with("SciEfficacy_")), na.rm=FALSE)
    , SciEfficacy_score_byAvg = rowMeans(across(starts_with("SciEfficacy_") & !ends_with("_score")), na.rm = TRUE)
    , MasteryGoal_score = rowSums(across(starts_with("MasteryGoal_")), na.rm=FALSE)
    , MasteryGoal_score_byAvg = rowMeans(across(starts_with("MasteryGoal_") & !ends_with("_score")), na.rm = TRUE)
    , PerformanceGoal_score = rowSums(across(starts_with("PerformanceGoal_")), na.rm=FALSE)
    , PerformanceGoal_score_byAvg = rowMeans(across(starts_with("PerformanceGoal_") & !ends_with("_score")), na.rm = TRUE)
    , Interest_score = rowSums(across(starts_with("Interest_")), na.rm=FALSE)
    , Interest_score_byAvg = rowMeans(across(starts_with("Interest_") & !ends_with("_score")), na.rm = TRUE)
    , BelongingPre_score = rowSums(across(starts_with("BelongingPre_")), na.rm=FALSE)
    , BelongingPre_score_byAvg = rowMeans(across(starts_with("BelongingPre_") & !ends_with("_score")), na.rm = TRUE)
    , BelongingPost_score = rowSums(across(starts_with("BelongingPost_")), na.rm=FALSE)
    , BelongingPost_score_byAvg = rowMeans(across(starts_with("BelongingPost_") & !ends_with("_score")), na.rm = TRUE)
    , ClassEngagement_score = rowSums(across(starts_with("ClassEngagement_")), na.rm=FALSE)
    , ClassEngagement_score_byAvg = rowMeans(across(starts_with("ClassEngagement_") & !ends_with("_score")), na.rm = TRUE)
    , .before = Fixed_1
  )
