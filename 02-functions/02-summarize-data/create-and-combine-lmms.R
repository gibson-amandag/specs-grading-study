run_all_section_time_LMMs <- function(scores_df){
  set_sum_contrasts()
  
  # Fixed Mindset ----
  fixedMindset_lmm <- mixed(
    Fixed_score_byAvg ~ sectionID * Time + (1|ID)
    , data = scores_df %>%
      filter(
        !is.na(Fixed_score_byAvg)
      )
    , method = "KR"
  )
  
  
  # Growth Mindset ----
  growthMindset_lmm <- mixed(
    Growth_score_byAvg ~ sectionID * Time + (1|ID)
    , data = scores_df %>%
      filter(
        !is.na(Growth_score_byAvg)
      )
    , method = "KR"
  )
  
  
  # Mastery Goals ----
  masteryGoals_lmm <- mixed(
    MasteryGoal_score_byAvg ~ sectionID * Time + (1|ID)
    , data = scores_df %>%
      filter(
        !is.na(MasteryGoal_score_byAvg)
      )
    , method = "KR"
  )
  
  # Performance Goals ----
  performanceGoals_lmm <- mixed(
    PerformanceGoal_score_byAvg ~ sectionID * Time + (1|ID)
    , data = scores_df %>%
      filter(
        !is.na(PerformanceGoal_score_byAvg)
      )
    , method = "KR"
  )
  
  
  # Science Academic Self-Efficacy -----
  sciEfficacy_lmm <- mixed(
    SciEfficacy_score_byAvg ~ sectionID * Time + (1|ID)
    , data = scores_df %>%
      filter(
        !is.na(SciEfficacy_score_byAvg)
      )
    , method = "KR"
  )
  
  
  # Science Interest ------
  sciInterest_lmm <- mixed(
    Interest_score_byAvg ~ sectionID * Time + (1|ID)
    , data = scores_df %>%
      filter(
        !is.na(Interest_score_byAvg)
      )
    , method = "KR"
  )
  
  
  
  # Academic Belonging ------
  belonging_lmm <- mixed(
    BelongingPre_score_byAvg ~ sectionID * Time + (1|ID)
    , data = scores_df %>%
      filter(
        !is.na(BelongingPre_score_byAvg)
      )
    , method = "KR"
  )
  
  return(
    list(
      fixedMindset_lmm = fixedMindset_lmm
      , growthMindset_lmm = growthMindset_lmm
      , masteryGoals_lmm = masteryGoals_lmm
      , performanceGoals_lmm = performanceGoals_lmm
      , sciEfficacy_lmm = sciEfficacy_lmm
      , sciInterest_lmm = sciInterest_lmm
      , belonging_lmm = belonging_lmm
    )
  )
  
}

makeSectionTimeLMM_flextable <- function(
  lmm_list    
){
  fixedMindset_lmm_tbl = lmm_list$fixedMindset_lmm$anova_table %>%
    simplifyLMMOutput()
  growthMindset_lmm_tbl = lmm_list$growthMindset_lmm$anova_table %>%
    simplifyLMMOutput()
  masteryGoals_lmm_tbl = lmm_list$masteryGoals_lmm$anova_table %>%
    simplifyLMMOutput()
  performanceGoals_lmm_tbl = lmm_list$performanceGoals_lmm$anova_table %>%
    simplifyLMMOutput()
  sciEfficacy_lmm_tbl = lmm_list$sciEfficacy_lmm$anova_table %>%
    simplifyLMMOutput()
  sciInterest_lmm_tbl = lmm_list$sciInterest_lmm$anova_table %>%
    simplifyLMMOutput()
  belonging_lmm_tbl = lmm_list$belonging_lmm$anova_table %>%
    simplifyLMMOutput()
  
  scalesLMMs_tbl <- bind_rows(
    list(
      "fixed mindset" = fixedMindset_lmm_tbl
      , "growth mindset" = growthMindset_lmm_tbl
      , "mastery goals" = masteryGoals_lmm_tbl
      , "performance goals" = performanceGoals_lmm_tbl
      , "science self-efficacy" = sciEfficacy_lmm_tbl
      , "science interest" = sciInterest_lmm_tbl
      , "academic belonging" = belonging_lmm_tbl
    ), .id = "feature"
  ) %>%
    pivot_wider(
      id_cols = "feature"
      , names_from = "variable"
      , values_from = c("F", "df", "p")
    )
  
  scalesLMMs_header <- data.frame(
    col_keys = c(
      "feature"
      , "F_section", "df_section", "p_section"
      , "F_time", "df_time", "p_time"
      , "F_section * time", "df_section * time", "p_section * time"
    )
    , line1 = c(
      ""
      , rep("section", 3)
      , rep("time", 3)
      , rep("section * time", 3)
    )
    , line2 = c(
      "feature"
      , rep(c("F", "df", "p"), 3)
    )
  )
  
  scalesLMMs_flexTable <- scalesLMMs_tbl %>%
    makeManuscriptFlexTable(
      headerDF = scalesLMMs_header
      , round2Cols = c("F_section", "F_time", "F_section * time")
      , vertLines = c(1, 4, 7)
    )
  
  return(
    scalesLMMs_flexTable
  )
}

run_scaleByTime_LMMs <- function(scalesTime_df) {
  model_masteryBySciEff <- mixed(
    MasteryGoal_score_byAvg ~ SciEfficacy_score_byAvg*Time + (1 | ID),
    check_contrasts = FALSE,
    data = scalesTime_df,
    method = "KR"
  )
  
  model_masteryByInterest <- mixed(
    MasteryGoal_score_byAvg ~ Interest_score_byAvg*Time + (1 | ID),
    check_contrasts = FALSE,
    data = scalesTime_df,
    method = "KR"
  )
  
  return(
    list(
      bySciEff = model_masteryBySciEff
      , byInterest = model_masteryByInterest
    )
  )
}

makeScalesTime_flexTable <- function(lmm_list){
  sciEfficacy_tbl <- coef(summary(lmm_list$bySciEff$full_model)) %>%
    subTimeInRowNames() %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    mutate(
      variable =
        ifelse(
          variable == "SciEfficacy_score_byAvg"
          , "avg scale score"
          , ifelse(
            variable == "SciEfficacy_score_byAvg:time"
            , "avg scale score * time"
            , variable
          )
        )
    ) %>%
    rename(
      p = `Pr(>|t|)`
    ) %>%
    formatPCol()
  
  sciInterest_tbl <- coef(summary(lmm_list$byInterest$full_model)) %>%
    subTimeInRowNames() %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    mutate(
      variable =
        ifelse(
          variable == "Interest_score_byAvg"
          , "avg scale score"
          , ifelse(
            variable == "Interest_score_byAvg:time"
            , "avg scale score * time"
            , variable
          )
        )
    ) %>%
    rename(
      p = `Pr(>|t|)`
    ) %>%
    formatPCol()
  
  scalesByTime_tbl <- bind_rows(
    list(
      "science self-efficacy" = sciEfficacy_tbl
      , "science interest" = sciInterest_tbl
    ), .id = "feature"
  )
  
  scalesByTime_flexTable <- scalesByTime_tbl %>%
    makeManuscriptFlexTable(
      round1Cols = c("df")
      , round2Cols = c("Estimate", "t value")
      , round3Cols = c("Std. Error")
      , vertMergeCols = c("feature")
      , horzLines = c(4)
    )
  
  return(scalesByTime_flexTable)
}