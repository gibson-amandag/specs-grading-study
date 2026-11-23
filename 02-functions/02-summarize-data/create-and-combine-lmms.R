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

makeOmnibusLMM_flextable <- function(model_list){
  
  omnibusLMMs_tbl <- purrr::imap_dfr(
    
    model_list,
    
    function(mod, outcome){
      
      mod$anova_table %>%
        simplifyLMMOutput() %>%
        mutate(outcome = outcome) %>%
        mutate(
          outcome = case_when(
            outcome == "Fixed" ~ "fixed mindset"
            , outcome == "Growth" ~ "growth mindset"
            , outcome == "MasteryGoal" ~ "mastery goals"
            , outcome == "PerformanceGoal" ~ "performance goals"
            , outcome == "SciEfficacy" ~ "science self-efficacy"
            , outcome == "Interest" ~ "science interest"
            , outcome == "Belonging" ~ "academic belonging"
          )
        )
      
    }
    
  )
  
  omnibusLMMs_tbl <- omnibusLMMs_tbl %>%
    pivot_wider(
      id_cols = outcome,
      names_from = variable,
      values_from = c(F, df, p)
    )
  
  omnibusLMMs_header <- data.frame(
    col_keys = c(
      "outcome"
      , "F_cluster", "df_cluster", "p_cluster"
      , "F_time", "df_time", "p_time"
      , "F_cluster * time", "df_cluster * time", "p_cluster * time"
    )
    , line1 = c(
      ""
      , rep("cluster", 3)
      , rep("time", 3)
      , rep("cluster * time", 3)
    )
    , line2 = c(
      "feature"
      , rep(c("F", "df", "p"), 3)
    )
  )
  
 omnibusLMMs_flexTable <-omnibusLMMs_tbl %>%
    makeManuscriptFlexTable(
      headerDF =omnibusLMMs_header
      , round2Cols = c("F_cluster", "F_time", "F_cluster * time")
      , vertLines = c(1, 4, 7)
    )
  
  return(
   omnibusLMMs_flexTable
  )
  
}

extract_cluster_time <- function(model, outcome){
  
  cld_tbl <- emmeans(
    model,
    ~ cluster | Time
  ) %>%
    
    multcomp::cld(
      Letters = letters
    ) %>%
    
    as.data.frame()
  
  cld_tbl %>%
    
    mutate(
      
      outcome = outcome,
      
      # value = sprintf(
      #   "%.2f [%.2f, %.2f] %s",
      #   emmean,
      #   lower.CL,
      #   upper.CL,
      #   str_trim(.group)
      # ),
      
      value = sprintf(
        "%.2f\n[%.2f,%.2f]",
        emmean,
        lower.CL,
        upper.CL
      ),
      
      column = paste0(
        Time,
        "_",
        cluster
      )
      
    ) %>%
    mutate(
      outcome = case_when(
        outcome == "Fixed" ~ "fixed mindset"
        , outcome == "Growth" ~ "growth mindset"
        , outcome == "MasteryGoal" ~ "mastery goals"
        , outcome == "PerformanceGoal" ~ "performance goals"
        , outcome == "SciEfficacy" ~ "science self-efficacy"
        , outcome == "Interest" ~ "science interest"
        , outcome == "Belonging" ~ "academic belonging"
      )
    ) %>%
    
    select(
      outcome,
      column,
      value
    ) %>%
    mutate(
      column = factor(
        column, c("pre_1", "pre_2", "pre_3", "post_1", "post_2", "post_3")
      )
    ) %>%
    arrange(
      outcome
      , column
    )
  
}

makeClusterWithinTime_flextable <- function(model_list){
  
  sig_models <- purrr::keep(
    
    model_list,
    
    ~ .x$anova_table[
      "cluster:Time",
      "Pr(>F)"
    ] < .05
    
  )
  
  tbl <- purrr::imap_dfr(
    sig_models,
    extract_cluster_time
  )
  
  tbl_wide <- tbl %>%
    
    pivot_wider(
      
      id_cols = outcome,
      names_from = column,
      values_from = value
      
    )
  
  flx_header <- data.frame(
    col_keys = c(
      "outcome"
      , "pre_1", "pre_2", "pre_3", "post_1", "post_2", "post_3"
    )
    , line1 = c(
      ""
      , rep("pre", 3)
      , rep("post", 3)
    )
    , line2 = c(
      "feature"
      , rep(c("Cluster 1", "Cluster 2", "Cluster 3"), 2)
    )
  )
  
  flxTbl <- makeManuscriptFlexTable(
    tbl_wide
    , flx_header
    , vertLines = c(1, 4)
  )
  
  flextable(tbl_wide) %>%
    
    theme_booktabs() %>%
    
    autofit()
  
  return(flxTbl)
  
}

extract_time_cluster <- function(model, outcome){
  
  cld_tbl <- emmeans(
    model,
    ~ Time | cluster
  ) %>%
    
    multcomp::cld(
      Letters = letters
    ) %>%
    
    as.data.frame()
  
  cld_tbl %>%
    
    mutate(
      
      outcome = outcome,
      
      value = sprintf(
        "%.2f\n[%.2f,%.2f]",
        emmean,
        lower.CL,
        upper.CL
      ),
      
      column = paste0(
        cluster,
        "_",
        Time
      )
      
    ) %>%
    
    mutate(
      outcome = case_when(
        outcome == "Fixed" ~ "fixed mindset",
        outcome == "Growth" ~ "growth mindset",
        outcome == "MasteryGoal" ~ "mastery goals",
        outcome == "PerformanceGoal" ~ "performance goals",
        outcome == "SciEfficacy" ~ "science self-efficacy",
        outcome == "Interest" ~ "science interest",
        outcome == "Belonging" ~ "academic belonging"
      )
    ) %>%
    
    select(
      outcome,
      column,
      value
    ) %>%
    
    mutate(
      column = factor(
        column,
        c(
          "1_pre", "1_post",
          "2_pre", "2_post",
          "3_pre", "3_post"
        )
      )
    ) %>%
    
    arrange(
      outcome,
      column
    )
  
}

makeTimeWithinCluster_flextable <- function(model_list){
  
  sig_models <- purrr::keep(
    
    model_list,
    
    ~ .x$anova_table[
      "cluster:Time",
      "Pr(>F)"
    ] < .05
    
  )
  
  tbl <- purrr::imap_dfr(
    sig_models,
    extract_time_cluster
  )
  
  tbl_wide <- tbl %>%
    
    pivot_wider(
      
      id_cols = outcome,
      names_from = column,
      values_from = value
      
    )
  
  flx_header <- data.frame(
    col_keys = c(
      "outcome",
      "1_pre", "1_post",
      "2_pre", "2_post",
      "3_pre", "3_post"
    ),
    
    line1 = c(
      "",
      rep("Cluster 1", 2),
      rep("Cluster 2", 2),
      rep("Cluster 3", 2)
    ),
    
    line2 = c(
      "feature",
      rep(c("Pre", "Post"), 3)
    )
  )
  
  flxTbl <- makeManuscriptFlexTable(
    tbl_wide,
    flx_header,
    vertLines = c(1, 3, 5)
  )
  
  return(flxTbl)
  
}
