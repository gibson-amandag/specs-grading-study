#' make factors
#'
#' @param df a dataframe
#' @param cols the columns that you wish to make factors; if using multiple use c(...)
#'
#' @return a dataframe with the indicated columns made factors
#' @export
#'
#' @examples
#' Demo_dam %>%
#' makeFactors(c(
#'  damID,
#'  dam,
#'  ParaType,
#'  litterNum,
#'  cohort
#' ))
#' 
#' makeFactors(Off_ID, c(mouseID))
#' makeFactors(EndPara_off, mouseID)
makeFactors <- function(df, cols){
  df <- df %>%
    mutate(
      across(
        {{ cols }},
        as.factor
      )
    )
  return(df)
}

convertNumToPhrase <- function(baseNum, questionType, withBreaks = FALSE, phraseTable = phraseTable_df){
  if(! (questionType %in% phraseTable$qType)){
    return(baseNum)
  }
  if(is.na(baseNum)){
    return(NA)
  }
  phrase <- phraseTable %>%
    filter(
      qType == questionType
      , baseNumber == baseNum
    ) %>%
    pull(
      if(withBreaks) wBreaks else phrase
    ) %>%
    as.character()
  
  if(length(phrase)==0){
    return(baseNum)
  }
  return(phrase)
}

getQType <- function(qID, numOrName = "name", questions = questions_df){
  if((numOrName == "name" && !(qID %in% questions$qName)) || (numOrName == "num" && !(qID %in% questions$questionID))){
    print("couldn't find")
    return("")
  }
  questionType <- questions %>%
    filter(
      if(numOrName == "name") qName == qID else questionID == qID
    ) %>%
    pull(
      qType
    ) %>%
    as.character()
  return(questionType)
}