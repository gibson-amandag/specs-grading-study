
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  rstudioapi::showQuestion(
    title = "Action Required",
    message = "Please remove the password from the file before proceeding.",
    ok = "Continue",
    cancel = "Cancel"
  )
} else {
  response <- readline(prompt = "Please remove the password from the file and press [Enter] to continue...")
}

responses_df_raw <- loadExcelSheet(dataFolder, excelFileName, "responses")
responses_post_df_raw <- loadExcelSheet(dataFolder, excelFileName, "responses_post")
demographics_df_raw <- loadExcelSheet(dataFolder, excelFileName, "demographics")
lab_df_raw <- loadExcelSheet(dataFolder, excelFileName, "lab")
questions_df_raw <- loadExcelSheet(dataFolder, excelFileName, "questions")
scales_df_raw <- loadExcelSheet(dataFolder, excelFileName, "scales")
phraseTable_df_raw <- loadExcelSheet(dataFolder, excelFileName, "phraseTable")

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  rstudioapi::showQuestion(
    title = "Action Required",
    message = "Please password protect the file again.",
    ok = "Continue",
    cancel = "Cancel"
  )
} else {
  response <- readline(prompt = "Password protect the file again, then press [Enter]...")
}