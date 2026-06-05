
isManuscript <- FALSE

# Load packages -----------------------------------------------------------

source("./01-scripts/00-load-packages.R")

# Read environment --------------------------------------------------------

# read the .Renviron document from the project root folder
# Allows for different paths on different devices
readRenviron("./.Renviron")

# Store today's date, to help with versioned output
dateToday <- Sys.Date()


# Define input ------------------------------------------------------------

# get the path to the data folder
dataFolder <- normalizePath(Sys.getenv("DATA_FOLDER"))

# Get the Excel file name
excelFileName <- Sys.getenv("FILE_NAME")

# Define output -----------------------------------------------------------

# This gets prepended to the file names when saving
filePrefix <- Sys.getenv("FILE_PREFIX")

# type of images to save for plots. 
# .png, .svg, or .pdf, for example
imgTypePlots <- Sys.getenv("PLOT_OUTPUT_FORMAT")

# save individual plot files?
savePlots <- Sys.getenv("SAVE_PLOTS")

#Current Computer Type
# some functions and outputs need to be formatted differently depending on Windows or mac
currentCompType <- Sys.getenv("COMP_TYPE")

# Where output should be saved
outputFolder <- normalizePath(Sys.getenv("OUTPUT_FOLDER"))

dataOutputFolder <- normalizePath(file.path(outputFolder, "data"))
plotOutputFolder <- normalizePath(file.path(outputFolder, "plots"))
reportOutputFolder <- normalizePath(file.path(outputFolder, "reports"))


# Script folder -----------------------------------------------------------


#Where R script files are saved
scriptsFolder <- normalizePath(file.path("01-scripts"))



# Source function files ---------------------------------------------------

#Where Function files are saved
functionsFolder <- normalizePath(file.path("02-functions"))

functionFiles <- list.files(
  functionsFolder, 
  full.names = TRUE,
  recursive = TRUE, 
  pattern = "*.R"
)

sapply(functionFiles, source)
