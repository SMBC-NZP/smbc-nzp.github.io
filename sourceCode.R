#=================================================================================*
# ---- functions ----
#=================================================================================*

# Function searches packages in installed package list, adds them if you don't have them, and loads the library:

smartLibrary <- function(packageVector){
  for(i in 1:length(packageVector)){
    package <- packageVector[i]
    if(!package %in% rownames(installed.packages())){
      install.packages(packageVector[i],repos="http://cran.rstudio.com/",
                       dependencies=TRUE)
    }
  }
  lapply(packageVector, library, character.only = TRUE)
}

# Add theme to all plot output:

theme_add <- function(){
  theme(
    axis.title = element_text(size = rel(1.5))
  )
}

#=================================================================================*
# ---- packages, settings, and options ----
#=================================================================================*

# Add packages:

smartLibrary('tidyverse')

# Options:

knitr::opts_chunk$set(echo = TRUE)

#=================================================================================*
# ---- data ----
#=================================================================================*

exampleFactor <- factor(c('three','two','one', 'one'))

exampleFactorLevels <- factor(
  exampleFactor,
  levels = c('one', 'two', 'three')
)

exampleFactorLabels <- factor(
  exampleFactorLevels,
  labels = c('One', 'Two', 'Three')
)

numericVector <- c(1, 1, 2, 3)

#---------------------------------------------------------------------------------*
# ---- github data ----
#---------------------------------------------------------------------------------*

fileURL <- getURL('https://raw.githubusercontent.com/SMBC-NZP/smbc-nzp.github.io/master/rWorkshop/data/birdCounts.csv')

birdCounts <- read.csv(text = fileURL) %>%
  tbl_df

