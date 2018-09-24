# Function for reading a .lig file, adapted from Hallworth et al.

read_lig <-
  function(ligFile){
    # Check to see that tidyverse is installed (install if FALSE):
    if(!"tidyverse" %in% rownames(installed.packages())){
      install.packages('tidyverse', dependencies = TRUE)
      print('Installing package and package dependencies')
    }
    # Load tidyverse:
    require(tidyverse)
    # Read in the .lig file and assign column names:
    read.csv(
      ligFile,
      sep = ',',
      skip=1,
      header = FALSE,
      stringsAsFactors = FALSE,
      col.names = c("valid","dateTime","julian","light")) %>%
      # Mutate dateTime object to be a proper dateTime:
      mutate(
        dateTime = dateTime %>%
          strptime("%d/%m/%y %H:%M:%S",tz="GMT") %>%
          as.POSIXct)
  }

# Function for reading a lightBug file, adapted from Hallworth et al.

read_lightBug <-
  function(file) {
    # Check to see that tidyverse is installed (install if FALSE):
    if(!"tidyverse" %in% rownames(installed.packages())){
      install.packages('tidyverse', dependencies = TRUE)
      print('Installing package and package dependencies')
    }
    # Load tidyverse:
    require(tidyverse)
    # Read in the .txt file and assign column names & classes:
    read.table(
      file,
      header = TRUE,
      sep = "\t",
      col.names = c("time", "date", "light"),
      colClasses = c("character", "character", "integer")
    ) %>%
      # Generate a proper dateTime object from date and time variables:
      mutate(dateTime = paste(date, time) %>%
               strptime("%d/%m/%y %H:%M") %>%
               as.POSIXct(tz = 'GMT')) %>%
      # Select just the columns we want for analysis:
      dplyr::select(dateTime, light)
  }

