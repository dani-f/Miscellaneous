######################
#
# Visualize data in Excel
# A convenient helper function to quickly visualize your data in Excel (or, in this case, .csv)
#
# Seen here: https://twitter.com/brodriguesco/status/1447468259725434886
# 
######################

excel_visualize <- function(.data){
  # consider using if(interactive())... to avoid undesired file executions
  tmp <- tempfile(fileext = ".csv")
  write.csv2(.data, tmp)
  file.show(tmp)
  invisible(.data) # to continue piping
}

library(dplyr)
mtcars %>% excel_visualize()
mtcars %>% excel_visualize() %>% count(cyl)
