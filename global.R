library(htmltools)
library(RMySQL)

tmp <- ""

share <- list(
  title = "My Life Path",
  url = "http://54.210.79.190",
  image = "http://daattali.com/shiny/img/bcl.png",
  description = "This website generates a map of your life path. ",
  twitter_user = "ZZZapdos"
)

# # setup mysql connection
# options(mysql = list(
#   "host" = "localhost",
#   "port" = 3306,
#   "user" = "xhuang",
#   "password" = "sf123456"
# ))
# databaseName <- "traj_map"
# 
# conn <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host,
#                   port = options()$mysql$port, user = options()$mysql$user,
#                   password = options()$mysql$password)

conn <- dbConnect(MySQL(), host = "localhost", port = 3306, user = "xhuang", password = "sf123456", 
                  dbname = "traj_map")
  
fields_traj <- c('ID', 'city', 'country', 'lat', 'lng', 'pop', 'startTime', 'endTime', 'inputTime', 'reason')
fields_user <- c('gender', 'age')
table_traj <- 'traj_info'
table_user <- "user_info"

singleQueryGen <- function(data){
  # data <- iris[1,]
  raw <- paste(data, collapse = "','")
  raw2 <-  paste("('", raw, "')", sep = "")
  return(raw2)
}

saveData <- function(data, table) {
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s VALUES %s",
    table, 
    paste(apply(data, 1, singleQueryGen), collapse = ", ")
  )
  # Submit the update query and disconnect
  dbGetQuery(conn, query)
  # dbDisconnect(db)
}

# to draw date range input range boxes with only months and years, no dates
# Reference: https://stackoverflow.com/questions/31152960/display-only-months-in-daterangeinput-or-dateinput-for-a-shiny-app-r-programmin
dateRangeMonthsInput <- function(inputId, label, start = NULL, end = NULL,
                                 min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                                 minviewmode = "months", # added manually
                                 weekstart = 0, language = "en", separator = " to ", width = NULL, 
                                 autoclose = TRUE) {
  
  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%Y-%m-%d")
  if (inherits(end,   "Date"))  end   <- format(end,   "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    div(id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
        
        controlLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(class = "input-daterange input-group", 
            span(class = "input-group-addon", "from "), 
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = start, 
              `data-date-autoclose` = if (autoclose) "true" else "false"
            ),
            span(class = "input-group-addon", " to "),
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = end, 
              `data-date-autoclose` = if (autoclose) "true" else "false"
            )
        )
    ),
    datePickerDependency
  )
}


`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# the datePickerDependency is taken from https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
  (function() {
  var datepicker = $.fn.datepicker.noConflict();
  $.fn.bsDatepicker = datepicker;
  })();
  </script>")

# to draw arrows
# function get arrowhead() returns coordinates of a the arrowhead of a line
get_arrowhead <- function (fromPoint, toPoint){
  
  # dx,dy = arrow line vector
  dx <- toPoint[1] - fromPoint[1];
  dy <- toPoint[2] - fromPoint[2];
  
  # normalize
  length <- sqrt(dx * dx + dy * dy);
  unitDx <- dx / length;
  unitDy <- dy / length;
  
  # increase this to get a larger arrow head
  arrowHeadBoxSize = length * .05;
  
  arrowPoint1 <- data.frame(x = (toPoint[1] - unitDx * arrowHeadBoxSize - unitDy * arrowHeadBoxSize),
                      y = (toPoint[2] - unitDy * arrowHeadBoxSize + unitDx * arrowHeadBoxSize));
  arrowPoint2 <- data.frame(x = (toPoint[1] - unitDx * arrowHeadBoxSize + unitDy * arrowHeadBoxSize),
                      y = (toPoint[2] - unitDy * arrowHeadBoxSize - unitDx * arrowHeadBoxSize));
  
  return(plyr::rbind.fill(arrowPoint1, toPoint, arrowPoint2))
  
}
  
rearrange <- function (x, n, wt, perc = F) 
{
  library(rlang)
  wt <- enquo(wt)
  if (quo_is_missing(wt)) {
    vars <- tbl_vars(x)
    wt_name <- vars[length(vars)]
    inform(glue("Selecting by ", wt_name))
    wt <- sym(wt_name)
  }
  if (!is_scalar_integerish(n)) {
    abort("`n` must be a scalar integer")
  }
  if(perc) {
    n <- floor(nrow(x) * n)
  }
  if (n > 0) {
    quo <- quo(rbind(filter(x, min_rank(desc(!!wt)) <= !!n) %>% arrange(desc(!!wt)),
                     filter(x, min_rank(desc(!!wt)) > !!n)))
  }
  else {
    quo <- quo(filter(x, min_rank(!!wt) <= !!abs(n)) %>% arrange(!!wt),
               filter(x, min_rank(!!wt) > !!abs(n)))
  }
  eval_tidy(quo)
}
