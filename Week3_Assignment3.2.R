


# Function to remind my_birthday
remind_me <- function(){
  my_birthday <- "5 april"
  return(my_birthday)
}

# DEMONSTATE THAT IT WORKS
remind_me()


# Function to find the answers to questions of week 1
# This function returns blocks of code were you define functions so for example 
# cheat_function("remind_me")
# It works for Q3.1.13 --> "plotstock
# And for the pokergame and monkeytyping function 
cheat_function <- function(block_name) {
  # Check if the function exists
  if (exists(block_name) && is.function(get(block_name))) {
    # Use deparse to obtain the source code
    source_code <- deparse(get(block_name))
    return(source_code)
  } else {
    return(NULL)
  }
}

# DEMONSTATE THAT IT WORKS
plotstock <- function(symbol = "AAPL", year = "2023", filename = "stock_plot.png") {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  getSymbols(symbol, from = start_date, to = end_date)
  chartSeries(get(symbol))
  dev.copy(png, filename)
  dev.off()
}

cheat_function("plotstock")

# Make art function
make_art <- function(seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  n_points <- 400
  x <- runif(n_points, min = 0, max = 1000)
  y <- runif(n_points, min = 0, max = 1000)
  z <- runif(n_points, min = 0, max = 1000)
  colors <- paste("#", sample(letters[1:6], n_points, replace = TRUE), sep = "")
  art_data <- data.frame(x = x, y, z)
  art_plot <- plot_ly(data = art_data, y = ~y, z = ~z,  x = ~x, opacity = .8, color = colors)
  line_trace <- add_trace(art_plot, type = "scatter3d", mode = "lines", line = list(width = 2))
  return(line_trace)
}

# DEMONSTATE IT WORKS
make_art() #Now turn and zoom in/out to get the desired image

