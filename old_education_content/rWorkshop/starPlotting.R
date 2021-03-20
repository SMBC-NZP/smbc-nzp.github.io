# Make star data frame:

starFramer <- function(frameSize, nStars, sizeProbability, 
                       sizeClasses, sizes){
  starryNight <- data_frame(
    x = sample(1:frameSize, nStars, replace = FALSE),
    y = sample(1:frameSize, nStars, replace = FALSE),
    size = sample(
      sizeClasses, nStars, 
      prob = sizeProbability, 
      replace = TRUE
    )
  )
  return(starryNight)
}

# Star theme:

starTheme <- function(x){
  theme(
    legend.position = 'none',
    plot.margin = unit(c(0,0,0,0), "null"),
    plot.background = element_rect(fill = 'black'),
    panel.background = element_rect(fill = 'black'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
}

# Plot star data frame:
  
starPlotter <- function(starFrame){
  starFrame %>%
    rowwise() %>%
    mutate(pointSize = sample(1:size, 1)) %>%
    ggplot(aes(x = x, y = y, size = pointSize)) +
    geom_point(color = 'white') +
    scale_size_area(max_size = 1) +
    scale_x_continuous(1:10000, expand = c(0,0)) +
    scale_y_continuous(1:10000, expand = c(0,0)) +
    starTheme()
}

#------------------------------------------------------------------*
# ---- Bouncing: ----
#------------------------------------------------------------------*

starryNight <- starFramer(
  frameSize = 30,
  nStars = 20,
  sizeProbability = c(.5, .25, .22, .03, .01),
  sizeClasses = 1:5
)

reps <- 1000

frameList <- vector('list', length = reps)

frameList[[1]] <- starryNight %>%
  mutate(mover = 1)
#   mutate(mover = sample(c(-1, 1), 
#                         nrow(starryNight),
#                         replace = TRUE))

plotList <- vector('list', length = reps)
  
for(i in 2:reps){
  frameList[[i]] <- frameList[[i-1]] %>%
    mutate(
      mover = ifelse(x >= 30, -1, mover),
      mover = ifelse(x <= 1, 1, mover),
      x = x + mover
      )
  outPlot[[i]] <- frameList[[i]] %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(size = 6) +
    scale_x_continuous(1:30, expand = c(0,0)) +
    scale_y_continuous(1:30, expand = c(0,0)) +
    theme(
      legend.position = 'none',
      plot.margin = unit(c(0,0,0,0), "null"),
#       plot.background = element_rect(fill = 'black'),
#       panel.background = element_rect(fill = 'black'),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  plotList[[i]] <- print(outPlot[[i]])
}

# Plotting stars:

starryNight <- starFramer(
  frameSize = 10000,
  nStars = 1500,
  sizeProbability = c(.5, .25, .22, .03, .01),
  sizeClasses = 1:5
)

reps <- 100

frameList <- vector('list', length = reps)

frameList[[1]] <- starryNight

plotList <- vector('list', length = reps)

for(i in 2:reps){
  frameList[[i]] <- frameList[[i-1]] %>%
    mutate(
      x = ifelse(x >= 10000, x - 100, x + 100),
      x = ifelse(x == 1, x + 100, x)
    )
  
  frameList[[i]] %>%
    starPlotter %>%
    print
}




