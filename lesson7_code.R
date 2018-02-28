#---------------------------------------------------------------------------------*
# ---- Set up ----
#---------------------------------------------------------------------------------*

# Load RCurl library:

library(RCurl)

# Load a source script:

script <-
  getURL(
    "https://raw.githubusercontent.com/bsevansunc/workshop_languageOfR/master/sourceCode_lesson6.R"
  )

# Evaluate then remove the source script:

eval(parse(text = script))

rm(script)

library(lubridate)

#---------------------------------------------------------------------------------*
# ---- Initiate plot ----
#---------------------------------------------------------------------------------*

# Define data:

ggplot(birdMeasures)

# Add aesthetic:

ggplot(birdMeasures, aes(x = spp))

#---------------------------------------------------------------------------------*
# ---- Geometries ----
#---------------------------------------------------------------------------------*

ggplot(birdMeasures, aes(x = spp)) +
  geom_bar()

birdMeasures %>%
  ggplot(aes(x = spp)) +
  geom_bar()

birdMeasures %>%
  filter(spp != 'NOCA') %>%
  ggplot(aes(x = spp)) +
  geom_bar()

#---------------------------------------------------------------------------------*
# ---- Geometries: adding arguments ----
#---------------------------------------------------------------------------------*

# Histogram:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram()

# Add binwidth:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(binwidth = 1)

# Add bins:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(bins = 20)

# Add fill:

birdMeasures %>%
  filter(spp != 'NOCA') %>%
  ggplot(aes(x = spp)) +
  geom_bar(fill = 'gray')

# Add color:

birdMeasures %>%
  filter(spp != 'NOCA') %>%
  ggplot(aes(x = spp)) +
  geom_bar(fill = 'gray', 
           color = 'black')

# Add size of border:

birdMeasures %>%
  filter(spp != 'NOCA') %>%
  ggplot(aes(x = spp)) +
  geom_bar(fill = 'gray', 
           color = 'black',
           size = 0.7)
#---------------------------------------------------------------------------------*
# ---- Geometries: adding aesthetics ----
#---------------------------------------------------------------------------------*

# Fill bar plot by region:

birdMeasures %>%
  filter(spp != 'NOCA') %>%
  ggplot(aes(x = spp)) +
  geom_bar(aes(fill = region))

# Fill histogram by sex:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex),
                 bins = 20)

# Color histogram bars:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex),
                 bins = 20,
                 color = 'black')

#---------------------------------------------------------------------------------*
# ---- Facets ----
#---------------------------------------------------------------------------------*

# Facet by species:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), 
                 bins = 20,
                 color = 'black') +
  facet_wrap(~spp)

# Facet by species, 2 rows:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), bins = 20) +
  facet_wrap(~spp, nrow = 2)

#---------------------------------------------------------------------------------*
# ---- Labels ----
#---------------------------------------------------------------------------------*

# Add plot, x, y axis titles:

birdMeasures %>%
  filter(spp != 'NOCA') %>%
  ggplot(aes(x = spp)) +
  geom_bar(aes(fill = region),
           color = 'black',
           size = .7) +
  labs(title = 'Birds banded and recaptured 2000-2017',
       x = 'Species',
       y = 'Count')

# Modify labels with piping:

birdMeasures %>%
  filter(spp != 'NOCA') %>%
  mutate(spp = factor(
    spp,
    labels = c(
      'American robin',
      'Black-capped chickadee',
      'Carolina chickadee',
      'Gray catbird'
    )
  )) %>%
  ggplot(aes(x = spp)) +
  geom_bar(aes(fill = region),
           color = 'black',
           size = .7) +
  labs(title = 'Birds banded and recaptured 2000-2017',
       x = 'Species',
       y = 'Count')

# Assign plot name:

birdCaptures_basicPlot <- birdMeasures %>%
  filter(spp != 'NOCA') %>%
  mutate(spp = factor(
    spp,
    labels = c(
      'American robin',
      'Black-capped chickadee',
      'Carolina chickadee',
      'Gray catbird'
    )
  )) %>%
  ggplot(aes(x = spp)) +
  geom_bar(aes(fill = region),
           color = 'black',
           size = .7) +
  labs(title = 'Birds banded and recaptured 2000-2017',
       x = 'Species',
       y = 'Count')


# Labels for massDensity:

massDensity <- birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  mutate(spp = factor(
    spp,
    labels = c(
      'Black-capped',
      'Carolina'
    )
  )) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex),
               alpha = 0.7) +
  facet_wrap(~spp, nrow = 2) +
  labs(title = "Mass of Carolina and Black-capped chickadees",
       x = 'Mass', 
       y = 'Density')

#---------------------------------------------------------------------------------*
# ---- scaling ----
#---------------------------------------------------------------------------------*

# Remove expansion:

birdCaptures_basicPlot +
  scale_y_continuous(expand = c(0,0))

# Set limits:

birdCaptures_basicPlot +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1500))

# Set breaks:

birdCaptures_basicPlot +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1500),
                     breaks = seq(0, 1500, by = 250))

#---------------------------------------------------------------------------------*
# ---- colors ----
#---------------------------------------------------------------------------------*

# Colors can be set manually:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), 
                 bins = 20,
                 color = 'black') +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = c('blue', 'red'))

# Hunting for good colors online can improve the look of a plot:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), 
                 bins = 20,
                 color = 'black') +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = c('#9EB8C5', '#F32017'))

# Colors can be saved and used later:

zPalette <- c('#9EB8C5', '#F32017')

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), 
                 bins = 20,
                 color = 'black') +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = zPalette)

#---------------------------------------------------------------------------------*
# ---- legends ----
#---------------------------------------------------------------------------------*

# Modify legend by changing the data frame coming in:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  mutate(sex = factor(sex,
                      labels = c('Female','Male'))) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), 
                 bins = 20,
                 color = 'black') +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = c('#9EB8C5', '#F32017'))

ggsave('legend1.png', width = 7, height = 5)

# Modify the legend in scale_fill_manual:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), 
                 bins = 20,
                 color = 'black') +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = c('#9EB8C5', '#F32017'), 
                    name = 'Sex', 
                    labels = c('Female', 'Male'))

#---------------------------------------------------------------------------------*
# ---- Themes ----
#---------------------------------------------------------------------------------*

# Assign name to plot objects, histogram:

histogram2Theme <- birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  mutate(spp = factor(
    spp,
    labels = c(
      'Black-capped',
      'Carolina'
    )
  )) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), 
                 bins = 20,
                 color = 'black') +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 150),
                     breaks = seq(0, 150, by = 25)) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = c('#9EB8C5', '#F32017'), 
                    name = 'Sex', 
                    labels = c('Female', 'Male'))

# Assign name to plot objects, density:

density2Theme <- massDensity +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.8),
                     breaks = seq(0, 0.8, by = 0.1)) +
  scale_fill_manual(values = zPalette,
                    name = 'Sex', 
                    labels = c('Female', 'Male')) 

# Remove gray panel background using element_rect:

histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white')
  )

# Change panel lines using element_line:
  
histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
     x = 'Mass',
     y = 'Density',
     fill = 'Sex') +
  theme(
  panel.background = element_rect(fill = 'white'),
  panel.grid.major = element_line(color = 'gray80', size = .2),
  )

# Modify the strip background using element_rect:
  
histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    strip.background = element_rect(fill = 'white')
  )

# Modify the y axis lines using element_line:
  
histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    axis.line = element_line(color = 'black', size = .5),
    strip.background = element_rect(fill = 'white')
  )

# Remove the legend title using element_blank:


histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    axis.line = element_line(color = 'black', size = .5),
    strip.background = element_rect(fill = 'white'),
    legend.title = element_blank()
  )

# Change the size of tick mark text using axis.text and element_text:
  
histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    axis.line = element_line(color = 'black', size = .5),
    strip.background = element_rect(fill = 'white'),
    legend.title = element_blank(),
    axis.text = element_text(size = 12)
  )

# Make the axis titles bigger we use axis.title and element_text:

histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    axis.line = element_line(color = 'black', size = .5),
    strip.background = element_rect(fill = 'white'),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 18)
  )

# Make the facet labels bigger we use axis.title and element_text:
  
histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    axis.line = element_line(color = 'black', size = .5),
    strip.background = element_rect(fill = 'white'),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 18)
  )

# Make the plot title larger using plot.title and element_text:
  
histogram2Theme +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    axis.line = element_line(color = 'black', size = .5),
    strip.background = element_rect(fill = 'white'),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    plot.title = element_text(size = 22)
  )

# Add a margin between the plot and title (see ?margin):

massPlot +
  labs(title = 'Mass of Carolina and\nBlack-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    axis.line = element_line(color = 'black', size = .5),
    strip.background = element_rect(fill = 'white'),
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 18),
    plot.title = element_text(size = 22, margin = margin(b = 40))
  )