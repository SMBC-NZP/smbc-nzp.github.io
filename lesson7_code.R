#=================================================================================*
# ---- setup ----
#=================================================================================*

# Load libraries:

library(RCurl)
library(lubridate)

# Load a source script:

script <-
  getURL(
    "https://raw.githubusercontent.com/bsevansunc/workshop_languageOfR/master/sourceCode_lesson6.R"
  )

# Evaluate then remove the source script:

eval(parse(text = script))

rm(script)

#=================================================================================*
# ---- birdBanding ----
#=================================================================================*

caps <- read.csv('captureTable.csv') %>%
  as_tibble

names(caps) <- str_replace_all(names(caps), 'CaptureCapture', '')

bandMeasures <- caps %>%
  select(spp, bandNumber, enc, date, mass, wing, tl, age, sex) %>%
  filter(mass != '99999', wing != '99999', tl != '99999', age != 'HY') %>%
  mutate_at(c('mass', 'wing', 'tl'), as.numeric) %>%
  na.omit %>%
  group_by(spp) %>%
  mutate(n = n()) %>%
  ungroup %>%
  filter(n > 100) %>%
  select(-n) %>%
  filter(age != 'U', sex != 'U')

outlierBottom <- function(x){
  median(x) - 2.5*mad(x)
}

outlierTop <- function(x){
  median(x) + 2.5*mad(x)
}

bandMeasuresClean <- bandMeasures %>%
  group_by(spp, sex) %>%
  mutate(
    massB = outlierBottom(mass),
    massT = outlierTop(mass),
    wingB = outlierBottom(wing),
    wingT = outlierTop(wing),
    tlB = outlierBottom(tl),
    tlT = outlierTop(tl)
  ) %>%
  ungroup %>%
  filter(mass > massB & mass < massT,
         wing > wingB & wing < wingT,
         tl > tlB & tl < tlT) %>%
  select(spp:sex)

birdMeasures <- bandMeasuresClean %>%
  filter(age != 'noData', sex != 'noData') %>%
  filter(spp %in% c('GRCA', 'CACH', 'AMRO', 'BCCH'))

#=================================================================================*
# ---- plotting distributions ----
#=================================================================================*

#---------------------------------------------------------------------------------*
# ---- histogram ----
#---------------------------------------------------------------------------------*

# Creating a ggplot starts with specifying the data to be plotted and 
# the "aesthetics" of the plot:

ggplot(birdMeasures, aes(x = mass))

# Next, we add an argument specifying how to plot the data. Here, we plot
# a histogram:

ggplot(birdMeasures, aes(x = mass)) +
  geom_histogram()

# We received a warning message, because there are undoubtedly more informative
# and prettier ways to bin the data than the bins that are chosen automatically.
# We can specify the width or number of bins as an argument of geom_histogram:


ggplot(birdMeasures, aes(x = mass)) +
  geom_histogram(binwidth = .5)

ggplot(birdMeasures, aes(x = mass)) +
  geom_histogram(bins = 20)

# The distribution of the data are just plain strange. Remember that
# there are 4 species present. Let's add a "fill" argement to the 
# aesthetics. We can add the new aesthetic to the plot itself:

ggplot(birdMeasures, aes(x = mass, fill = spp)) +
  geom_histogram(bins = 20)

# But, as fill is a quality of the bars rather than the plot window,
# it's best practice to add the new aesthetic to the geometry:

ggplot(birdMeasures, aes(x = mass)) +
  geom_histogram(aes(fill = spp), bins = 20)

# Notice that the first argument of ggplot is the data being plotted? That's 
# a clue that these data should be moved out front and piped in:

birdMeasures %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = spp), bins = 20)

# This makes the code clearer and frees us up a bit. For example, if we
# wanted to compare just the Black-capped chickadee and Carolina chickadee,
# we could add a filter argument:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = spp), bins = 20)

#---------------------------------------------------------------------------------*
# ---- violin plots ----
#---------------------------------------------------------------------------------*

# Violin plots can be a very straightforward way to observe differences in 
# distributions:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = spp, y = mass)) +
  geom_violin(aes(fill = spp))

# The function coord_flip can be used to switch the x and y axis:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = spp, y = mass)) +
  geom_violin(aes(fill = spp)) +
  coord_flip()


#---------------------------------------------------------------------------------*
# ---- density plots ----
#---------------------------------------------------------------------------------*

# Another option is to plot the density of each species:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = spp))

# To better observe both groups, you can adjust transparency levels using the 
# "alpha" parameter:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = spp), alpha = 0.5)

#=================================================================================*
# ---- facets ----
#=================================================================================*

# The overlap between the species mass measurements might be
# clearer if we split the plot. We can do so using facet_wrap:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = spp), alpha = 0.5) +
  facet_wrap(~spp)

# The above is clearer, but specifying the number of rows provides
# makes size comparisons between the species more clear:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = spp), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2)

# With the extra space we've created, we can easily use the above to display
# size differences based on species and sex:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2)

#=================================================================================*
# ---- colors ----
#=================================================================================*

# The default colors that ggplot uses are just plain ugly. Luckily it's easy
# to modify the colors to those of your choosing. Here we'll use the function
# scale_fill_manual to specify the colors for the fill:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = c('blue', 'red'))

# There really wasn't much improvement here. My preference is to hunt around
# for colors I like and grab their hex codes. For example, I used a "color-picker"
# app to extract the colors of Team Zissou's uniforms in movie The Life Aquatic.

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = c('#9EB8C5', '#F32017'))

# It's a good idea to save the colors that you like for future use. For example,
# I could have have saved my colors as:

zPalette <- c('#9EB8C5', '#F32017')

# You can then apply your color palette using:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = zPalette)

# If we're happy with the results and are ready to start refining the plot,
# we can assign a name:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = zPalette)

#=================================================================================*
# ---- scaling axes ----
#=================================================================================*

# One thing we notice here is that the plot, by default, goes below 0 on the y 
# axis and well outside of the bounds of the data on the x axis. We can fix this 
# using "scale_y_continuous" and scale_x_continuous. 

# Let's explicitly set the y axis:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = zPalette) +
  scale_y_continuous(expand = c(0,0)) 

# Hmmmm ... maybe add a little space to the top:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = zPalette) +
  scale_y_continuous(limits = c(0,.8), expand = c(0,0))

# And explicitly set line breaks:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = zPalette) +
  scale_y_continuous(
    limits = c(0, .8),
    breaks = seq(0, .8, by = 0.1),
    expand = c(0, 0)
  )

# Let's do the same with the x axis:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_fill_manual(values = zPalette) +
  scale_y_continuous(
    limits = c(0, .8),
    breaks = seq(0, .8, by = 0.1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(7, 14),
    breaks = 7:14,
    expand = c(0, 0)
  )

#=================================================================================*
# ---- labels ----
#=================================================================================*

# We want our legend and facet labels to be more informative. The "easiest" way to
# do this is by modifying the data frame. One method for doing so is to changing 
# species to a factor and setting the factor labels. We'll assign a name tp this
# plot so that we can make a lot more changes without having to rewrite the whole
# thing.

massPlot <- birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  mutate(
    spp = factor(spp, labels = c('Black-capped', 'Carolina')),
    sex = factor(sex, labels = c('Female', 'Male'))) %>%
  ggplot(aes(x = mass)) +
  geom_density(aes(fill = sex), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  scale_y_continuous(
    limits = c(0, .8),
    breaks = seq(0, .8, by = 0.1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    limits = c(7, 14),
    breaks = 7:14,
    expand = c(0, 0)
  )

# The above is a start, but not a very professional looking plot. Let's start
# by capitalizing the x and y labels:

massPlot +
  xlab('Mass') +
  ylab('Density')

# To add a title to the whole plot, we can use the ggtitle argument:
  
massPlot +
  xlab('Mass') +
  ylab('Density') +
  ggtitle('Mass of Carolina and Black-capped chickadees')

# You can add/modify labels and the title of your plot one step:

massPlot +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density')

# "labs" can also be used to change the legend title, by specifying a label
# for the fill aesthetic:

massPlot +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex')

#=================================================================================*
# ---- themes ----
#=================================================================================*

# We're almost there, but it still isn't very handsome. It's time to modify the
# themes, which defines how the plot looks. 

# Themes are defined based on theme elements. These include:
# - element_rect 
# - element_text
# - element_line

# Now we'll start changing the way it looks. Let's remove that terrible
# panel background using element_rect:

massPlot +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white')
  )

# I liked having the panel lines, so let's bring them in using 
# element_line:

massPlot +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
  )

# How about the facet strips?

massPlot +
  labs(title = 'Mass of Carolina and Black-capped chickadees',
       x = 'Mass',
       y = 'Density',
       fill = 'Sex') +
  theme(
    panel.background = element_rect(fill = 'white'),
    panel.grid.major = element_line(color = 'gray80', size = .2),
    strip.background = element_rect(fill = 'white')
  )

# Now I want the y axis lines as well:

massPlot +
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

# Well, that legend title is pretty lame. Let's remove it:

massPlot +
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

# Finally, let's change the size of the text throughout. To make the text
# associated with the tick labels bigger we use axis.text:

massPlot +
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

# To make the axis titles bigger we use axis.title:

massPlot +
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

# To make the facet labels bigger we use axis.title:

massPlot +
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

# Finally, let's make the plot title good and large:

massPlot +
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

massPlot +
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

# That made the title drift off the page. Let's add a line break:

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
    plot.title = element_text(size = 22)
  )

# I still don't like how the title looks, it's too close to the plot.
# we can add a margin to fix this

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
    plot.title = element_text(size = 22, margin = margin(b = 20))
  )

# Let's space out the axis text as well (see ?margin):

massPlot +
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
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    strip.text = element_text(size = 16),
    panel.spacing = unit(1.5, 'lines'),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 18, margin = margin(b = 20))
  )


# That may look a little crazy now, but click the zoom button above the
# plot window. As you resize the plot window, the relative size of the 
# elements change.

#=================================================================================*
# ---- junkyard ----
#=================================================================================*

ggsave('examplePlot.png', width = 7,  height = 7)



#=================================================================================*
# ---- junkyard ----
#=================================================================================*

# It is difficul

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = mass)) +
  geom_histogram(stat = 'density', aes(fill = spp))



# These plots are not very informative still, because the bird
# measurements are on very different scales:

birdMeasures %>%
  ggplot(aes(x = mass)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~spp, scales = 'free')

# I wonder if males and females have different masses, let's
# color by sex:

birdMeasures %>%
  ggplot(aes(x = mass)) +
  geom_histogram(aes(fill = sex), binwidth = 1) +
  facet_wrap(~spp, scales = 'free')


birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) +
  geom_density(aes(fill = spp))  +
  facet_wrap(~spp, scales = 'free')

#=================================================================================*
# ---- points ----
#=================================================================================*

# Creating a ggplot starts with specifying the data to be plotted and 
# the "aesthetics" of the plot (aes):

ggplot(birdMeasures, aes(x = wing, y = mass))

# Notice that the first argument of ggplot function is the data frame being
# plotted. As such, we should pipe the data frame into ggplot:

birdMeasures %>%
  ggplot(aes(x = wing, y = mass))

# Next, we add an argument specifying how to plot the data (geometry). Here, we
# will compare the length of wings by the mass using point geometry:

birdMeasures %>%
  ggplot(aes(x = wing, y = mass)) +
  geom_point()

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = wing, y = mass)) +
  geom_point(aes(color = spp))

# This distribution is quite strange. Perhaps due to the species being plotted?
# We can break this plot into a sub-plot for each species using the facet_wrap
# function:

birdMeasures %>%
  filter(spp %in% c('BCCH', 'CACH')) %>%
  ggplot(aes(x = wing, y = mass)) +
  geom_point() +
  facet_wrap(~spp)

# This plot is still not very informative, because the species are of very
# different sizes. We can set the "scale" argument of facet_wrap such that
# the scale varies by species:

birdMeasures %>%
  ggplot(aes(x = wing, y = mass)) +
  geom_point() +
  facet_wrap(~spp, scale = 'free')

# The results are certainly more informative. I wonder if the size difference
# between males and females may be driving some of the diversity in sizes? To
# explore this, let's color points by by sex. We do so by adding an aesthetic
# to the geom_point argument:

birdMeasures %>%
  ggplot(aes(x = wing, y = mass)) +
  geom_point(aes(color = sex)) +
  facet_wrap(~spp, scale = 'free')

birdMeasures %>%
  ggplot(aes(x = wing, y = mass)) +
  # geom_point(aes(color = sex)) +
  facet_wrap(~spp, scale = 'free') +
  # geom_density2d(aes(color = sex)) +
  stat_density_2d(aes(fill = ..density..), contour = FALSE, geom = 'raster')
