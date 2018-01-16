outpath <- '/Users/bsevans/Desktop/gits/smbc-nzp.github.io/rWorkshop/images/'

# Functions:

theme_add <- function(){
  theme(
    axis.title = element_text(size = rel(3)),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.text = element_text(size = rel(2)),
    plot.margin=unit(c(1,1,1,1),"cm")
  )
}

# Data:

exampleFactor <- factor(c('three','two','one', 'one'))

exampleFactorLevels <- factor(
  exampleFactor,
  levels = c('one', 'two', 'three')
)

exampleFactorLabels <- factor(
  exampleFactorLevels,
  labels = c('One', 'Two', 'Three')
)

# Plot 1: Raw factor

exampleFactorPlot <- data.frame(exampleFactor) %>%
  ggplot(aes(x = exampleFactor)) + 
  geom_bar(fill = 'gray70', color = 'black', size = 1) +
  scale_y_continuous(limits = c(0, 2.5), expand = c(0,0)) +
  theme_classic() +
  theme_add()

ggsave(
  paste0(outpath, 'exampleFactorPlot.png'),
  exampleFactorPlot,
  scale = 2,
  width = 6,
  height = 4,
  units = 'in'
)

# Plot 2: Factor, levels

exampleFactorLevelsPlot <- data.frame(exampleFactorLevels) %>%
  ggplot(aes(x = exampleFactorLevels)) + 
  geom_bar(fill = 'gray70', color = 'black', size = 1) +
  scale_y_continuous(limits = c(0, 2.5), expand = c(0,0)) +
  theme_classic() +
  theme_add()

ggsave(
  paste0(outpath, 'exampleFactorLevelsPlot.png'),
  exampleFactorLevelsPlot,
  scale = 2,
  width = 6,
  height = 4,
  units = 'in'
)

# Plot 2: Factor, labels

exampleFactorLabelsPlot <- data.frame(exampleFactorLabels) %>%
  ggplot(aes(x = exampleFactorLabels)) + 
  geom_bar(fill = 'gray70', color = 'black', size = 1) +
  scale_y_continuous(limits = c(0, 2.5), expand = c(0,0)) +
  theme_classic() +
  theme_add()

ggsave(
  paste0(outpath, 'exampleFactorLabelsPlot.png'),
  exampleFactorLabelsPlot,
  scale = 2,
  width = 6,
  height = 4,
  units = 'in'
)

```{r echo=FALSE}
knitr::asis_output(htmltools::htmlPreserve("
                                           <div>
                                           <div>block 2
                                           </div>
                                           </div>
                                           "))
```
# <hr>
