# p_i is the proportional abundance of the ith species, so 

countFrame <-
  data_frame(
    site = c(1,2,3,1,2),
    spp = c(rep('amre', 3), 'btbw', 'btbw'),
    count = c(2,3,1,1,1)
  )

# Diversity measures:

countFrame %>%
  spread(spp, count, fill = 0)
  


d(abundances, lev = "alpha", wts = FALSE, q = 1, boot = FALSE)

# Calculate proportional abundance for each species:

countFrame_p <-
  countFrame %>%
  group_by(spp) %>%
  summarize(
    abundance = sum(count)) %>%
  mutate(
    p = abundance/sum(abundance))

# Function to calculate the "basic sum":

basicSum <-
  function(pVector, q){
   sum(pVector^q)
  }

# This is the basis of diversity indices, for example Species richness:

basicSum(countFrame_p$p, 0)

# ... and Simpson's index:

basicSum(countFrame_p$p, 2)

countFrame %>%
  group_by(spp) %>%
  summarize(
    abundance = sum(count)) %>%
  .$abundance %>%
  vegan::diversity(index = 'simpson')

vegan::diversity(countFrame_p$p, index = 'simpson')

# Simpson's Index (D) measures the probability that two individuals randomly selected from a sample will belong to the same group (e.g. species). There are two versions of the formula for calculating D. Either is acceptable, but be consistent.

# (sum(n * (n-1)))/N*(N-1)

totalCount <- 
  sum(countFrame$count)

sum((countFrame$count * (countFrame$count - 1))/(totalCount * (totalCount - 1)))




