

## Find out which trait is associated with more sites/more detection/more ...

library(traitdata)
data("elton_birds")

data <- elton_birds %>%
  as_tibble() %>%
  filter(English == "White-throated Sparrow")
