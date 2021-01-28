library(tidyverse)
library(ratdat)

ratWeights <- ratdat::surveys %>% 
              left_join(ratdat::species, by = "species_id") %>%
              left_join(ratdat::plots, by = "plot_id") %>%
              filter(!is.na(weight), genus != "Spermophilus") %>%
              mutate(Date = as.Date(paste(year, month, day, sep = "-")),
                     Name = paste(genus, species),
                     genus = droplevels(factor(genus)))

write_rds(ratWeights, file = "exampleApps/rats/data/ratWeights.rds")
genus <- unique(ratWeights$genus)
write_rds(genus, file = "exampleApps/rats/data/genus.rds")


ggplot(ratWeights) +
  geom_point(aes(x = hindfoot_length, y = weight,
                 group = genus, color = genus), size = 5, alpha = 0.5) +
  scale_color_viridis(discrete = TRUE) +
  theme_bw()




# Nested list: species inside taxa
## Bird Rabbit Reptile Rodent

ratWeights %>% 
  group_by(Date, plot_id, Name) %>%
  mutate(count = n()) %>%
  select(Date, plot_id, plot_type, Name, genus, count) %>%
  distinct() %>%
ggplot() +
  geom_line(aes(x = Date, 
                 y = count,
                 group = genus,
                 color = genus)) +
  scale_color_viridis(discrete = TRUE) +
  theme_bw() +
  facet_wrap(~plot_type)
