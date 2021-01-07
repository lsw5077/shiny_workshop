library(rfishbase)
library(tidyverse)

# This is the data cleaning file for the sofishticated app
# Last updated 1/6/2021 by Lyndsie Wszola

# Make a list of fish we want growth trajectories for
# Using their common and scientific names

favList <- data.frame(Species = c("Sander vitreus",
                                  "Salvelinus namaycush",
                                  "Perca flavescens",
                                  "Esox lucius",
                                  "Esox masquinongy",
                                  "Lota lota",
                                  "Acipenser fulvescens"),
                      Common_Name = c("Walleye",
                                      "Lake trout",
                                      "Yellow perch",
                                      "Northern pike",
                                      "Muskellunge",
                                      "Burbot",
                                      "Lake sturgeon"))

# Make a function using the so-called von Bertalanffy growth function
# This parameterization actually comes from Beverton and Holt 1959. 

growBH <- function(lAsym, # Asymptotic length, hypothetical maximum
                   t, # Fish's age in years
                   k, # Brody Growth coefficient
                   t0) { # hypothetical age at which fish = 0
  
          # Fish grow as an inteterminate function of age and 
          # life history parameters
          
          lt = lAsym*(1-exp(-k*(t-t0)))
          
          # return age-specific length
          
          return(lt)
    
          }

# Using fish base, join life history parameters onto our
# list of favorite fish species

favFish <- favList %>%
           left_join(popgrowth(), by = "Species") %>%
           group_by(Species) %>%
           mutate(lAsym = mean(TLinfinity, na.rm = TRUE),
                  k = mean(K, na.rm = TRUE),
                  t0 = mean(to, na.rm = TRUE),
                  tmax = round(mean(tmax, na.rm = TRUE))) %>%
           select(Species, Common_Name, lAsym, k, t0, tmax) %>% 
           distinct()


# Calculate growth across years for each species.

fishDat <- favFish[rep(seq(nrow(favFish)), 1+as.numeric(favFish$tmax)),] %>%
           group_by(Species) %>%
           mutate(Age = row_number()-1) %>%
           rowwise() %>%
           mutate(Length = growBH(lAsym = lAsym,
                                  k = k,
                                  t0 = t0,
                                  t = Age))



#write.csv(fishDat, file = "C:/Users/Lyndsie/Documents/GitHub/shiny_workshop/exampleApps/sofishticatedApp/data/fishdat.csv")

write_rds(fishDat, path = "fishdat.rdata")

# For good app:

  # add comments
  # add functions and vectors
  # Add mouseover label
  # fix axes/scales

