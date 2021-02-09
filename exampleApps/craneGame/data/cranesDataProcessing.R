# Crane picture shuffling 

# Make a vector of image paths

pic <- c("craneMap.png",
         "Nest.jpg",
         "orangeFlock.jpg",
         "PlatteMorning.jpg",
         "PlatteRiverFlock.jpg",
         "squawk.jpg",
         "tallCrane.jpg",
         "threeFlock.jpg",
         "twoFlock.jpg")

# make a vector of fun facts

fact = c("80% of the world's sandhill cranes congregate on Nebraska's Platte River during migration",
         "Sandhill cranes almost always raise two young, called, adorably, colts, per year",
         "Sandhill cranes mate for life, up to 20 years",
         "Omnivorous sandhill cranes use their long bills to probe wetlands, prairies, and wet meadows for food",
         "Sandhill cranes have one of the largest winspans in North America: up to 1.5 meters!",
         "Sandhill cranes perform elaborate dances during the mating season. These graceful duets include wing flapping, bowing, prancing, and tossing vegetation in the air.",
         "The oldest sandhill crane fossil yet discovered, excavated from Macasphalt Shell Pit in Florida, is over 2.5 million years old. ",
         "Sandhill cranes are legal to hunt in every state on their midcontinent flyway except Nebraska",
         "Sandhill cranes are a conservation success story, increasing in population size since 1966.")

# make a vector of alt text for accessibility

alt = c(paste0("Image description: Sandhill Crane (Antigone canadensis).",
               "Relative abundance is depicted for each season along",
               "a color gradient from a light color indicating lower",
               "relative abundance to a dark color indicating a higher",
               "relative abundance. Relative abundance is the estimated ",
               "number of individuals detected by an eBirder during a ",
               "traveling count at the optimal time of day."),
        paste0("Image description: A sandhill crane nest with two large cream-colored ",
               "eggs in a large round nest built of sticks floating ",
               "on a wetland."),
        paste0("Image description: Seven sandhill cranes stretching their legs",
                "and wings out to land against an orange sunset sky"),
        paste0("Image description: A large flock of sandhill cranes standing in and flying above ",
               "the Platte River, a wide shallow river bordered by grass and ",
               "cottonwood trees in an orange dawn."),
        paste0("Image description: A large flock of sandhill cranes standing in and flying above ",
               "the Platte River, a wide shallow river bordered by grass and ",
               "cottonwood trees."),
        paste0("Image description: A sandhill crane, a tall bird with fluffy grey plummage ",
               "and a red forehead skimming across green grass with wings ",
               "oustretched and mouth open making their trademark trumpeting call."),
               "A sandhill crane standing at its full, considerable, height",
               "A flock of three sandhill cranes in flight",
               "A flock of two sandhill cranes in flight")

# Combine all three vectors into a dataframe        

cranePics <- data.frame(cbind(pic, fact, alt))

# write to file

write.csv(cranePics, file = 'C:/Users/Lyndsie/Documents/GitHub/shiny_workshop/exampleApps/craneGame/data/cranePics.csv')

# read in BBS routes

routes <- read.csv('C:/Users/Lyndsie/Documents/GitHub/shiny_workshop/exampleApps/craneGame/data/routes.csv')

# read in crane count data

counts <- read.csv('C:/Users/Lyndsie/Documents/GitHub/shiny_workshop/exampleApps/craneGame/data/counts.csv')

# join routes to get coordinates for maps

count_routes <- counts %>%
                left_join(routes, by = c("Country", "State", "Route"))

# write joined data to file

write.csv(count_routes, file = "C:/Users/Lyndsie/Documents/GitHub/shiny_workshop/exampleApps/craneGame/data/countRoutes.csv")
