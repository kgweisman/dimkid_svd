# SCRIPT: IMPORTING DATA

setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid_svd/dimkid_svd/")

# import data
d_ad <- read.csv("./data/means_adults.csv") %>% 
  mutate(X = ifelse(grepl("vegetative", X), "pvs", as.character(X)),
         X = factor(X,
                    levels = c("stapler", "car", "computer", "robot", "microbe",
                               "fish", "beetle", "blue_jay", "frog", "mouse",
                               "goat", "dog", "bear", "dolphin", "elephant",
                               "chimpanzee", "fetus", "pvs", "infant", "child",
                               "adult"))) %>%
  # levels = c("adult", "child", "infant", "pvs", "fetus",
  #            "chimpanzee", "elephant", "dolphin", "bear",
  #            "dog", "goat", "mouse", "frog", "blue_jay",
  #            "beetle", "fish", "microbe", "robot", "computer",
  #            "car", "stapler"))) %>%
  rename(character = X) %>%
  arrange(character)

d_79 <- read.csv("./data/means_79y.csv") %>% 
  mutate(X = factor(X,
                    levels = c("computer", "robot", "doll", "teddy_bear",
                               "beetle", "bird", "mouse", "goat", 
                               "elephant"))) %>%
  # levels = c("elephant", "goat", "mouse", "bird", "beetle",
  #            "teddy_bear", "doll", "robot", "computer"))) %>%
  rename(character = X) %>%
  arrange(character)

d_46 <- read.csv("./data/means_46y.csv") %>%
  mutate(X = factor(X,
                    levels = c("computer", "robot", "doll", "teddy_bear",
                               "beetle", "bird", "mouse", "goat", 
                               "elephant"))) %>%
  # levels = c("elephant", "goat", "mouse", "bird", "beetle",
  #            "teddy_bear", "doll", "robot", "computer"))) %>%
  rename(character = X) %>%
  arrange(character)

# set item and property names
capacity_names_ad <- names(d_ad[-1])
character_names_ad <- levels(d_ad$character)

capacity_names_ch <- names(d_79[-1])
character_names_ch <- levels(d_79$character)