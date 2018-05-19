# make adult data: means by character & capacity

d_adult_mean <- read.csv("https://osf.io/kdzge/download") %>%
  select(subid, condition, happy:pride) %>%
  gather(capacity, response, -subid, -condition) %>%
  group_by(condition, capacity) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  ungroup() %>%
  group_by(capacity) %>%
  mutate(cond_mean = mean(mean),
         mean_cent = mean - cond_mean) %>%
  ungroup() %>%
  select(condition, capacity, mean_cent) %>%
  spread(capacity, mean_cent) %>%
  remove_rownames() %>%
  data.frame() %>%
  mutate_all(funs(gsub(" ", "_", .))) %>%
  column_to_rownames("condition")

write.csv(d_adult_mean, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid_svd/dimkid_svd/data/means_adults.csv")

# make 7-9yo data: means by character & capacity
# first run cogsci2018_rev.Rmd

d_79_mean_t <- d_old %>%
  group_by(character, capacity) %>%
  summarise(mean = mean(responseNum)) %>%
  ungroup() %>%
  group_by(capacity) %>%
  mutate(char_mean = mean(mean),
         mean_cent = mean - char_mean) %>%
  ungroup() %>%
  select(character, capacity, mean_cent) %>%
  spread(capacity, mean_cent) %>%
  remove_rownames() %>%
  data.frame() %>%
  column_to_rownames("character")

# make 4-6yo data: means by character & capacity
# first run cogsci2018_rev.Rmd

d_46_mean_t <- d_young %>%
  group_by(character, capacity) %>%
  summarise(mean = mean(responseNum)) %>%
  ungroup() %>%
  group_by(capacity) %>%
  mutate(char_mean = mean(mean),
         mean_cent = mean - char_mean) %>%
  ungroup() %>%
  select(character, capacity, mean_cent) %>%
  spread(capacity, mean_cent) %>%
  remove_rownames() %>%
  data.frame() %>%
  column_to_rownames("character")

write.csv(d_46_mean_t, "/Users/kweisman/Documents/Research (Stanford)/Projects/Dimkid_svd/dimkid_svd/data/means_46y.csv")
