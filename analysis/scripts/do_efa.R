# SCRIPT: FACTOR ANALYSIS

efa_loadings_ad <- fa(d_ad %>% column_to_rownames("character"),
                      nfactors = 3, rotate = "varimax")$loadings[] %>%
  fa.sort() %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  rownames_to_column("order") %>%
  mutate(order = as.numeric(order)) %>%
  gather(factor, loading, -c(capacity, order))

efa_colors_ad <- efa_loadings_ad %>%
  group_by(capacity) %>%
  top_n(1, abs(loading))

# ggplot(efa_loadings_ad,
#        aes(x = factor, y = reorder(capacity, desc(order)),
#            fill = loading, label = format(round(loading, 2), digits = 2))) +
#   geom_tile(color = "black") +
#   geom_text(size = 3) +
#   annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 20.5, ymax = 40.5, 
#            alpha = 0, color = "black") +
#   annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 9.5, ymax = 20.5, 
#            alpha = 0, color = "black") +
#   annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 0.5, ymax = 9.5, 
#            alpha = 0, color = "black") +
#   scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1),
#                        guide = guide_colorbar(barheight = 15)) +
#   theme_minimal() +
#   labs(title = "EFA: Adults",
#        x = "Factor", y = "Capacity", fill = "Loading")

efa_loadings_79 <- fa(d_79 %>% column_to_rownames("character"),
                      nfactors = 3, rotate = "varimax")$loadings[] %>%
  fa.sort() %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  rownames_to_column("order") %>%
  mutate(order = as.numeric(order)) %>%
  gather(factor, loading, -c(capacity, order))

efa_colors_79 <- efa_loadings_79 %>%
  group_by(capacity) %>%
  top_n(1, abs(loading))

# ggplot(efa_loadings_79,
#        aes(x = factor, y = reorder(capacity, desc(order)),
#            fill = loading, label = format(round(loading, 2), digits = 2))) +
#   geom_tile(color = "black") +
#   geom_text(size = 3) +
#   annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 14.5, ymax = 20.5, 
#            alpha = 0, color = "black") +
#   annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 6.5, ymax = 14.5, 
#            alpha = 0, color = "black") +
#   annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0.5, ymax = 6.5, 
#            alpha = 0, color = "black") +
#   scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1),
#                        guide = guide_colorbar(barheight = 15)) +
#   theme_minimal() +
#   labs(title = "EFA: Children, 7-9y",
#        x = "Factor", y = "Capacity", fill = "Loading")

efa_loadings_46 <- fa(d_46 %>% column_to_rownames("character"),
                      nfactors = 3, rotate = "varimax")$loadings[] %>%
  fa.sort() %>%
  data.frame() %>%
  rownames_to_column("capacity") %>%
  rownames_to_column("order") %>%
  mutate(order = as.numeric(order)) %>%
  gather(factor, loading, -c(capacity, order))

efa_colors_46 <- efa_loadings_46 %>%
  group_by(capacity) %>%
  top_n(1, abs(loading))

# ggplot(efa_loadings_46,
#        aes(x = factor, y = reorder(capacity, desc(order)),
#            fill = loading, label = format(round(loading, 2), digits = 2))) +
#   geom_tile(color = "black") +
#   geom_text(size = 3) +
#   annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 8.5, ymax = 20.5, 
#            alpha = 0, color = "black") +
#   annotate("rect", xmin = 2.5, xmax = 3.5, ymin = 3.5, ymax = 8.5, 
#            alpha = 0, color = "black") +
#   annotate("rect", xmin = 1.5, xmax = 2.5, ymin = 0.5, ymax = 3.5, 
#            alpha = 0, color = "black") +
#   scale_fill_distiller(palette = "RdYlBu", limits = c(-1, 1),
#                        guide = guide_colorbar(barheight = 15)) +
#   theme_minimal() +
#   labs(title = "EFA: Children, 4-6y",
#        x = "Factor", y = "Capacity", fill = "Loading")
