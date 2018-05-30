# SCRIPT: MAKING CUSTOM FUNCTIONS

mean_cent_fun <- function(df, 
                          by = c("capacity", "character"),
                          age_group = c("adults", "children")){
  
  if(age_group == "adults"){
    capacity_names = capacity_names_ad
    character_names = character_names_ad
  }
  if(age_group == "children"){
    capacity_names = capacity_names_ch
    character_names = character_names_ch
  }
  
  if(by == "capacity"){
    not_by <- "character"
  }
  
  if(by == "character"){
    not_by <- "capacity"
    # df <- df %>%
    #   gather(capacity, mean_rating, -character) %>%
    #   mutate(capacity = factor(capacity, levels = capacity_names)) %>%
    #   spread(character, mean_rating)
  }
  
  df_cent <- df %>%
    gather(capacity, mean_rating, -character) %>%
    group_by_(as.name(by)) %>%
    mutate(group_mean = mean(mean_rating),
           mean_cent = mean_rating - group_mean) %>%
    ungroup() %>%
    select(-mean_rating, -group_mean) %>%
    spread_(key_col = by,
            value_col = "mean_cent") %>%
    data.frame()
  
  return(df_cent)
}

U_fun <- function(svd,
                  age_group = c("adults", "children", "children_comb"),
                  transpose = c(FALSE, TRUE),
                  multiply = c(FALSE, TRUE)){
  
  if(transpose == FALSE) {
    if(age_group == "adults"){y_names <- character_names_ad}
    if(age_group == "children"){y_names <- character_names_ch}
    if(age_group == "children_comb"){
      y_names <- c(paste(character_names_ch, "79", sep = "_"),
                   paste(character_names_ch, "46", sep = "_"))
    }
  } else if(transpose == TRUE) {
    if(age_group == "adults"){y_names <- capacity_names_ad}
    if(age_group == "children"){y_names <- capacity_names_ch}
    if(age_group == "children_comb"){y_names <- capacity_names_ch}
  }  
  
  S <- svd$d
  
  U <- svd$u %>% 
    data.frame() %>%
    rownames_to_column("property") %>%
    mutate(property = as.numeric(property),
           property = factor(property, labels = y_names)) %>%
    gather(mode, value, -property) %>%
    mutate(mode = as.numeric(as.character(gsub("X", "", mode))),
           mode = ifelse(mode < 10, paste0("mode_0", as.character(mode)), 
                         paste0("mode_", as.character(mode)))) 
  
  if(multiply){
    UxS <- U %>%
      mutate(S = factor(mode, labels = S),
             S = as.numeric(as.character(S)),
             value = value * S)
    df_final <- UxS
  } else {df_final <- U}
  
  return(df_final)
  
}

plot_U_fun <- function(svd,
                       age_group = c("adults", "children", "children_comb"),
                       transpose = c(FALSE, TRUE),
                       multiply = c(FALSE, TRUE)){
  
  df_final <- U_fun(svd, age_group, transpose, multiply)
  
  plot <- ggplot(df_final, aes(x = mode, y = property, fill = value)) +
    geom_tile(color = "black") +
    # geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
    scale_fill_distiller(palette = "RdBu", direction = -1,
                         guide = guide_colorbar(barheight = 15)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  return(plot)
}

plot_U_fun_prop <- function(svd, 
                            age_group = c("adults", "children", "children_comb"),
                            transpose = FALSE,
                            multiply = FALSE,
                            efa_colors = FALSE,
                            modes = c("mode_01", "mode_02")){
  
  df_plot <- U_fun(svd, age_group, transpose, multiply) %>%
    filter(mode %in% modes)
  
  if(multiply == TRUE){
    df_plot <- df_plot %>% select(-S)
  }
  
  if(efa_colors == TRUE){
    if(age_group == "adults"){colors <- efa_colors_ad}
    if(age_group == "children"){colors <- efa_colors_79}
    if(age_group == "children_comb"){colors <- efa_colors_79}
    df_plot <- df_plot %>%
      full_join(colors %>% 
                  distinct(capacity, factor) %>% 
                  rename(property = capacity)) %>%
      mutate(factor = factor(factor))
  } else {
    df_plot <- df_plot %>%
      mutate(factor = "none",
             factor = factor(factor))
  }
  
  if(efa_colors == TRUE){
    plot <- df_plot %>%
      spread(mode, value) %>%
      ggplot(aes(x = .[3], y = .[4], label = property, color = factor)) +
      geom_point() +
      ggrepel::geom_text_repel(size = 3) +
      scale_x_continuous(breaks = seq(-100, 100, .1)) +
      scale_y_continuous(breaks = seq(-100, 100, .1)) +
      theme_minimal()
    
    if(age_group == "adults"){
      plot <- plot +
        scale_color_brewer(palette = "Set1")
    } else {
      plot <- plot + 
        scale_color_manual(values = c("#377eb8", "#4daf4a", "#e41a1c"))
    }
  } else {
    plot <- df_plot %>%
      spread(mode, value) %>%
      ggplot(aes(x = .[3], y = .[4], label = property, color = property)) +
      geom_point() +
      ggrepel::geom_text_repel(size = 3) +
      scale_x_continuous(breaks = seq(-100, 100, .1)) +
      scale_y_continuous(breaks = seq(-100, 100, .1)) +
      theme_minimal() +
      scale_color_manual(guide = "none",
                         values = rep("black", 
                                      length(levels(df_plot$property))))
  }
  
  return(plot)
  
}

plot_S_fun <- function(svd){
  
  S <- diag(x = svd$d, nrow = length(svd$d), ncol = length(svd$d)) %>%
    data.frame() %>%
    rownames_to_column("modeA") %>%
    gather(modeB, value, -modeA) %>%
    mutate(modeA = ifelse(as.numeric(modeA) < 10, 
                          paste0("mode_0", as.character(modeA)), 
                          paste0("mode_", as.character(modeA))),
           modeB = gsub("X", "", modeB),
           modeB = ifelse(as.numeric(modeB) < 10,
                          paste0("mode_0", as.character(modeB)),
                          paste0("mode_", as.character(modeB))))
  
  plot <- ggplot(S, aes(x = modeA, 
                        y = reorder(modeB, desc(modeB)), 
                        fill = value)) +
    geom_tile(color = "black") +
    # geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
    scale_fill_distiller(palette = "Reds", direction = 1,
                         guide = guide_colorbar(barheight = 15)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  return(plot)
}

V_fun <- function(svd,
                  age_group = c("adults", "children", "children_comb"),
                  transpose = c(FALSE, TRUE),
                  multiply = c(FALSE, TRUE)){
  
  if(transpose == FALSE) {
    if(age_group == "adults"){y_names <- capacity_names_ad}
    if(age_group == "children"){y_names <- capacity_names_ch}
    if(age_group == "children_comb"){y_names <- capacity_names_ch}
  } else if(transpose == TRUE) {
    if(age_group == "adults"){y_names <- character_names_ad}
    if(age_group == "children"){y_names <- character_names_ch}
    if(age_group == "children_comb"){
      y_names <- c(paste(character_names_ch, "79", sep = "_"),
                   paste(character_names_ch, "46", sep = "_"))
    }
  }
  
  S <- svd$d
  
  V <- svd$v %>% 
    data.frame() %>%
    rownames_to_column("item") %>%
    mutate(item = as.numeric(item),
           item = factor(item, labels = y_names)) %>%
    gather(mode, value, -item) %>%
    mutate(mode = as.numeric(as.character(gsub("X", "", mode))),
           mode = ifelse(mode < 10, paste0("mode_0", as.character(mode)), 
                         paste0("mode_", as.character(mode)))) 
  
  if(multiply){
    VxS <- V %>%
      mutate(S = factor(mode, labels = S),
             S = as.numeric(as.character(S)),
             value = value * S)
    df_final <- VxS
  } else {df_final <- V}
  
  return(df_final)
}

plot_V_fun <- function(svd,
                       age_group = c("adults", "children", "children_comb"),
                       transpose = c(FALSE, TRUE),
                       multiply = c(FALSE, TRUE)){
  
  df_final <- V_fun(svd, age_group, transpose, multiply)
  
  plot <- ggplot(df_final, aes(x = mode, y = item, fill = value)) +
    geom_tile(color = "black") +
    # geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
    scale_fill_distiller(palette = "RdBu", direction = -1,
                         guide = guide_colorbar(barheight = 15)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  return(plot)
}

plot_V_fun_item <- function(svd, 
                            age_group = c("adults", "children", "children_comb"),
                            transpose = FALSE,
                            multiply = FALSE,
                            efa_colors = FALSE,
                            modes = c("mode_01", "mode_02")){
  
  df_plot <- V_fun(svd, age_group, transpose, multiply) %>%
    filter(mode %in% modes)
  
  if(multiply == TRUE){
    df_plot <- df_plot %>% select(-S)
  }
  
  if(efa_colors == TRUE){
    if(age_group == "adults"){colors <- efa_colors_ad}
    if(age_group == "children"){colors <- efa_colors_79}
    if(age_group == "children_comb"){colors <- efa_colors_79}
    df_plot <- df_plot %>%
      full_join(colors %>% 
                  distinct(capacity, factor) %>% 
                  rename(item = capacity)) %>%
      mutate(factor = factor(factor))
  } else {
    df_plot <- df_plot %>%
      mutate(factor = "none",
             factor = factor(factor))
  }
  
  if(efa_colors == TRUE){
    
    plot <- df_plot %>%
      spread(mode, value) %>%
      ggplot(aes(x = .[3], y = .[4], label = item, color = factor)) +
      geom_point() +
      ggrepel::geom_text_repel(size = 3) +
      scale_x_continuous(breaks = seq(-100, 100, .1)) +
      scale_y_continuous(breaks = seq(-100, 100, .1)) +
      theme_minimal()
    
    if(age_group == "adults"){
      plot <- plot + 
        scale_color_brewer(palette = "Set1")
    } else {
      plot <- plot + 
        scale_color_manual(values = c("#377eb8", "#4daf4a", "#e41a1c"))
    }
  } else {
    plot <- df_plot %>%
      spread(mode, value) %>%
      ggplot(aes(x = .[3], y = .[4], label = item, color = item)) +
      geom_point() +
      ggrepel::geom_text_repel(size = 3) +
      scale_x_continuous(breaks = seq(-100, 100, .1)) +
      scale_y_continuous(breaks = seq(-100, 100, .1)) +
      theme_minimal() +
      scale_color_manual(guide = "none",
                         values = rep("black", length(levels(df_plot$item))))
  }
  
  return(plot)
  
}

reconstruct_fun <- function(svd,
                            age_group = c("adults", "children", "children_comb"),
                            transpose = c(FALSE, TRUE)){
  
  if(age_group == "adults"){
    capacity_names <- capacity_names_ad
    character_names <- character_names_ad
  }
  if(age_group == "children"){
    capacity_names <- capacity_names_ch
    character_names <- character_names_ch
  }
  if(age_group == "children_comb"){
    capacity_names <- capacity_names_ch
    character_names <- c(paste(character_names_ch, "79", sep = "_"),
                         paste(character_names_ch, "46", sep = "_"))
  }
  
  if(transpose == FALSE){
    x_names <- capacity_names
    y_names <- character_names
  }
  
  if(transpose == TRUE){
    x_names <- character_names
    y_names <- capacity_names
  }
  
  # calculate outer products, multiply by singular value
  OP <- list()
  for(i in 1:length(svd$d)){
    OP[[i]] <- outer(svd$u[,i], svd$v[,i]) * svd$d[i]
  }
  
  # add together
  OP_sum <- Reduce("+", OP) %>%
    data.frame() %>%
    rownames_to_column("y") %>%
    mutate(y = factor(as.numeric(y), labels = y_names)) %>%
    gather(x, value, -y) %>%
    mutate(x = gsub("X", "", x),
           x = factor(as.numeric(x), labels = x_names))
  
  return(OP_sum)
}

plot_reconstruct_fun <- function(svd,
                                 age_group = c("adults", 
                                               "children", "children_comb"),
                                 transpose = c(FALSE, TRUE)){
  
  OP_sum <- reconstruct_fun(svd, age_group, transpose)
  
  plot <- ggplot(OP_sum, aes(x = x, y = y, fill = value)) +
    geom_tile(color = "black") +
    # geom_text(aes(label = format(round(value, 2), nsmall = 2)), size = 3) +
    scale_fill_distiller(palette = "RdBu", direction = -1,
                         guide = guide_colorbar(barheight = 15)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  return(plot)
}