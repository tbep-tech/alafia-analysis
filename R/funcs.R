box_fun <- function(datin, param,  yrsel = 2021, ptsz = 0.5,
                         family = NA, txtlab = TRUE, log = F){

  # monthly averages
  aves <- datin %>%
    filter(name == !!param) %>% 
    dplyr::mutate(
      mo = month(mo, label = T)
    )
  
  yrs <- aves %>% 
    pull(yr) %>% 
    unique()
  
  yrrng <- c(2010, 2020)#yrs[!yrs %in% yrsel] %>% 
    # range(na.rm = T)
  
  # toplo1 is all but current year
  toplo1 <- aves %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(!yr %in% yrsel)
  
  # toplo2 is current year
  toplo2 <- aves %>%
    dplyr::filter(yr %in% yrsel)
  
  # colors and legend names
  cols <- c("black", "red")
  names(cols)[1] <- case_when(
    yrsel == yrrng[1] ~ paste(yrrng[1] + 1, yrrng[2], sep = '-'),
    yrsel == yrrng[2] ~ paste(yrrng[1], yrrng[2] - 1, sep = '-'),
    yrsel > yrrng[1] & yrsel < yrrng[2] ~ paste(paste(yrrng[1], yrsel - 1, sep = '-'), paste(yrsel + 1, yrrng[2], sep = '-'), sep = ', '),
    T ~ paste(yrrng, collapse = '-')
  )
  names(cols)[2] <- as.character(yrsel)

  p <- ggplot() +
    geom_boxplot(data = toplo1, aes(x = mo, y = value, colour = names(cols)[1]), outlier.colour = NA) +
    geom_point(data = toplo1, aes(x = mo, y = value, group = yr, colour = names(cols)[1]), position = position_jitter(width = 0.2), size = ptsz) +
    geom_point(data = toplo2, aes(x = mo, y = value, group = yr, fill = names(cols)[2]), pch = 21, color = cols[2], size = 3, alpha = 0.7) +
    theme(axis.title.x = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background = element_rect(fill = '#ECECEC'),
          legend.position = 'top',#c(0.85, 0.95),
          legend.background = element_rect(fill=NA),
          legend.key = element_rect(fill = '#ECECEC'),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 8, hjust = 1)
    ) +
    facet_wrap(~station, ncol = length(unique(datin$station))) +
    labs(y = unique(aves$lab)) +
    scale_colour_manual(values = cols[1]) +
    scale_fill_manual(values = cols[2]) +
    scale_linetype_manual(values = 'dotted') +
    guides(linetype = guide_legend(override.aes = list(colour = 'blue')))
  
  if(log)
    p <- p + scale_y_log10()
  
  return(p)
  
}