plot_snotel <-
  function(x){
    
    ribbons <-
      list(`Min–5th` = c("1%", "5%"),
           `5th–25th` = c("5%", "25%"),
           `25th–75th` = c("25%", "75%"),
           `75th–95th` = c("75%", "95%"),
           `95th–Max` = c("95%", "99%")
      ) %>%
      purrr::imap_dfr(
        ~(
          x$past %>%
            dplyr::transmute(date,
                             ribbon = .y,
                             ymin = .[[.x[[1]]]],
                             ymax = .[[.x[[2]]]])
        )
      ) %>%
      dplyr::mutate(ribbon = factor(ribbon, 
                                    levels = rev(
                                      c("Min–5th",
                                        "5th–25th",
                                        "25th–75th",
                                        "75th–95th",
                                        "95th–Max")
                                    ),
                                    ordered = TRUE
      ))
    
    ggplot() +
      ggtitle(x$station) +
      
      ## PAST CONDITIONS
      geom_ribbon(data = 
                    ribbons,
                  mapping = aes(x = date, ymin = ymin, ymax = ymax, fill = ribbon),
                  alpha = 0.25) +
      geom_line(data =
                  x$past %>%
                  dplyr::select(date,
                                Minimum = `1%`,
                                Median = `50%`,
                                Maximum = `99%`) %>%
                  tidyr::pivot_longer(-date),
                mapping = aes(x = date, y = value, 
                              color = name)) +
      scale_fill_manual(name = paste0('Percentiles'), 
                        values = c(`Min–5th` = 'darkred',
                                   `5th–25th` = "orange",
                                   `25th–75th` = "green",
                                   `75th–95th` = "cyan",
                                   `95th–Max` = "darkblue"),
                        guide = guide_legend(order = 1)) +
      scale_color_manual(name = NULL,
                         values = c(
                           'Minimum' = 'darkred',
                           'Median' = 'forestgreen',
                           'Maximum' = 'darkblue'),
                         guide = guide_legend(order = 2)) +
      scale_y_continuous(limits = c(0, NA),
                         expand = expansion(c(0,0.05),0),
                         name = "Snow Water Equivalent (mm)") +
      scale_x_date(date_labels = "%B %Y",
                   expand = expansion(0,0),
                   name = NULL) +
      
      ## PRESENT CONDITIONS
      geom_line(data =
                  x$present,
                mapping = aes(x = date, 
                              y = swe,
                              
                              linewidth = "Observed"),
                color = "black",
                lineend = "round") +
      scale_linewidth_manual(name = "Present Conditions",
                             values = 1.5,
                             guide = guide_legend(order = 3)) +
      
      ## FUTURE CONDITIONS
      ggnewscale::new_scale_fill() +
      ggnewscale::new_scale_color() +
      geom_ribbon(data =
                    x$future,
                  mapping = aes(x = date,
                                ymin = `5%`,
                                ymax = `95%`,
                                fill = "Likely Range"),
                  alpha = 0.2) +
      geom_line(data = x$future,
                mapping = aes(x = date, y = `50%`, 
                              color = "Most Likely",
                              linetype = "Most Likely")) +
      
      scale_fill_manual(name = "Future Conditions",
                        values = "black",
                        guide = guide_legend(order = 4)) +
      
      scale_color_manual(name = NULL,
                         values = "black",
                         guide = guide_legend(order = 5)) +
      scale_linetype_manual(name = NULL,
                            values = 2,
                            guide = guide_legend(order = 5)) +
      theme_bw() +
      theme(legend.margin = margin(),
            plot.title = element_text(face = "bold",
                                      hjust = 0.5),
            legend.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold")
      )
  }