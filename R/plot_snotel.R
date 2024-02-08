plot_snotel <-
  function(x, usdm = FALSE){
    
    if(usdm){
      ribbons <-
        list(
          `D4` = c("Minimum", "2%"),
          `D3` = c("2%", "5%"),
          `D2` = c("5%", "10%"),
          `D1` = c("10%", "20%"),
          `D0` = c("20%", "30%"),
          # Neutral = c("30%", "70%"),
          `W0` = c("70%", "80%"),
          `W1` = c("80%", "90%"),
          `W2` = c("90%", "95%"),
          `W3` = c("95%", "98%"),
          `W4` = c("98%", "Maximum")
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
                                        c("D4",
                                          "D3",
                                          "D2",
                                          "D1",
                                          "D0",
                                          # "Neutral",
                                          "W0",
                                          "W1",
                                          "W2",
                                          "W3",
                                          "W4")
                                      ),
                                      ordered = TRUE
        ))
      
      ribbon_colors <-
        c(
          `D4` = "#730000",
          `D3` = "#E60000",
          `D2` = "#FFAA00",
          `D1` = "#FCD37F",
          `D0` = "#FFFF00",
          # Neutral = "white",
          `W0` = "#9DFF44",
          `W1` = "#22FFFF",
          `W2` = "#1197FE",
          `W3` = "#1100FF",
          `W4` = "#0A0099"
        )
      
      ribbon_columns <- 2
    }else{
      ribbons <-
        list(`Min–5th` = c("Minimum", "5%"),
             `5th–25th` = c("5%", "25%"),
             `25th–75th` = c("25%", "75%"),
             `75th–95th` = c("75%", "95%"),
             `95th–Max` = c("95%", "Maximum")
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
      
      ribbon_colors <-
        c(
          `Min–5th` = "#730000",
          `5th–25th` = "orange",
          `25th–75th` = "green",
          `75th–95th` = "cyan",
          `95th–Max` = "#0A0099"
        )
      
      ribbon_columns <- 1
    }
    
    
    
    ggplot() +
      ggtitle(x$station) +
      
      ## PAST CONDITIONS
      geom_ribbon(data = 
                    ribbons,
                  mapping = aes(x = date, ymin = ymin, ymax = ymax, fill = ribbon),
                  alpha = 0.5) +
      geom_line(data =
                  x$past %>%
                  dplyr::select(date,
                                Minimum = `Minimum`,
                                Median = `50%`,
                                Maximum = `Maximum`) %>%
                  tidyr::pivot_longer(-date),
                mapping = aes(x = date, y = value, 
                              color = name)) +
      scale_fill_manual(name = paste0('Past Conditions\n', 
                                      x$past_years[[1]],"–",
                                      x$past_years[[2]]), 
                        values = ribbon_colors,
                        guide = guide_legend(
                          order = 1, 
                          ncol = ribbon_columns
                        )) +
      scale_color_manual(name = NULL,
                         values = c(
                           'Minimum' = first(ribbon_colors),
                           'Median' = 'black',
                           'Maximum' = last(ribbon_colors)),
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
      scale_linewidth_manual(name = paste0("Present Conditions"),
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
