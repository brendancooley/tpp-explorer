# cutpoints
inv_imp_pen_sd <- sd(tradeA$inv_imp_pen)
inv_imp_pen_cut <- mean(tradeA$inv_imp_pen)
tradeA <- filter(tradeA, inv_imp_pen < inv_imp_pen_cut)

trade_tooltip <- function(x) {
  if (is.null(x)) return(NULL)

  obs <- tradeA[tradeA$ID == x$ID, ]
  
  paste0("<b>", obs$name, "</b><br>",
         obs$isicnames, "<br>"
  )
}

test <- 'USA'
tradeA$indicator <- as.factor(ifelse(tradeA$ccode == test, 1, 0))
tradeA$ID <- as.character(seq(1, length(tradeA$ccode)))
vis <- tradeA %>% ggvis(x = ~inv_imp_pen, y = ~tauhat) %>%
    layer_points(size := 50, size.hover := 200,
                 fillOpacity := 0.2, fillOpacity.hover := 0.5,
                 stroke = ~indicator, key := ~ID) %>%
    add_tooltip(trade_tooltip, "hover") %>%
    add_axis("x", title = 'Inverse Import Penetration') %>%
    add_axis("y", title = 'Magnitude of Tariff and Nontariff Barriers')


tradeC <- filter(tradeA, indicator == 1)
test <- tradeC %>% ggvis(x = ~inv_imp_pen, y = ~tauhat) %>%
  layer_points(size = ~alpha, size.hover := 200,
               fillOpacity := 0.2, fillOpacity.hover := 0.5,
               stroke := 'darkorange',
               key := ~ID, data = tradeC) %>%
  add_tooltip(trade_tooltip, "hover") %>%
  add_axis("x", title = 'Inverse Import Penetration') %>%
  add_axis("y", title = 'Magnitude of Tariff and Nontariff Barriers')
