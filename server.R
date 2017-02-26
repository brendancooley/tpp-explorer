library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(ggvis)

tradeA <- read_csv('tradeA.csv')
tradeA$ID <- as.character(seq(1, length(tradeA$ccode)))
tradeA$tau_imput <- as.factor(tradeA$tau_imput)

# cutpoints
inv_imp_pen_sd <- sd(tradeA$inv_imp_pen)
inv_imp_pen_cut <- mean(tradeA$inv_imp_pen)
tradeA <- filter(tradeA, inv_imp_pen < inv_imp_pen_cut)

# Country mapping
C <- read_csv('IDE_ISIC.csv')
c <- C %>% group_by(ccode, name) %>%
  summarise(n = n())

shinyServer(function(input, output) {
  
  trade_dat <- reactive({
    t <- tradeA
    if (input$imp_comp == 'yes') {
      t <- filter(tradeA, net_imp_tv > 0) 
    }
    if (input$tar_type == 'MFN') {
      if (input$ntb == 'yes') {
        t$tauhat <- (t$tar_iwmfn + t$ave_core_wgt - 2) / (t$tar_iwmfn + t$ave_core_wgt - 1)
      }
      else {
        t$tauhat <- (t$tar_iwmfn - 1) / t$tar_iwmfn
      }
    }
    if (input$tar_type == 'Applied') {
      if (input$ntb == 'yes') {
        t$tauhat <- (t$tar_iwahs + t$ave_core_wgt - 2) / (t$tar_iwahs + t$ave_core_wgt - 1)
      }
      else {
        t$tauhat <- (t$tar_iwahs - 1) / t$tar_iwahs
      }
    }
    t$indicator <- as.factor(ifelse(t$ccode == input$country, 1, 0))
    t$imput_col <- ifelse(t$tau_imput == 0, 'darkorange', 'red')
    t <- as.data.frame(t)
    t
  })
  
  country <- reactive ({
    
  })
  
  trade_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    
    ttemp <- isolate(trade_dat())
    obs <- ttemp[ttemp$ID == x$ID, ]
    
    paste0("<b>", obs$ccode, "</b>", ", ", obs$year, "<br>",
           obs$isicnames, "<br>",
           paste('alpha = ', round(obs$alpha,3), sep = "")
    )
  }
  
  tradePlotA <- reactive({
    tradeC <- filter(trade_dat, indicator == 1)
    tradeNC <- filter(trade_dat, indicator == 0)
    trade_dat %>% ggvis(x = ~inv_imp_pen_elast, y = ~tauhat) %>%
      layer_points(size = ~alpha, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke := 'steelblue',
                   key := ~ID, data = tradeNC) %>%
      layer_points(size = ~alpha, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke := 'darkorange',
                   key := ~ID, data = tradeC) %>%
      add_tooltip(trade_tooltip, "hover") %>%
      add_axis("x", title = 'Inverse Import Penetration * Inverse Elasticity') %>%
      add_axis("y", title = 'Magnitude of Tariff and Nontariff Barriers')
    })

  output$plot1head <- renderText({'All Observations'})
  tradePlotA %>% bind_shiny('all')
  
  tradePlotC <- reactive({
    tradec <- filter(trade_dat, indicator == 1)
    tradec %>% ggvis(x = ~inv_imp_pen_elast, y = ~tauhat) %>%
      layer_points(size = ~alpha, size.hover := 500,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke := ~imput_col,
                   key := ~ID) %>%
      layer_model_predictions(model = 'lm', formula = tauhat ~ inv_imp_pen - 1) %>%
      add_tooltip(trade_tooltip, "hover") %>%
      add_axis("x", title = 'Inverse Import Penetration * Inverse Elasticity') %>%
      add_axis("y", title = 'Magnitude of Tariff and Nontariff Barriers') %>%
      hide_legend('stroke')
  })
  
  country <- reactive({
    n <- filter(c, ccode == input$country)
    n$name
  })
  
  output$plot2head <- renderText({country()})
  tradePlotC %>% bind_shiny('n')
  
})

## next steps:
  # 1) allow for elasticity adjustment
  # 2) look at different combinations of tariff types