library(shiny)
library(bslib)

options(shiny.autoreload.legacy_warning = FALSE)
options(shiny.fullstacktrace = FALSE)

ui <- fluidPage(
  titlePanel("Climate-RL simulation"),
  sidebarLayout(

    # INPUTS
    sidebarPanel(
      width = 3, # out of a total 12 units
      style = "height: 89vh; overflow-y: auto;", # sidebar scroll

      # EXPERIMENT SETTINGS
      tags$h3("Experiment settings"),
      numericInput("n_part", "Number of participants:", min = 1, value = 50),
      numericInput("n_trials", "Number of trials:", min = 1, value = 30),
      hr(),

      # SIMULATION TYPE
      tags$h3("Simulation type"),
      radioButtons("conf_bias", "Confirmation bias",
                   c("None (standard model)" = "none",
                    "Learning level (LRN)" = "LRN")
      ),
      conditionalPanel(condition = "input.conf_bias == 'LRN'",
        radioButtons("d_or_c", "Bias is:",
                     c("Discrete" = "discr", "Continuous" = "cont")),
        conditionalPanel(condition = "input.d_or_c == 'discr'",
          radioButtons("confirmatory", "An outcome is confirmatory when:",
                       c("R ≈ belief" = "approx", "R ≥ belief" = "geq"))
        ),
        radioButtons("belief_type", "Beliefs are:",
                     c("Static" = "stat", "Dynamic" = "dyn"))
      ),
      hr(),
      
      # PARAMETER SETTINGS
      tags$h3("Parameter settings (group-level means)"),
      # learning rate
      conditionalPanel(condition = "input.conf_bias == 'none'",
        sliderInput("LR", "Learning rate:", min = 0, max = 1,
                    value = 0.4, step = 0.1, ticks = FALSE)
      ),
      conditionalPanel(condition = "input.conf_bias == 'LRN' &&
                                    input.d_or_c == 'discr'",
        sliderInput("LR_conf", "Confirmatory learning rate:", min = 0, max = 1,
                    value = 0.8, step = 0.1, ticks = FALSE),
        sliderInput("LR_disconf", "Disconfirmatory learning rate:", min = 0, max = 1,
                    value = 0.2, step = 0.1, ticks = FALSE)
      ),
      conditionalPanel(condition = "input.conf_bias == 'LRN' &&
                                    input.d_or_c == 'cont'",
        sliderInput("w_LR", "Weight for learning rate", min = 0, max = 1,
                    value = 0.4, step = 0.1, ticks = FALSE)
      ),
      
      sliderInput("inv_temp", "Inverse temperature:", min = 0, max = 2,
                  value = 0.5, step = 0.1, ticks = FALSE),
      sliderInput("initQF", "Initial Q friendly:", min = 1, max = 10, 
                  value = 8, ticks = FALSE),
      sliderInput("initQU", "Initial Q unfriendly:", min = 1, max = 10,
                  value = 2, ticks = FALSE),
      sliderInput("mu_R_F", "Mean R friendly:", min = 1, max = 10,
                  value = 5, ticks = FALSE),
      sliderInput("mu_R_U", "Mean R unfriendly:", min = 1, max = 10,
                  value = 5, ticks = FALSE),
      sliderInput("sigma_R", "Std dev R:", min = 0, max = 5,
                  value = 2, ticks = FALSE),
      conditionalPanel(condition = "input.conf_bias == 'LRN' &&
                                    input.d_or_c == 'discr'",
        sliderInput("margin", "Margin:", min = 0, max = 5,
                    value = 2, ticks = FALSE)
      ),
    ),
    # OUTPUT
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

server <- function(input, output) {
  params <- reactive({
    p <- list(
      n_part = input$n_part,
      n_trials = input$n_trials,
      inv_temp = input$inv_temp,
      initQ = list(F = input$initQF, U = input$initQU),
      mu_R = list(F = input$mu_R_F, U = input$mu_R_U),
      sigma_R = input$sigma_R
    )

    if (input$conf_bias == 'none') {
      p$LR <- input$LR
    }
    if (input$conf_bias == 'LRN') {
      if (input$d_or_c == 'discr') {
        p$LRs = list(conf = input$LR_conf, disconf = input$LR_disconf)
        p$margin = input$margin
      }
      if (input$d_or_c == 'cont') {
        p$w_LR = input$w_LR
      }
    }
    return(p)
  })

  sim <- new.env()
  source("../sim_for_app.R", local = sim)

  # helper function
  run_discr <- function(p, confirmatory, belief_type) {
    LR_fun <- switch(confirmatory,
                     approx = sim$LR_approx,
                     geq    = sim$LR_geq,
                     stop("error: not 'approx' or 'geq'!"))
    return(sim$run_LRN_discr(p, LR_fun, belief_type))
  }

  dat <- reactive({
    d <- switch(input$conf_bias,
      none = sim$run_std(params()),

      LRN = {
        if (input$d_or_c == "discr") {
          run_discr(params(), input$confirmatory, input$belief_type)
        } else if (input$d_or_c == "cont") {
          sim$run_LRN_cont(params(), input$belief_type)
        } else {
          stop("error: not 'discr' or 'cont'!")
        }
      },

      # RTN = ...

      stop("error: not 'none' or 'LRN'!")
    )
    return(d)
  })

  plot <- new.env()
  source("../../plot_utils.R", local = plot)

  plot_title <- reactive({
    t <- switch(input$conf_bias,
      none = {"std"},

      LRN = {paste(
        "LRN", switch(input$d_or_c,
          discr = {paste("discr", input$confirmatory, input$belief_type, sep = "_")},
          cont = {paste("cont", input$belief_type, sep = "_")}
          ), 
        sep = "_")}
      
      # RTN = ...
    )
    return(paste0("Simulation type: ", t))
  })
  
  output$plot <- renderPlot({
    plot$sim_plots(dat(), NA, plot_title())
 })
}

shinyApp(ui = ui, server = server)