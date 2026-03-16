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
        radioButtons("beliefs", "Beliefs are:",
                     c("Static" = "stat", "Dynamic" = "dyn"))
      ),
      hr(),
      
      # PARAMETER SETTINGS
      tags$h3("Parameter settings"),
      # learning rate
      conditionalPanel(condition = "input.conf_bias == 'none'",
        sliderInput("LR", "Learning rate:",
                    min = 0, max = 1,
                    value = 0.4, step = 0.1,
                    ticks = FALSE)
      ),
      conditionalPanel(condition = "input.conf_bias == 'LRN' &&
                                    input.d_or_c == 'discr'",
        sliderInput("LR_conf", "Confirmatory learning rate:",
                    min = 0, max = 1,
                    value = 0.8, step = 0.1,
                    ticks = FALSE),
        sliderInput("LR_disconf", "Disconfirmatory learning rate:",
                    min = 0, max = 1,
                    value = 0.2, step = 0.1,
                    ticks = FALSE)
      ),
      conditionalPanel(condition = "input.conf_bias == 'LRN' &&
                                    input.d_or_c == 'cont'",
        sliderInput("w_LR", "Weight for learning rate",
                    min = 0, max = 1,
                    value = 0.4, step = 0.1,
                    ticks = FALSE)
      ),
      
      sliderInput(inputId = "inv_temp",
                  label = "Inverse temperature:",
                  min = 0,
                  max = 2,
                  value = 0.5,
                  step = 0.1,
                  ticks = FALSE),
      sliderInput(inputId = "initQF",
                  label = "Initial Q friendly:",
                  min = 1,
                  max = 10,
                  value = 8,
                  ticks = FALSE),
      sliderInput(inputId = "initQU",
                  label = "Initial Q unfriendly:",
                  min = 1,
                  max = 10,
                  value = 2,
                  ticks = FALSE),
      sliderInput(inputId = "mu_R_F",
                  label = "Mean R friendly:",
                  min = 1,
                  max = 10,
                  value = 5,
                  ticks = FALSE),
      sliderInput(inputId = "mu_R_U",
                  label = "Mean R unfriendly:",
                  min = 1,
                  max = 10,
                  value = 5,
                  ticks = FALSE),
      sliderInput(inputId = "sigma_R",
                  label = "Std dev R:",
                  min = 0,
                  max = 5,
                  value = 2,
                  ticks = FALSE),
      conditionalPanel(condition = "input.conf_bias == 'LRN' &&
                                    input.d_or_c == 'discr'",
        sliderInput("margin", "Margin:",
                    min = 0, max = 5,
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
      initQF = input$initQF,
      initQU = input$initQU,
      mu_R = c(input$mu_R_F, input$mu_R_U),
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
    p
  })

  sim <- new.env()
  source("../sim.R", local = sim)

  dat <- reactive({
    if (input$conf_bias == 'none') {
      d <- sim$run_std(params())
    }
    if (input$conf_bias == 'LRN') {
      if (input$d_or_c == 'discr') {
        if (input$confirmatory == 'approx') {
          LR_fun <- sim$LR_approx
        }
        if (input$confirmatory == 'geq') {
          LR_fun <- sim$LR_geq
        }
        d <- sim$run_LRN_discr(params(), LR_fun, input$beliefs)
      }
      if (input$d_or_c == 'cont') {
        d <- sim$run_LRN_cont(params(), input$beliefs)
      }
    }
    d
  })

  plot <- new.env()
  source("../../plot_utils.R", local = plot)

  plot_title <- reactive({
    t <- "Simulation type: "
    if (input$conf_bias == 'none') {
      t <- paste0(t, "std")
    } 
    if (input$conf_bias == 'LRN') {
      t <- paste0(t, "LRN")
      if (input$d_or_c == "discr") {
        t <- paste(t, input$d_or_c, input$confirmatory, input$beliefs, sep = "_")

      }
      if (input$d_or_c == "cont") {
        t <- paste(t, input$d_or_c, input$beliefs, sep = "_")
      }
    }
    t
  })
  
  output$plot <- renderPlot({
    plot$sim_plots(dat(), params(), plot_title())
 })
}

shinyApp(ui = ui, server = server)