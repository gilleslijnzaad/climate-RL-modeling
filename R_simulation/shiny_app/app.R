library(shiny)

options(shiny.autoreload.legacy_warning = FALSE)
options(shiny.fullstacktrace = FALSE)

ui <- fluidPage(
  titlePanel("Climate-RL simulation"),
  sidebarLayout(
    # INPUTS
    sidebarPanel(
      numericInput(inputId = "n_part",
                   label = "Number of participants:",
                   min = 1,
                   value = 50),
      numericInput(inputId = "n_trials",
                   label = "Number of trials:",
                   min = 1,
                   value = 30),
      sliderInput(inputId = "LR",
                  label = "Learning rate:",
                  min = 0,
                  max = 1,
                  value = 0.4,
                  step = 0.1,
                  ticks = FALSE),
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

    ),
    # OUTPUT
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

server <- function(input, output) {
  sim <- new.env()
  source("../sim.R", local = sim)

  params <- reactive({
    list(
      n_part = input$n_part,
      n_trials = input$n_trials,
      LR = input$LR,
      inv_temp = input$inv_temp,
      initQF = input$initQF,
      initQU = input$initQU,
      mu_R = c(input$mu_R_F, input$mu_R_U), # F and U
      sigma_R = input$sigma_R
    )
  })

  sim_dat <- reactive({
    sim$run_std(params())
  })

  plot <- new.env()
  source("../../plot_utils.R", local = plot)
  
  output$plot <- renderPlot({
    plot$sim_plots(sim_dat(), params())
 })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)