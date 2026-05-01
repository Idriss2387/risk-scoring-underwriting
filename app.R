

library(shiny)
library(tidymodels)
library(bundle)
library(xgboost)


bundled_model <- readRDS("modele_bundle.rds")
fit_model <- bundle::unbundle(bundled_model)

ui <- fluidPage(
  titlePanel("Outil de souscription en prévoyance"),
  numericInput("age", "Âge", 40, step = 1),
  numericInput("bmi", "BMI", 25, step = 0.1),
  selectInput("smoker", "Fumeur", choices = c("no", "yes")),
  actionButton("go", "Prédire"),
  br(),
  textOutput("result")
)

server <- function(input, output, session) {
  observeEvent(input$go, {
    tryCatch({
      new_client <- data.frame(
        age = as.numeric(input$age),
        bmi = as.numeric(input$bmi),
        smoker = factor(input$smoker, levels = c("no", "yes"))
      )
      
      pred <- predict(fit_model, new_data = new_client, type = "prob")
      score <- pred$.pred_yes[1]
      
      decision <- ifelse(
        score < 0.2, "Acceptation automatique",
        ifelse(score < 0.5, "Analyse manuelle", "Refus ou restriction")
      )
      
      output$result <- renderText({
        paste0(
          "Score de risque : ", signif(score, 6),
          " | Décision : ", decision
        )
      })
    }, error = function(e) {
      output$result <- renderText(paste("Erreur :", e$message))
    })
  })
}

shinyApp(ui, server)


