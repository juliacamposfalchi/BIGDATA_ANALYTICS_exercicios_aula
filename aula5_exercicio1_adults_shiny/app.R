if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

url_adults <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"

adults <- read.csv(
  url_adults,
  header = FALSE,
  strip.white = TRUE
)

names(adults) <- c(
  "age", "workclass", "fnlwgt", "education", "education.num",
  "marital.status", "occupation", "relationship", "race", "sex",
  "capital.gain", "capital.loss", "hours.per.week", "native.country",
  "income"
)

adults[adults == "?"] <- NA

cat_vars <- c(
  "workclass", "education", "marital.status", "occupation", "relationship",
  "race", "sex", "native.country", "income"
)

for (v in cat_vars) {
  adults[[v]] <- factor(adults[[v]])
}

adults$income <- factor(trimws(as.character(adults$income)))
adults$income <- factor(gsub("\\.$", "", adults$income))

income_levels <- na.omit(levels(adults$income))
sex_levels <- na.omit(levels(adults$sex))
education_levels <- na.omit(levels(adults$education))

ui <- fluidPage(
  titlePanel("Aula 5 - Exercício 1 (Adults)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "income",
        "Income",
        choices = c("Todos", income_levels),
        selected = "Todos"
      ),
      selectInput(
        "sex",
        "Sex",
        choices = c("Todos", sex_levels),
        selected = "Todos"
      ),
      selectizeInput(
        "education",
        "Education",
        choices = education_levels,
        selected = education_levels,
        multiple = TRUE
      ),
      sliderInput(
        "age",
        "Age",
        min = min(adults$age, na.rm = TRUE),
        max = max(adults$age, na.rm = TRUE),
        value = c(min(adults$age, na.rm = TRUE), max(adults$age, na.rm = TRUE))
      ),
      sliderInput(
        "hours",
        "Hours per week",
        min = min(adults$hours.per.week, na.rm = TRUE),
        max = max(adults$hours.per.week, na.rm = TRUE),
        value = c(min(adults$hours.per.week, na.rm = TRUE), max(adults$hours.per.week, na.rm = TRUE))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Tabela",
          DTOutput("tbl"),
          br(),
          verbatimTextOutput("resumo")
        ),
        tabPanel(
          "Gráficos",
          plotlyOutput("p_age"),
          br(),
          plotlyOutput("p_hours"),
          br(),
          plotlyOutput("p_scatter")
        ),
        tabPanel(
          "Renda vs Educação",
          plotlyOutput("p_income_edu")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dados_filtrados <- reactive({
    df <- adults %>%
      filter(
        !is.na(age),
        !is.na(hours.per.week),
        education %in% input$education,
        age >= input$age[1],
        age <= input$age[2],
        hours.per.week >= input$hours[1],
        hours.per.week <= input$hours[2]
      )

    if (input$income != "Todos") {
      df <- df %>% filter(income == input$income)
    }

    if (input$sex != "Todos") {
      df <- df %>% filter(sex == input$sex)
    }

    df
  })

  output$tbl <- renderDT({
    df <- dados_filtrados() %>%
      select(age, education, sex, hours.per.week, capital.gain, capital.loss, income) %>%
      head(500)

    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$resumo <- renderPrint({
    df <- dados_filtrados()
    cat("Linhas após filtros:", nrow(df), "\n")
    if (nrow(df) > 0) {
      print(df %>% count(income) %>% mutate(prop = n / sum(n)))
    }
  })

  output$p_age <- renderPlotly({
    df <- dados_filtrados()
    p <- ggplot(df, aes(x = age, fill = income)) +
      geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
      theme_minimal() +
      labs(title = "Distribuição de idade", x = "Idade", y = "Frequência")
    ggplotly(p)
  })

  output$p_hours <- renderPlotly({
    df <- dados_filtrados()
    p <- ggplot(df, aes(x = income, y = hours.per.week, fill = income)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Horas por semana por renda", x = "Income", y = "Hours per week") +
      theme(legend.position = "none")
    ggplotly(p)
  })

  output$p_scatter <- renderPlotly({
    df <- dados_filtrados()
    p <- ggplot(df, aes(x = age, y = hours.per.week, color = income)) +
      geom_point(alpha = 0.4) +
      theme_minimal() +
      labs(title = "Idade vs horas por semana", x = "Idade", y = "Hours per week")
    ggplotly(p)
  })

  output$p_income_edu <- renderPlotly({
    df <- dados_filtrados()

    aux <- df %>%
      filter(!is.na(education), !is.na(income)) %>%
      count(education, income) %>%
      group_by(education) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()

    p <- ggplot(aux, aes(x = reorder(education, prop), y = prop, fill = income)) +
      geom_col(position = "stack") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Proporção de renda por nível de educação", x = "Education", y = "Proporção")

    ggplotly(p)
  })
}

shinyApp(ui, server)
