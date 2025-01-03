library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)
library(forcats)
library(ggridges)
library(bslib)
library(showtext)

# Define the Google Sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1iyLtDl6BWISbKic_nHtakDXeZNgzqkm3Vw7cE4ewpn0/edit"

# Function to read data from Google Sheets
read_google_sheet <- function() {
  gs4_deauth() # Deauthorize for public sheets
  data <- read_sheet(sheet_url)
  return(data)
}

# Define UI
ui <- page(
  dir = "rtl",
    page_navbar(
    includeCSS("styles.css"),
    # dir = "rtl",
    # lang = "he",
    title = "באיזו עיר רוצים לגור?",
    nav_panel(
      "לפי תחום", value = "by_question",
      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          h5("בחרו תחום"),
          radioButtons(
            "question",
            label = "",
            choices = c(
              "ממוצע של כלל השאלות",
              "באיזו מידה העיר מאפשרת את הפיתוח המשימתי-כלכלי של התנועה?", 
              "באיזו מידה העיר משפיעה באופן חיובי על יצירת השותפות עם מעגלים אחרים בתנועה?", 
              "באיזו מידה העיר תאפשר את פיתוח המשימה החלוצית של המעגל?", 
              "באיזו מידה יש בעיר תשתיות וגופים שאנו יכולים לחבור אליהם?", 
              "באיזו מידה העיר צריכה אותנו?",
              "באיזו מידה העיר מאפשרת את מימוש המטרות של המעגל (התאגדות, חינוך ילדים, משימה)?", 
              "באיזו מידה אני רוצה לעבור לעיר הזו?"
            )
          )
        ),
        plotOutput("plot_by_question")
      )
    ),
    nav_panel(
      "לפי עיר", value = "by_city",
      layout_sidebar(
        sidebar = sidebar(
          open = TRUE,
          h5("בחרו עיר"),
          radioButtons(
            "city",
            label = "",
            choices = c(
              "ממוצע של כלל הערים",
              "יקנעם",
              "חיפה",
              "נתניה",
              "נהריה",
              "חריש",
              "ירושלים"
            )
          )
        ),
        plotOutput("plot_by_city")
      )
    ),
    nav_item(
      radioButtons(
        "plot_switch",
        label = "",
        inline = TRUE,
        choices = c(
          "תרשים יפה",
          "תרשים נכון"
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  font_add_google("Assistant")
  theme_set(theme_minimal())
  theme_update(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Assistant", face = "bold", size = 20),
  )
  
  df <- read_google_sheet() |> 
    mutate(id = 1:n()) |> 
    pivot_longer(!c(`חותמת זמן`, id), names_to = c("question", "city"), names_sep = " \\[", values_to = "value") |> 
    mutate(
      question = str_sub(question, 4) |> str_trim(),
      city = str_sub(city, end = -2),
      city = fct_reorder(city, value, .fun = mean)
    )
    
  df_question <- reactive({
    if (input$question == "ממוצע של כלל השאלות")
    {
      df
    } else {
      df |> 
        filter(question == input$question)
    }
  })
  
  df_city <- reactive({
    if (input$city == "ממוצע של כלל הערים")
    {
      df
    } else {
      df |> 
        filter(city == input$city)
    }
  })
  
  plot_geoms_question <- reactive({
    if (input$plot_switch == "תרשים יפה")
    {
          geom_density_ridges(
            aes(y = city),
            quantile_lines = TRUE, quantile_fun = mean
          )
    }
    else {
      list(
        geom_histogram(
          bins = 5,
          color = "black"
        ),
          geom_vline(aes(xintercept = mean_value), linewidth = 2),
          facet_grid(rows = vars(city), switch = "y", as.table = FALSE),
          scale_y_continuous(
            position = "right",
            minor_breaks = NULL
          )
      )
    }
  })
  
  plot_geoms_city <- reactive({
    if (input$plot_switch == "תרשים יפה")
    {
      geom_density_ridges(
        aes(y = question),
        quantile_lines = TRUE, quantile_fun = mean
      )
    }
    else {
      list(
        geom_histogram(
          bins = 5,
          color = "black"
        ),
        geom_vline(aes(xintercept = mean_value), linewidth = 2),
        facet_grid(rows = vars(question), switch = "y", as.table = FALSE),
        scale_y_discrete(
          position = "right"
        ),
        theme(strip.text.y.left = element_text(angle = 0))
      )
    }
  })
  
  output$plot_by_question <- renderPlot({
    req(df_question())
    df_question() |> 
      mutate(
        .by = city,
        mean_value = mean(value)
      ) |> 
      mutate(city = fct_reorder(city, value, .fun = mean)) |>
      ggplot(aes(value, fill = mean_value)) + 
      plot_geoms_question() + 
      scale_x_continuous(breaks = 1:5, labels = 1:5) +      scale_fill_brewer(type = "div") +
      scale_fill_gradient(low = "red", high = "blue") +
      labs(
        title = paste0("התפלגות וממוצע מדד לפי עיר", "\n", input$question),
        x = "ערך מדד",
        y = "",
      )
  })
  
  output$plot_by_city <- renderPlot({
    req(df_city())
    df_city() |> 
      mutate(question = str_wrap(question, width = 40)) |> 
      mutate(
        .by = question,
        mean_value = mean(value)
      ) |> 
      mutate(question = fct_reorder(question, value, .fun = mean)) |>
      ggplot(aes(value, fill = mean_value)) + 
      plot_geoms_city() +
      scale_x_continuous(breaks = 1:5, labels = 1:5) +
      scale_fill_gradient(low = "red", high = "blue", breaks = 1:5) +
      labs(
        title = paste("התפלגות וממוצע מדד לפי שאלה,", input$city),
        x = "ערך מדד",
        y = "",
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
