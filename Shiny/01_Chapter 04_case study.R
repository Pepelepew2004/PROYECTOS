###################################################################################################
#  Creador: José Eduardo Martínez Téllez
#  Fecha: Julio-2022
#   Titulo:  Learning Shiny
#  Descr: Chapter 04 Case Study - Mastering Shiny
###################################################################################################


# Package -----------------------------------------------------------------

pacman::p_load(shiny, vroom, tidyverse, psych, naniar,
               magrittr, janitor, BBmisc, clipr, skimr)



# Exporting data --------------------------------------------------------------------

# dir.create("neiss")
# download <- function(name) {
#   url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
#   download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
# }
# download("injuries.tsv.gz")
# download("population.tsv")
# download("products.tsv")



# 1 - Data Injuries -------------------------------------------------------


injuries <- vroom::vroom("neiss/injuries.tsv.gz")


# 1.1 - Quick analysis ----------------------------------------------------------

describe(injuries)
summary(injuries)
skimr::skim(injuries)

naniar::gg_miss_var(injuries)


#  2 - Data Productos -----------------------------------------------------

products <- vroom::vroom("neiss/products.tsv")


# 2.1 - Quick analysis ----------------------------------------------------------
describe(products)
summary(products)
skimr::skim(products)

naniar::gg_miss_var(products)




#  3 - Data Population ----------------------------------------------------
population <- vroom::vroom("neiss/population.tsv")



# 3.1 - Quick analysis ----------------------------------------------------------


describe(population)
summary(population)
skimr::skim(population)

naniar::gg_miss_var(population)




# ui ----------------------------------------------------------------------

prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
  #<< first-row
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  #>>
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  #<< tables
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  #>>
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  #<< plot
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  #>>
}

shinyApp(ui, server)