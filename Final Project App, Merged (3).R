library(shiny)
library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(ggforce)
library(ggthemes)
setwd("~/Documents/Harvard, Year 1 /BST Final Project/PetFinder-master/Data")
proj_data <- read.csv("train.csv")
proj_data <- proj_data %>% mutate(purebreed = Breed2==0)
proj_data2 <- proj_data %>% mutate(age_adjusted_years = (Age + 0.5)/12)
cols <- c("Breed1", "Breed2", "Gender", "Color1", "Color2", "Color3", "MaturitySize", "FurLength", 
          "Vaccinated", "Dewormed", "Sterilized", "Health", "State", "AdoptionSpeed", "Type", "purebreed")
proj_data2 %<>% mutate_each_(funs(factor(.)),cols)
proj_data2$Gender <- factor(proj_data2$Gender,
                            levels = c(1,2,3),
                            labels = c("Male", "Female", "Mixed"))
proj_data2$Type <- factor(proj_data2$Type,
                          levels = c(1,2),
                          labels = c("Dog", "Cat"))
proj_data2$purebreed <- factor(proj_data2$purebreed,
                               levels = c(FALSE,TRUE),
                               labels = c("Mixed Breed", "Pure Breed"))
proj_data2$AdoptionSpeed <- factor(proj_data2$AdoptionSpeed, 
                                   levels = c(0,1,2,3,4),
                                   labels = c("Same Day", "Between 1 and 7 days",
                                              "Between 8 and 30 days", "Between 31 and 90 Days",
                                              "No Adoption After 100 Days"))
proj_data2$Color1 <- factor(proj_data$Color1, 
                            levels = c(1,2,3,4,5,6,7),
                            labels = c("Black", "Brown",
                                       "Golden", "Yellow",
                                       "Cream", "Gray", "White"))
proj_data2$Type <- factor(proj_data$Type,levels = c(1,2),labels = c("Dog", "Cat"))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Pet Adoption in Malaysia"),
                tabsetPanel(
                  tabPanel("Adoption Speed of Sheltered Dogs and Cats in Malaysia",
                sidebarLayout(
                  sidebarPanel(
                    p("The grid shows the adoption speeds of dogs and cats by breed status and age. 
                      Please use the selection panel below to stratify the data by gender. 
                      Note: a gender of 'mixed' status indicates the animals were adopted in groups."),
                    br(),
                    selectInput(inputId = "Gender", label = "Select Gender", choices = as.list(levels(proj_data2$Gender)))),
                  mainPanel(plotOutput("plot1")))),
                tabPanel("Adoption Speeds By Color",
                sidebarLayout(
                  sidebarPanel(
                    p("The panel to the right shows the count of dogs and cats stratified by the primary base color of their fur.
                      Please use drop down menu to visualize this data for each adoption speed."),
                    br(),
                    selectInput(inputId = "AdoptionSpeed", label = "Select Adoption Speed", choices = as.list(levels(proj_data2$AdoptionSpeed)))),
                  mainPanel(plotOutput("colorplot"))))))
# Define server logic required to draw a histogram
server <- function(input, output){
  Gender <- reactive(proj_data2 %>% filter(Gender == input$Gender))
  output$plot1 <- renderPlot({proj_data2 %>% filter(Gender == input$Gender) %>% ggplot() +
      theme(text = element_text(size=20, family = "Tahoma"), axis.text.x = element_text(angle = 65, hjust = 1, size=12)) + xlab("Adoption Speed") +
      ylab("Age in Years") + scale_y_continuous(trans = "log2") + geom_boxplot(aes(AdoptionSpeed, age_adjusted_years, fill = AdoptionSpeed)) +
      theme(legend.position = "none") + facet_grid(Type~purebreed)
  })
  
  output$colorplot <- renderPlot({proj_data2 %>% filter(AdoptionSpeed == input$AdoptionSpeed) %>% ggplot() +
      theme(text = element_text(size=20, family = "Tahoma"), axis.text.x = element_text(angle = 65, hjust = 1, size=12)) + xlab("Primary Color") +
      ylab("Count") + geom_bar(aes(Color1, fill = Color1)) + facet_grid(.~Type) +
      theme(legend.position = "none")})}

# Run the application 
shinyApp(ui = ui, server = server)

