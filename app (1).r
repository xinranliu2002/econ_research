library(dplyr)
library(shiny)
library(tidyverse)
library(DT)
library(RColorBrewer)
library(readxl)


migration_rate <- read_excel("~/Desktop/SUmmer research/Week 8/migration_rate.xlsx")
df_China <- read_excel("~/Desktop/SUmmer research/Week 8/df_China.xlsx")


#user interface
ui <- fluidPage(
  titlePanel(title = "Migration Rate for Townships(%)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year", label = "Select Year", min = 1998, max = 2020, value = 2005)
      
    ),
    mainPanel(
      plotOutput(outputId = "graph"),
      dataTableOutput(outputId = "table")
    )
  )
)

#server function
server <- function(input, output){
  
  
  rate <- reactive({
    migration_rate %>%
      pivot_longer(names_to = "year", values_to = "prop", cols = -"Province") %>%
      filter(year == input$year[1]) %>%
      mutate(prop = prop * 100)%>%
      select(prop, Province)
    })
    
  df_China <- df_China %>%
    select(NAME, lat, long, class, group)
  
  output$graph <- renderPlot({
    
    df_cut <- df_China %>%
      left_join(rate(), by = c("NAME" = "Province")) %>%
      mutate(prop = as.numeric(as.character(prop))) %>%
      filter(class == "Mainland") %>%
      mutate(prop_range = cut(prop, c(0,4,8,12,16,20,24,28,33,38,44,50,56,62)))
    
    df_na <- df_cut %>%
      filter(is.na(prop_range)) %>%
      mutate(prop_range = case_when(is.na(prop_range) ~ "No data"))
    
    df_main <- df_cut %>%
      filter(is.na(prop_range)== FALSE) %>%
      rbind(df_na)
    
    df <- df_main %>%
      mutate(color = case_when(   prop_range == "No data" ~ "#FFFFFF",
                                  prop_range == "(0,4]" ~ "#FDEDEC", 
                                  prop_range == "(4,8]" ~ "#FADBD8",
                                  prop_range == "(8,12]" ~ "#F5B7B1",
                                  prop_range == "(12,16]" ~ "#F1948A",
                                  prop_range == "(16,20]" ~ "#EC7063",
                                  prop_range == "(20,24]" ~ "#E74C3C",
                                  prop_range == "(24,28]" ~ "#CB4335",
                                  prop_range == "(28,33]" ~ "#B03A2E",
                                  prop_range == "(33,38]" ~ "#943126",
                                  prop_range == "(38,44]" ~ "#78281F",
                                  prop_range == "(44,50]" ~ "#661e16",
                                  prop_range == "(50,56]" ~ "#521610",
                                  prop_range == "(56,62]" ~ "#3b0e09"
      ))
    
    no <- data.frame(color = "#FFFFFF")
    
    col <- data.frame(count(df, color)) %>%
      select(color) %>%
      filter(color != "#FFFFFF") %>%
      apply(2, rev) %>%
      as.data.frame() %>%
      rbind(no)
    
    ggplot(df,aes(x = long, y = lat, group = interaction(class, group))) +
      geom_polygon(aes(fill = prop_range), colour = "black", size = 0.3)+
      coord_map("polyconic") +
      scale_fill_manual(values = col$color, na.value = "#FFFFFF")+
      guides(fill = guide_legend(ncol=2, title = "", keywidth = 0.5,
                                 keyheight = 0.5, reverse = FALSE)) +
      xlab("Longitude") +
      ylab("Latitude") + 
      ggtitle("Migration Rate (%) by Province") +
      theme(
        plot.title = element_text(hjust = 0.4),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.19,0.3))
  })
  
  output$table <- renderDataTable({
    table <- rate() %>%
      arrange(desc(prop))
  })
}

#creat app
shinyApp(ui = ui, server = server)