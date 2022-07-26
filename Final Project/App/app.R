#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(readxl)
library(usmap)
library(ggplot2)
library(glmnet)
library(broom)
library(caret)
library(mgcv)
library(xgboost)

library(shiny)
library(shinyBS)
library(bslib) #theme
library(hablar) #retype()
library(formattable) #comma()

data = read_xlsx("CountyHealthRankingsData2022.xlsx", 4, skip = 1)
add_data = read_xlsx("CountyHealthRankingsData2022.xlsx", 6, skip = 1)

new_data = select(data, -contains(c("CI", "Quartile"), ignore.case = F))
new_add_data = select(add_data, -contains(c("CI", "Quartile"), ignore.case = F))
comb_data = inner_join(new_data, new_add_data, by = c("FIPS", "State", "County"))

reliable_data = comb_data %>%
  mutate(na_id = as.numeric(is.na(comb_data$Unreliable...4))) %>% #binary to is.na
  filter(na_id == 1) #keep only rows w NA (reliable rows)

sub_upper_lim = 200000
sub_lower_lim = 50000

reliable_data = reliable_data %>%
  mutate(county_class = 
           case_when(Population < (sub_upper_lim) & Population > (sub_lower_lim) ~ "Suburban",
                     Population >= (sub_upper_lim) ~ "Urban", 
                     TRUE ~ "Rural"))

county_response_df = reliable_data %>%
  rename(., all_YPLL = 'Years of Potential Life Lost Rate') %>%
  select(., State, County, all_YPLL) %>% 
  na.omit()

demo_response_df = reliable_data %>%
  group_by(State) %>%
  dplyr::slice(1) %>%
  ungroup(State) %>%
  rename(., state = State, all_YPLL = 'Years of Potential Life Lost Rate', Life_Expectancy = 'Life Expectancy') %>%
  select(., contains(c("YPLL Rate (", "state", "all_YPLL")))

index = as.numeric(is.na(demo_response_df$`YPLL Rate (AIAN) Unreliable`))
demo_response_df$`YPLL Rate (AIAN)`[which(index == 0)] = NA

demo_response_clean_df = demo_response_df %>%
  select(., -contains("Unreliable")) %>%
  pivot_longer(cols = !state,
               names_to = "Race", values_to = "YPLL")

state_pred_df = reliable_data %>%
  group_by(State) %>%
  dplyr::slice(1) %>%
  rename(., state = State, YPLL = 'Years of Potential Life Lost Rate') %>%
  summarise(
    #Alcohol/Drug
    drink_per = `% Excessive Drinking`, dui_per = `% Driving Deaths with Alcohol Involvement`, 
    #food/exercise
    food_index = `Food Environment Index`, inactive_per = `% Physically Inactive`, 
    exercise_access_per = `% With Access to Exercise Opportunities`, food_per = `% Food Insecure`, 
    healthy_access_per = `% Limited Access to Healthy Foods`, obesity_rate = `% Adults with Obesity`,
    #tobacco
    smoke = `% Smokers`,
    #sex
    std = `Chlamydia Rate`,
    #response
    YPLL,
    #Cluster
    county_class = as.factor(county_class),
    #Population
    Population) %>%
  na.omit()

final_df = reliable_data %>%
  group_by(State, County) %>%
  rename(., state = State, YPLL = 'Years of Potential Life Lost Rate') %>%
  summarise(
    #Alcohol/Drug
    drink_per = `% Excessive Drinking`, 
    #food/exercise
    inactive_per = `% Physically Inactive`, 
    food_per = `% Food Insecure`, 
    obesity_rate = `% Adults with Obesity`,
    #tobacco
    smoke = `% Smokers`,
    #sex
    std = `Chlamydia Rate`,
    #response
    YPLL,
    #Cluster
    county_class = as.factor(county_class),
    #Population
    Population) %>%
  na.omit()

ypll_hist = function(pred_value){
  
  value = pred_value #predicted value
  num = sum(value > county_response_df$all_YPLL) #count where value > responses
  denom = length(county_response_df$all_YPLL) #count of all responses
  percentile = round(num/denom, 2) #percentile
  per_label = paste(percentile*100, "percentile", sep = " ")
  
  ggplot(county_response_df, aes(x = all_YPLL)) +
    geom_histogram(col = "black", fill = "steelblue", binwidth = 500) + #stat_ecdf alternative but harder for lay to interpret
    geom_vline(xintercept = value, linetype = "dashed") +
    geom_text(aes(x = value, y = 240, label= per_label), nudge_x = 1700, colour = "steelblue", angle = 0, size = 5.5) +
    scale_x_continuous(label = scales::comma) + #the nudge lvl are weird bc im tuning it for shiny
    scale_y_continuous(breaks = seq(0,200,50)) +
    theme_bw() +
    labs(x = "YPLL", y = "Count", title = "Predicted YPLL Percentile Ranking") +
    theme(plot.title = element_text(size = 20))
}

expand_region = function(x){
  
  states_expanded <- if(x == "Northeast"){
    c('Connecticut', 'New Jersey', 'New York', 'Pennsylvania', 'Maine', 'Massachusetts', 'New Hampshire', 'Rhode Island', 
      'Vermont')
    
  } else if(x == "Midwest"){
    c('Illinois', 'Indiana', 'Michigan', 'Ohio', 'Wisconsin', 'Iowa', 'Kansas', 'Minnesota', 'Missouri', 'Nebraska', 
      'North Dakota', 'South Dakota')
    
  } else if(x == "South"){
    c('Delaware', 'Florida', 'Georgia', 'Maryland', 'North Carolina', 'South Carolina', 'Virginia', 'District of Columbia', 
      'West Virginia', 'Alabama', 'Kentucky', 'Mississippi', 'Tennessee', 'Arkansas', 'Louisiana', 'Oklahoma', 'Texas')
    
  } else if(x == "West"){
    c('Arizona', 'Colorado', 'Idaho', 'Montana', 'Nevada', 'New Mexico', 'Utah', 'Wyoming', 'Alaska', 'California', 'Hawaii', 
      'Oregon', 'Washington')
  } else {c()} #All case
  
  states_expanded
}

demo_map = function(df = demo_response_clean_df, Race_Type, limit_set = c(), region = '') {
  
  #Filter readable input to variable name in df
  filter_input = case_when(
    Race_Type == "All" ~ "all_YPLL",
    Race_Type == "Asian" ~ "YPLL Rate (Asian)",
    Race_Type == "Black" ~ "YPLL Rate (Black)",
    Race_Type == "Hispanic" ~ "YPLL Rate (Hispanic)",
    Race_Type == "Native American" ~ "YPLL Rate (AIAN)",
    Race_Type == "White" ~ "YPLL Rate (white)"
  )
  
  #Expand region input so it is readable in plot
  filter_region = expand_region(region)
  
  plot_usmap(data = filter(df, Race == filter_input), values = "YPLL", color = "black", include = filter_region) + 
    scale_fill_continuous(low = "white", high = "red", name = "YPLL", label = scales::comma, limits = limit_set) + 
    theme(legend.position = "right",
          legend.key.size = unit(10, 'mm')) +
    labs(title = "") #Race_Type
  
}

simple_to_label = function(x){
  
  label = case_when(
    x == "Excessive Drinking" ~ "% Excessive Drinking",
    x == "DUI" ~ "% Driving Deaths with Alcohol Involvement",
    x == "Food Index" ~ "Food Environment Index",
    x == "Inactivity" ~ "% Physically Inactive",
    x == "Exercise Opportunity" ~ "% With Access to Exercise Opportunities",
    x == "Food Insecurity" ~ "% Food Insecure",
    x == "Healthy Food Access" ~ "% Limited Access to Healthy Foods",
    x == "Obesity" ~ "% Adults with Obesity",
    x == "Smoking" ~ "% Smokers",
    x == "STI"~ "Chlamydia Rate"
  )
  
}

label_to_varname = function(x){
  
  varname = case_when(
    x == "% Excessive Drinking" ~ "drink_per",
    x == "% Driving Deaths with Alcohol Involvement" ~ "dui_per",
    x == "Food Environment Index" ~ "food_index",
    x == "% Physically Inactive" ~ "inactive_per",
    x == "% With Access to Exercise Opportunities" ~ "exercise_access_per",
    x == "% Food Insecure" ~ "food_per",
    x == "% Limited Access to Healthy Foods" ~ "healthy_access_per",
    x == "% Adults with Obesity" ~ "obesity_rate",
    x == "% Smokers" ~ "smoke",
    x == "Chlamydia Rate" ~ "std"
  )
  varname
}

pred_map = function(df = state_pred_df, pred, limit_set = c(), region = '') {
  
  #Simple to label
  label_x = simple_to_label(pred)
  
  #Label to varname
  varname = label_to_varname(label_x)
  
  #Expand region input so it is readable in plot
  filter_region = expand_region(region)
  
  plot_usmap(data = df, values = varname, color = "black", include = filter_region) + 
    scale_fill_continuous(low = "white", high = "red", name = label_x, label = scales::comma, limits = limit_set) + 
    theme(legend.position = "right",
          legend.key.size = unit(10, 'mm')) +
    labs(title = "") #Race_Type
  
}

final_gam_model <- gam(YPLL ~ s(inactive_per) + s(obesity_rate) + s(food_per) + s(drink_per) + s(smoke) + s(std) +
                         as.factor(county_class), #alternative: s(Population)
                       data = final_df, 
                       method = "REML")

county_options = c("Urban", "Suburban", "Rural") %>% sort()
demo_options = c("All", "Asian", "Black", "Hispanic", "Native American", "White") %>% sort()
region_options = c('All',"Northeast", "Midwest", "South", "West") %>% sort()
df_names = c("county_class", "inactive_per", "obesity_rate", "food_per", "drink_per", "smoke", "std")
pred_options = c("Excessive Drinking", "Inactivity", "Food Insecurity", "Obesity", "Smoking", "STI") %>% sort()

ui <- navbarPage(title = div(img(
  
  src = "https://www.logolynx.com/images/logolynx/e7/e7ec1b0c8c294ff754d8e20c5636e6ae.png", height = 60, width = 210), ""),
  
  windowTitle = "Optum Bridges",
  
  #theme = bs_theme(bootswatch = "lux"), #theme turns off bstooltips
  
  inverse = T,
  
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:18px !important; 
                            padding-bottom:0px !important;
                            height: 90px;
                            }')) #Alter height of navbarpage using CSS
  ),
  
  tabPanel(#classifying css class with text attributes that can be called
    tags$style(".first-p {color: white;
                   padding-top:18px !important;
                   font-size: 16px;
                   font-weight: 500;}"
    ), 
    
    title = p(class = "first-p", "Predictive Modeling"),
    
    fluidPage(
      
      titlePanel(title = h2("Premature Deaths Prediction", align = "center")),
      
      sidebarLayout(
        
        sidebarPanel( 
          selectInput(inputId = "county_class", label = "County Type:", choices = county_options),
          numericInput(inputId = "inactive_per", label = "Inactivity %:", value = 0, min = 0, max = 100, step = 0.1),
          numericInput(inputId = "obesity_rate", label = "Obesity %:", value = 0, min = 0, max = 100, step = 0.1),
          numericInput(inputId = "food_per", label = "Food Insecure %:", value = 0, min = 0, max = 100, step = 0.1),
          numericInput(inputId = "drink_per", label = "Excessive Drinking %:", value = 0, min = 0, max = 100, step = 0.1),
          numericInput(inputId = "smoke", label = "Smoking %:", value = 0, min = 0, max = 100, step = 0.1),
          numericInput(inputId = "std", label = "Chlamydia Cases:", value = 0, min = 0, max = 5000, step = 0.1),
          
          actionButton(inputId = "go", label = "Predict"),
          
          bsTooltip(id = "county_class", title = "County Description"),
          bsTooltip(id = "inactive_per", title = "% of Adults Reporting No Leisure Time Physical Activity"),
          bsTooltip(id = "obesity_rate", title = "% of Adults with BMI > 30"),
          bsTooltip(id = "food_per", title = "% of Population With Poor Access to Food"),
          bsTooltip(id = "drink_per", title = "% of Adults Reporting Binge/Heavy Drinking"),
          bsTooltip(id = "smoke", title = "% of Adults Smoking Tobacco"),
          bsTooltip(id = "std", title = "Number of Chlamydia Cases per 100k")
        ),
        
        mainPanel(br(), #manually add line break to create space for plot
                  plotOutput("plot1"),
                  uiOutput("text1"),
                  tags$head(tags$style("#text1{color: black;
                                 font-size: 22px;
                                 line-height: 17.0;
                                 text-indent: 37px;
                                 }"
                  )))
        
      ))),
  
  navbarMenu(title = p(class = "first-p", "Interactive Map"),
             
             tabPanel(title = "Premature Deaths",
                      
                      fluidPage(
                        
                        titlePanel(title = h2("Premature Deaths Map", align = "center")),
                        
                        sidebarLayout(
                          sidebarPanel( 
                            selectInput(inputId = "demo", label = "Race:", choices = demo_options),
                            selectInput(inputId = "region", label = "Region:", choices = region_options), #optional addition
                            
                            actionButton(inputId = "go_response", label = "Update")
                          ),
                          
                          mainPanel(
                            plotOutput("plot2"))
                        ))         
                      
             ),
             
             tabPanel(title = "Health Behaviors",
                      
                      fluidPage(
                        
                        titlePanel(title = h2("Health Behaviors Map", align = "center")),
                        
                        sidebarLayout(
                          sidebarPanel( 
                            selectInput(inputId = "pred", label = "Health Behavior:", choices = pred_options),
                            selectInput(inputId = "region_pred", label = "Region:", choices = region_options), #optional addition
                            
                            actionButton(inputId = "go_pred", label = "Update")
                          ),
                          
                          mainPanel(
                            plotOutput("plot3"))
                        ))         
                      
             )
  ))

server <- function(input,output){
  
  #store inputs
  x_value <- reactive({
    c(input$county_class, input$inactive_per, input$obesity_rate, input$food_per, input$drink_per, input$smoke, input$std)
  })
  
  #convert inputs to dataframe
  shiny_df <- reactive({
    new_table = data.frame((matrix(nrow = 1, data = x_value()))) %>% retype()
    colnames(new_table) = df_names #get appropriate colnames
    new_table
  })
  
  #Get predictions
  shiny_pred <- reactive({
    predict(final_gam_model, newdata = shiny_df()) %>% comma(., digits = 2) #add comma to thousand and round by 2
  })
  
  #Text output
  pred_reactive <- eventReactive(input$go, #put the expected YPLL text together
                                 {
                                   paste("The expected county YPLL per 100k is:", shiny_pred(), "Years")
                                 })
  
  output$text1 <- renderUI({pred_reactive()})
  
  #Plot output
  ypll_plot_1 <- eventReactive(input$go, {
    ypll_hist(pred_value = as.numeric(shiny_pred()))
  })
  
  output$plot1 <- renderPlot({
    ypll_plot_1()
  }, height = 550, width = 900)
  
  response_plot <- eventReactive(input$go_response, {
    demo_map(Race_Type = input$demo, region = input$region)
  })
  
  output$plot2 <- renderPlot({
    response_plot()
  }, height = 500, width = 800)
  
  pred_plot <- eventReactive(input$go_pred, {
    pred_map(pred = input$pred, region = input$region_pred)
  })
  
  output$plot3 <- renderPlot({
    pred_plot()
  }, height = 500, width = 800)
}


shinyApp(ui = ui, server = server)








