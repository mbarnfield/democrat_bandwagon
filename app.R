###### NEW!!!!!!!!!!!!!!



library(shiny)
library(tidyverse)
library(gmodels)
library(miceadds)
library(estimatr)
library(randomNames)
library(extraDistr)
library(skellam)

# load experiment data ----
experiment <- readRDS("data/experiment.rds")
profiles <- readRDS("data/profiles.rds")

# source helper functions -----
source("helpers.R")

# app -----------------------------------
ui <- fluidPage( # -----------------
                 titlePanel("Bandwagon effect conjoint analysis: USA"),
                 tags$h4(
                   "A conjoint analysis experiment, simulating a Democratic primary as if held in on 28th April, with only two candidates remaining. Scroll down to see, download, and model data."
                 ),
                 br(),
                 br(),
                 tags$h2("Experiment"),
                 br(),
                 selectInput("preference", "Which candidate do you intend to vote for?",
                             c("Joe Biden", "Bernie Sanders")),
                 br(),
                 tags$h3("Candidates"),
                 br(),
                 tabPanel(
                   "Candidates",
                   tags$h5(
                     "Imagine you are voting at the Democratic primary in your state, choosing between the following two candidates. Please consider the information available and indicate how you would like to vote using the buttons below each profile."),
                   fluidRow(
                     column(width = 6, DT::dataTableOutput("cand1"),
                            align = "center"
                     ),
                     column(width = 6, DT::dataTableOutput("cand2"),
                            align = "center"
                     )
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(
                       width = 6,
                       actionButton(inputId = "one", label = "Vote for Candidate One"),
                       align = "center"
                     ),
                     column(
                       width = 6,
                       actionButton(inputId = "two", label = "Vote for Candidate Two"),
                       align = "center"
                     )
                   ),
                   br(),
                   br(),
                   fluidRow(
                     column(
                       width = 12,
                       actionButton(inputId = "abstain", label = "I would not turn out to vote"),
                       align = "center"
                     )
                   ),
                   br()
                 ),
                 br(),
                 tags$h2("Data"),
                 br(),
                 tags$h3("Your Choices"),
                 br(),
                 br(),
                 DT::dataTableOutput("choice"),
                 br(),
                 tags$h3("Download"),
                 radioButtons("filetype", "File type:",
                              choices = c("csv", "tsv")),
                 br(),
                 downloadButton('downloadData', 'Download'),
                 br(),
                 tags$h2("Model"),
                 br(),
                 tags$h3("Your AMCEs"),
                 tags$h5(
                   "Note: please make several choices before running model. It will crash with fewer than two choices made, and will not produce confidence intervals before a minimum of four choices are made (and may often require even more than that -- it depends how many features you have)."
                 ),
                 br(),
                 actionButton(inputId = "model", label = "Run model"),
                 br(),
                 br(),
                 plotOutput("amce_plot", height = 1200),
                 br(),
                 br(),
                 tags$h3("Switching AMCEs - indicates presence of bandwagon effects."),
                 tags$h5(
                   "Note: this model requires you to make a lot of choices, especially if you are rarely switching from your preferred party."
                 ),
                 br(),
                 actionButton(inputId = "switch_model", label = "Run model"),
                 br(),
                 br(),
                 plotOutput("switch_plot", height = 1200)
)


# Define server logic
server <- function(input, output) {
  
  # empty model outputs to be filled later
  amce_r <-
    reactiveValues(
      amces = data.frame(
        data.frame(
          outcome = 0,
          statistic = "amce",
          feature = "all",
          level = "all",
          estimate = 0,
          std.error = 0,
          t = 0,
          p = 0,
          lower = 0,
          upper = 0,
          r_squared = 0
        )),
      switches = data.frame(
        data.frame(
          outcome = 0,
          statistic = "amce",
          feature = "all",
          level = "all",
          estimate = 0,
          std.error = 0,
          t = 0,
          p = 0,
          lower = 0,
          upper = 0,
          r_squared = 0
        )))
  
  # row number starting points to be updated after each task, to move through tasks
  row_num_1 <- 1
  row_num_2 <- 2

  rv_shown <- reactiveValues(
    data1 = profiles[row_num_1,],
    data2 = profiles[row_num_2,])
  
  rv_hidden <- reactiveValues(
    data1 = experiment[row_num_1,],
    data2 = experiment[row_num_2,])
  
  # randomised order of features, to redo after each task
  col_no <- reactiveValues(col_no = sample(ncol(profiles[1, ])))
  
  # empty dataframes to update after tasks
  choice <- reactiveValues(choice = data.frame(),
                           for_model = data.frame(),
                           for_switch_model = data.frame())
  
  # candidate display
  output$cand1 <- DT::renderDataTable({
    t(rv_shown$data1[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate One", ""), rownames = rep(""))
  
  output$cand2 <- DT::renderDataTable({
    t(rv_shown$data2[,as.numeric(col_no$col_no)])},
    options = list(dom = 't', bSort=FALSE), colnames = c("Candidate Two", ""), rownames = rep(""))
  
  # generate new data and log choice data after each task 
  observeEvent(input$one, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data1 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    candidate == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    candidate == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = state,
                                  switched_from_local = case_when(
                                    candidate == input$preference ~ state,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = change,
                                  switched_from_dynamic = case_when(
                                    candidate == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = state_change,
                                  switched_from_local_dynamic = case_when(
                                    candidate == input$preference ~ state_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_trump = rv_hidden$data1[, "vs_trump"],
                                  switched_from_trump = case_when(
                                    candidate == input$preference ~ vs_trump,
                                    TRUE ~ 0
                                  ),
                                  switched_to_contests = rv_hidden$data1[, "contests_won"],
                                  switched_from_contests = case_when(
                                    candidate == input$preference ~ contests_won,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data1[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data1[, "dl_emphasised"]
      )},
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    candidate == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data1[, "polls"],
                                  switched_from = case_when(
                                    candidate == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data1[, "state"],
                                  switched_from_local = case_when(
                                    candidate == input$preference ~ state,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data1[, "change"],
                                  switched_from_dynamic = case_when(
                                    candidate == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data1[, "state_change"],
                                  switched_from_local_dynamic = case_when(
                                    candidate == input$preference ~ state_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_trump = rv_hidden$data1[, "vs_trump"],
                                  switched_from_trump = case_when(
                                    candidate == input$preference ~ vs_trump,
                                    TRUE ~ 0
                                  ),
                                  switched_to_contests = rv_hidden$data1[, "contests_won"],
                                  switched_from_contests = case_when(
                                    candidate == input$preference ~ contests_won,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data1[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data1[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data1[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data1[, "dl_emphasised"])}) %>%
        mutate(switched = sum(switched),
               switched_from = sum(switched_from),
               switched_from_local = sum(switched_from_local),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from_local_dynamic = sum(switched_from_local_dynamic),
               switched_from_trump = sum(switched_from_trump),
               switched_from_contests = sum(switched_from_contests)))
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*2 + input$two*2 + input*abstain*2,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*2 + input$two*2 + input*abstain*2,]
    
    
    rv_shown$data2 <- profiles[row_num_2 + input$one*2 + input$two*2 + input*abstain*2,]
    rv_shown$data1 <- profiles[row_num_1 + input$one*2 + input$two*2 + input*abstain*2,]
    
    col_no$col_no <- sample(ncol(profiles[1, ]))

  
  })
  
  observeEvent(input$two, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data2 %>% mutate(chosen = 1,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$abstain,
                                  profile_no = 1,
                                  switched = case_when(
                                    candidate == input$preference ~ 0,
                                    TRUE ~ 0
                                  ),
                                  switched_to = polls,
                                  switched_from = case_when(
                                    candidate == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = state,
                                  switched_from_local = case_when(
                                    candidate == input$preference ~ state,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = change,
                                  switched_from_dynamic = case_when(
                                    candidate == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = state_change,
                                  switched_from_local_dynamic = case_when(
                                    candidate == input$preference ~ state_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_trump = rv_hidden$data1[, "vs_trump"],
                                  switched_from_trump = case_when(
                                    candidate == input$preference ~ vs_trump,
                                    TRUE ~ 0
                                  ),
                                  switched_to_contests = rv_hidden$data1[, "contests_won"],
                                  switched_from_contests = case_when(
                                    candidate == input$preference ~ contests_won,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = sn_emphasised,
                                  switched_to_local_emph = rv_hidden$data2[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data2[, "dl_emphasised"]
      )},
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 1,
                                  contest_no = input$one + input$two + input$abstain,
                                  profile_no = 2,
                                  switched = case_when(
                                    candidate == input$preference ~ 1,
                                    TRUE ~ 0
                                  ),
                                  switched_to = rv_hidden$data2[, "polls"],
                                  switched_from = case_when(
                                    candidate == input$preference ~ polls,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local = rv_hidden$data2[, "state"],
                                  switched_from_local = case_when(
                                    candidate == input$preference ~ state,
                                    TRUE ~ 0
                                  ),
                                  switched_to_dynamic = rv_hidden$data2[, "change"],
                                  switched_from_dynamic = case_when(
                                    candidate == input$preference ~ change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_local_dynamic = rv_hidden$data2[, "state_change"],
                                  switched_from_local_dynamic = case_when(
                                    candidate == input$preference ~ state_change,
                                    TRUE ~ 0
                                  ),
                                  switched_to_trump = rv_hidden$data1[, "vs_trump"],
                                  switched_from_trump = case_when(
                                    candidate == input$preference ~ vs_trump,
                                    TRUE ~ 0
                                  ),
                                  switched_to_contests = rv_hidden$data1[, "contests_won"],
                                  switched_from_contests = case_when(
                                    candidate == input$preference ~ contests_won,
                                    TRUE ~ 0
                                  ),
                                  switched_to_emph = rv_hidden$data2[, "sn_emphasised"],
                                  switched_to_local_emph = rv_hidden$data2[, "sl_emphasised"],
                                  switched_to_dynamic_emph = rv_hidden$data2[, "dn_emphasised"],
                                  switched_to_local_dynamic_emph = rv_hidden$data1[, "dl_emphasised"])}) %>%
        mutate(switched = sum(switched),
               switched_from = sum(switched_from),
               switched_from_local = sum(switched_from_local),
               switched_from_dynamic = sum(switched_from_dynamic),
               switched_from_local_dynamic = sum(switched_from_local_dynamic),
               switched_from_trump = sum(switched_from_trump),
               switched_from_contests = sum(switched_from_contests)))
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*2 + input$two*2 + input*abstain*2,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*2 + input$two*2 + input*abstain*2,]
    
    
    rv_shown$data2 <- profiles[row_num_2 + input$one*2 + input$two*2 + input*abstain*2,]
    rv_shown$data1 <- profiles[row_num_1 + input$one*2 + input$two*2 + input*abstain*2,]
    
    col_no$col_no <- sample(ncol(profiles[1, ]))
    
    
  })
  
  observeEvent(input$abstain, {
    choice$choice <- rbind(choice$choice, rbind(
      {rv_hidden$data1 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$abstain,
                                  profile_no = 1,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_to_local = 0,
                                  switched_from = 0,
                                  switched_from_local = 0,
                                  switched_to_dynamic = 0,
                                  switched_to_local_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_from_local_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_local_emph = 0,
                                  switched_to_dynamic_emph = 0,
                                  switched_to_local_dynamic_emph = 0,
                                  switched_to_trump = 0,
                                  switched_from_trump = 0,
                                  switched_to_contests = 0,
                                  switched_from_contests = 0
      )}, 
      {rv_hidden$data2 %>% mutate(chosen = 0,
                                  voted = 0,
                                  contest_no = input$one + input$two + input$abstain,
                                  profile_no = 2,
                                  switched = 0,
                                  switched_to = 0,
                                  switched_to_local = 0,
                                  switched_from = 0,
                                  switched_from_local = 0,
                                  switched_to_dynamic = 0,
                                  switched_to_local_dynamic = 0,
                                  switched_from_dynamic = 0,
                                  switched_from_local_dynamic = 0,
                                  switched_to_emph = 0,
                                  switched_to_local_emph = 0,
                                  switched_to_dynamic_emph = 0,
                                  switched_to_local_dynamic_emph = 0,
                                  switched_to_trump = 0,
                                  switched_from_trump = 0,
                                  switched_to_contests = 0,
                                  switched_from_contests = 0
      )}))
    
    rv_hidden$data1 <- experiment[row_num_1 + input$one*2 + input$two*2 + input*abstain*2,]
    rv_hidden$data2 <- experiment[row_num_2 + input$one*2 + input$two*2 + input*abstain*2,]
    
    
    rv_shown$data2 <- profiles[row_num_2 + input$one*2 + input$two*2 + input*abstain*2,]
    rv_shown$data1 <- profiles[row_num_1 + input$one*2 + input$two*2 + input*abstain*2,]
    
    col_no$col_no <- sample(ncol(profiles[1, ]))
    
  })
  
  # amce model
  observeEvent(input$model, {
    choice$for_model <- choice$choice %>%
      mutate(
        sn_emphasised = as_factor(case_when(
          sn_emphasised == 1 ~ "Static national emphasised",
          TRUE ~ "Static national not emphasised"
        )),
        dn_emphasised = as_factor(case_when(
          dn_emphasised == 1 ~ "Dynamic national emphasised",
          TRUE ~ "Dynamic national not emphasised"
        )),
        sl_emphasised = as_factor(case_when(
          sl_emphasised == 1 ~ "Static local emphasised",
          TRUE ~ "Static local not emphasised"
        )),
        dl_emphasised = as_factor(case_when(
          dl_emphasised == 1 ~ "Dynamic local emphasised",
          TRUE ~ "Dynamic local not emphasised"
        )),
        candidate = as_factor(candidate),
        polls = as_factor(paste0("National polls: ", polls, "%")),
        state = as_factor(paste0("State polls: ", state, "%")),
        change = as_factor(paste0("Change in national polls: ", change)),
        state_change = as_factor(paste0("Change in state polls: ", state_change)),
        sn_interaction = interaction(polls, sn_emphasised),
        dn_interaction = interaction(change, dn_emphasised),
        sl_interaction = interaction(state, sl_emphasised),
        dl_interaction = interaction(state_change, dl_emphasised)
      ) %>%
      select(
        candidate,
        polls,
        state,
        change,
        state_change,
        sn_emphasised,
        dn_emphasised,
        sl_emphasised,
        dl_emphasised,
        sn_interaction,
        dn_interaction,
        sl_interaction,
        dl_interaction,
        chosen
      ) 
    
    choice$for_model[, "polls"] <- factor(
      choice$for_model[, "polls"],
      levels = c(
        "National polls: 45%",
        "National polls: 20%",
        "National polls: 55%",
        "National polls: 70%"
      )
    )
    
    
    choice$for_model[, "change"] <- factor(
      choice$for_model[, "change"],
      levels = c(
        "Change in national polls: 0",
        "Change in national polls: -10",
        "Change in national polls: -7",
        "Change in national polls: -5",
        "Change in national polls: -2",
        "Change in national polls: 2",
        "Change in national polls: 5",
        "Change in national polls: 7",
        "Change in national polls: 10"
      )
    )
    
    choice$for_model[, "state_change"] <- factor(
      choice$for_model[, "state_change"],
      levels = c(
        "Change in state polls: 0",
        "Change in state polls: -10",
        "Change in state polls: -7",
        "Change in state polls: -5",
        "Change in state polls: -2",
        "Change in state polls: 2",
        "Change in state polls: 5",
        "Change in state polls: 7",
        "Change in state polls: 10"
      )
    )
    
    choice$for_model[, "state"] <- factor(
      choice$for_model[, "state"],
      levels = c(
        "State polls: 45%",
        "State polls: 20%",
        "State polls: 55%",
        "State polls: 70%"
      )
    )
    
    # run function on data
    amce_r$amces <- amce_rsq(choice$for_model,
                             chosen ~ candidate + polls + state + change + state_change + 
                               sn_interaction + dn_interaction + sl_interaction + dl_interaction
    )
  })
  
  # 'switching amce' model
  observeEvent(input$switch_model, {
    choice$for_switch_model <- choice$choice %>%
      filter(voted == 1) %>%
      mutate(
        switched_to_emph = as_factor(case_when(
          switched_to_emph == 1 ~ "Switched-to party polling emphasised",
          TRUE ~ "Switched-to party polling not emphasised")),
        switched_to_local_emph = as_factor(case_when(
          switched_to_local_emph == 1 ~ "Switched-to party state polling emphasised",
          TRUE ~ "Switched-to party state polling not emphasised")),
        switched_to_dynamic_emph = as_factor(case_when(
          switched_to_dynamic_emph == 1 ~ "Switched-to party change in polling emphasised",
          TRUE ~ "Switched-to party change in polling not emphasised")),
        switched_to_local_dynamic_emph = as_factor(case_when(
          switched_to_local_dynamic_emph == 1 ~ "Switched-to party change in state polling emphasised",
          TRUE ~ "Switched-to party change in state polling not emphasised")),
        candidate = as_factor(candidate),
        switched_to = as_factor(paste0("Switched-to party polling: ", switched_to, "%")),
        switched_from = as_factor(paste0("Switched-from party polling: ", switched_from, "%")),
        switched_to_local = as_factor(paste0("Switched-to party state polling: ", switched_to_local, "%")),
        switched_from_local = as_factor(paste0("Switched-from party state polling: ", switched_from_local, "%")),
        switched_to_dynamic = as_factor(paste0("Switched-to party change in polling: ", switched_to_dynamic)),
        switched_from_dynamic = as_factor(paste0("Switched-from party change in polling: ", switched_from_dynamic)),
        switched_to_local_dynamic = as_factor(paste0("Switched-to party change in state polling: ", switched_to_local_dynamic)),
        switched_from_local_dynamic = as_factor(paste0("Switched-from party change in state polling: ", switched_from_local_dynamic)),
        sn_interaction = interaction(polls, sn_emphasised),
        dn_interaction = interaction(change, dn_emphasised),
        sl_interaction = interaction(state, sl_emphasised),
        dl_interaction = interaction(state_change, dl_emphasised)
      ) %>%
      select(switched_to,
             switched_from,
             switched_to_local,
             switched_from_local,
             switched_to_dynamic,
             switched_from_dynamic,
             switched_to_local_dynamic,
             switched_from_local_dynamic,
             switched_to_emph,
             switched_to_local_emph,
             switched_to_dynamic_emph,
             switched_to_local_dynamic_emph,
             switched) 
    
    choice$for_switch_model[, "switched_to"] <- factor(
      choice$for_switch_model[, "switched_to"],
      levels = c(
        "Switched-to party polling: 45%",
        "Switched-to party polling: 20%",
        "Switched-to party polling: 55%",
        "Switched-to party polling: 70%"
      )
    )
    
    choice$for_switch_model[, "switched_to_local"] <- factor(
      choice$for_switch_model[, "switched_to_local"],
      levels = c(
        "Switched-to party state polling: 45%",
        "Switched-to party state polling: 20%",
        "Switched-to party state polling: 55%",
        "Switched-to party state polling: 70%"
      )
    )
    
    choice$for_switch_model[, "switched_from"] <- factor(
      choice$for_switch_model[, "switched_from"],
      levels = c(
        "Switched-from party polling: 45%",
        "Switched-from party polling: 20%",
        "Switched-from party polling: 55%",
        "Switched-from party polling: 70%"
      )
    )
    
    choice$for_switch_model[, "switched_from_local"] <- factor(
      choice$for_switch_model[, "switched_from_local"],
      levels = c(
        "Switched-from party state polling: 45%",
        "Switched-from party state polling: 20%",
        "Switched-from party state polling: 55%",
        "Switched-from party state polling: 70%"
      )
    )
    
    choice$for_switch_model[, "switched_to_dynamic"] <- factor(
      choice$for_switch_model[, "switched_to_dynamic"],
      levels = c(
        "Switched-to party change in polling: 0",
        "Switched-to party change in polling: -10",
        "Switched-to party change in polling: -7",
        "Switched-to party change in polling: -5",
        "Switched-to party change in polling: -2",
        "Switched-to party change in polling: 2",
        "Switched-to party change in polling: 5",
        "Switched-to party change in polling: 7",
        "Switched-to party change in polling: 10"
      )
    )
    
    choice$for_switch_model[, "switched_from_dynamic"] <- factor(
      choice$for_switch_model[, "switched_from_dynamic"],
      levels = c(
        "Switched-from party change in polling: 0",
        "Switched-from party change in polling: -10",
        "Switched-from party change in polling: -7",
        "Switched-from party change in polling: -5",
        "Switched-from party change in polling: -2",
        "Switched-from party change in polling: 2",
        "Switched-from party change in polling: 5",
        "Switched-from party change in polling: 7",
        "Switched-from party change in polling: 10"
      )
    )
    
    choice$for_switch_model[, "switched_to_local_dynamic"] <- factor(
      choice$for_switch_model[, "switched_to_local_dynamic"],
      levels = c(
        "Switched-to party change in state polling: 0",
        "Switched-to party change in state polling: -10",
        "Switched-to party change in state polling: -7",
        "Switched-to party change in state polling: -5",
        "Switched-to party change in state polling: -2",
        "Switched-to party change in state polling: 2",
        "Switched-to party change in state polling: 5",
        "Switched-to party change in state polling: 7",
        "Switched-to party change in state polling: 10"
      )
    )
    
    choice$for_switch_model[, "switched_from_local_dynamic"] <- factor(
      choice$for_switch_model[, "switched_from_local_dynamic"],
      levels = c(
        "Switched-from party change in state polling: 0",
        "Switched-from party change in state polling: -10",
        "Switched-from party change in state polling: -7",
        "Switched-from party change in state polling: -5",
        "Switched-from party change in state polling: -2",
        "Switched-from party change in state polling: 2",
        "Switched-from party change in state polling: 5",
        "Switched-from party change in state polling: 7",
        "Switched-from party change in state polling: 10"
      )
    )
    
    
    
    # run function on data
    amce_r$switches <- amce_rsq(choice$for_switch_model,
                                switched ~ switched_to + switched_from +
                                  switched_to_local + switched_from_local +
                                  switched_to_dynamic + switched_from_dynamic +
                                  switched_to_local_dynamic + switched_from_local_dynamic +
                                  switched_to_emph + switched_to_dynamic_emph + 
                                  switched_to_local_emph + switched_to_local_dynamic_emph
    )
  })
  
  # data table display
  output$choice <- DT::renderDataTable({
    choice$choice %>% arrange(desc(contest_no))
  })
  
  # data download button
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("choices", input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(choice$choice, file, sep = sep,
                  row.names = FALSE)
    })
  
  # first plot amces
  output$amce_plot <- renderPlot({
    ggplot(amce_r$amces, aes(estimate, level, colour = feature)) +
      geom_vline(linetype = "dashed", alpha = 0.5, xintercept = 0) +
      ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), na.rm = TRUE) +
      ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                              size = 0.2, height = 0, na.rm = TRUE,
                              position = ggstance::position_dodgev(height = 0.75)) +
      labs(x = "Change: Pr(Chosen Candidate)", y = "") +
      theme_minimal()
  })
  
  # second plot 'switching' amces
  output$switch_plot <- renderPlot({
    ggplot(amce_r$switches, aes(estimate, level, colour = feature)) +
      geom_vline(linetype = "dashed", alpha = 0.5, xintercept = 0) +
      ggplot2::geom_point(position = ggstance::position_dodgev(height = 0.75), na.rm = TRUE) +
      ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "lower", xmax = "upper"),  
                              size = 0.2, height = 0, na.rm = TRUE,
                              position = ggstance::position_dodgev(height = 0.75)) +
      labs(x = "Change: Pr(Chosen Candidate)", y = "") +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

