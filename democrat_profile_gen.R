## generate profiles for conjoint experiment

library(shiny)
library(tidyverse)
library(gmodels)
library(miceadds)
library(estimatr)
library(randomNames)
library(extraDistr)
library(skellam)
library(here)

## underlying experimental design
experiment <- data.frame(
  candidate = sample(c("Joe Biden", "Bernie Sanders"), 2),
  polls = rep(sample(c("landslide", "marginal"), 1), times = 2),
  state = rep(sample(c("landslide", "marginal"), 1), times = 2),
  change = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 2),
  state_change = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 2),
  sn_emphasised =
    as.integer(rbernoulli(2, p = 0.2)),
  dn_emphasised = 
    as.integer(rbernoulli(2, p = 0.2)),
  sl_emphasised = 
    as.integer(rbernoulli(2, p = 0.2)),
  dl_emphasised = 
    as.integer(rbernoulli(2, p = 0.2)),
  contests_won = sample(c(5, 10, 20), 2),
  vs_trump = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 2),
  mediate = rep(
    as.integer(
      rbernoulli(1,
                 p = 0.5)), 
    times = 2)) %>%
  mutate(
    polls = case_when(
      polls == "landslide" ~ sample(c(20, 70), 2),
      polls == "marginal" ~ sample(c(45, 55), 2),
      TRUE ~ 0
      ),
    state = case_when(
      state == "landslide" ~ sample(c(20, 70), 2),
      state == "marginal" ~ sample(c(45, 55), 2),
      TRUE ~ 0
    ))


for(i in 1:1000) {
  experiment <- rbind(experiment,
                      data.frame(
                        candidate = sample(c("Joe Biden", "Bernie Sanders"), 2),
                        polls = rep(sample(c("landslide", "marginal"), 1), times = 2),
                        state = rep(sample(c("landslide", "marginal"), 1), times = 2),
                        change = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 2),
                        state_change = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 2),
                        sn_emphasised =
                          as.integer(rbernoulli(2, p = 0.2)),
                        dn_emphasised = 
                          as.integer(rbernoulli(2, p = 0.2)),
                        sl_emphasised = 
                          as.integer(rbernoulli(2, p = 0.2)),
                        dl_emphasised = 
                          as.integer(rbernoulli(2, p = 0.2)),
                        contests_won = sample(c(5, 10, 20), 2),
                        vs_trump = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 2),
                        mediate = rep(
                          as.integer(
                            rbernoulli(1,
                                       p = 0.5)), 
                          times = 2)) %>%
                        mutate(
                          polls = case_when(
                            polls == "landslide" ~ sample(c(20, 70), 2),
                            polls == "marginal" ~ sample(c(45, 55), 2),
                            TRUE ~ 0
                          ),
                          state = case_when(
                            state == "landslide" ~ sample(c(20, 70), 2),
                            state == "marginal" ~ sample(c(45, 55), 2),
                            TRUE ~ 0
                          )))
}


## profiles displayed to respondents - EDIT
profiles <- experiment %>%
  mutate(
    sn_emphasis = case_when(
      sn_emphasised == 1 & polls == 70 ~ 
        "With a very strong showing in national polls, this candidate looks very likely to win the Democratic nomination.",
      sn_emphasised == 1 & polls == 55 ~ 
        "This candidate appears to have the most support in national polls, and looks more likely to win the Democratic nomination.",
      sn_emphasised == 1 & polls == 45 ~
        "This candidate appears to be less well-supported in national polls, and looks less likely to win the Democratic nomination.",
      sn_emphasised == 1 & polls == 20 ~
        "With such low support in national polls, it looks extremely unlikely that this candidate could win the Democratic nomination.",
      TRUE ~ ""
    ),
    sl_emphasis = case_when(
      sl_emphasised == 1 & state == 70 ~ 
        "With such strong support in your state, this candidate looks certain to win the most delegates.",
      sl_emphasised == 1 & state == 55 ~ 
        "With a slight lead in the polls in your state, this candidate looks fairly likely to win the most delegates.",
      sl_emphasised == 1 & state == 45 ~ 
        "Trailing slightly in the polls in your state, it is likely this candidate will win fewer delegates.",
      sl_emphasised == 1 & state == 20   ~ 
        "With such low support in the polls in your state, it looks very unlikely this candidate will win many delegates.",
      TRUE ~ ""
    ),
    dn_emphasis = case_when(
      dn_emphasised == 1 & change <= -5 ~ 
        "This candidate is in a state of crisis, losing considerable ground in the polls.",
      dn_emphasised == 1 & change > -5 & change < 0 ~ 
        "This candidate appears to be losing ground slightly in the polls.",
      dn_emphasised == 1 & change == 0 ~ 
        "This candidate has neither lost nor gained ground in the polls.",
      dn_emphasised == 1 & change < 5 & change > 0 ~ 
        "This candidate appears to be gaining ground slightly in the polls.",
      dn_emphasised == 1 & change >= 5 ~ 
        "This candidate has the wind in his sails and is gaining considerable ground in the polls.",
      TRUE ~ ""
    ),
    dl_emphasis = case_when(
      dl_emphasised == 1 & state_change <= -5 ~ 
        "This candidate appears to be haemorrhaging support in your state.",
      dl_emphasised == 1 & state_change < 0 & state_change > -5  ~ 
        "This candidate seems to have lost support slightly in your state.",
      dl_emphasised == 1 & state_change == 0 ~ 
        "This candidate looks to have about the same level of support in your state as before.",
      dl_emphasised == 1 & state_change > 0 & state_change < 5  ~ 
        "This candidate appears to have gained support slightly in your state.",
      dl_emphasised == 1 & state_change >= 5 ~ 
        "This candidate has considerable momentum in your state, gaining support since the last election.",
      TRUE ~ ""
    ),
    polls = paste0("National polls: ",
                   floor(jitter(as.numeric(polls), amount = 2)), "%"),
    state = paste0("State poll: ", floor(jitter(as.numeric(state), amount = 2)), "%"),
    state_change = case_when(
      state_change >= 0 ~ paste0("State poll change: +", state_change),
      state_change < 0 ~ paste0("State poll change: ", state_change)),
    emphases = paste0(sn_emphasis, " ", sl_emphasis, " ", dn_emphasis, " ", dl_emphasis),
    change = case_when(
      change >= 0 ~ paste0("National poll change: +", change),
      change < 0 ~ paste0("National poll change: ", change)),
    contests_won = paste0("Contests won so far: ", floor(jitter(as.numeric(contests_won), amount = 2))),
    vs_trump = case_when(
      vs_trump >= 0 ~ paste0("Likely performance vs Trump: +", vs_trump),
      vs_trump < 0 ~ paste0("Likely performance vs Trump: ", vs_trump))
    ) %>%
  select(
    candidate,
    polls, 
    change,
    state,
    state_change,
    contests_won,
    vs_trump,
    emphases)


saveRDS(experiment, here::here("Desktop/workflow/democrat_bandwagon/data", "experiment.rds"))
saveRDS(profiles, here::here("Desktop/workflow/democrat_bandwagon/data", "profiles.rds"))
