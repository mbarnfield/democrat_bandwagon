## amce function

amce_rsq <- function(data,
                     formula,
                     alpha = 0.05) {
  
  
  # estimate linear model
  mod <- lm_robust(formula = formula,
                   data = data,
                   alpha = alpha)
  
  
  # dataframe of results with cis
  mod_df <- broom::tidy(mod) %>%
    select(-df) %>%
    rename(lower = conf.low, upper = conf.high) %>%
    slice(2:nrow(.))
  
  # predictors var to help generate feature var
  predictors <- all.vars(stats::update(formula, 0 ~ .))
  
  # create feature variable - first have to work out no. of unique levels per feature
  features_df <- dplyr::select(data, dplyr::one_of(predictors))
  lengths <- vector("double", ncol(features_df))
  for (i in seq_along(features_df)) {
    lengths[[i]] <- length(unique(features_df[[i]]))
  }
  # -1 because of reference cats
  reps <- (lengths-1)
  # repeat each predictor the corresponding number of times
  mod_df$feature <- rep(predictors, times = reps)
  
  # create reference category
  # create dataframe of only predictor variables
  pred <- all.vars(stats::update(formula, 0 ~ .))
  pred_df <- dplyr::select(data, dplyr::one_of(pred))
  
  # find base level of every predictor (used as reference category in model)
  ref <- vector("character", ncol(pred_df))
  for (i in seq_along(pred_df)){
    ref[i] <- levels(pred_df[[i]])[1]
  }
  
  # dataframe of reference categories with estimate of 0, bind to original data
  mod_df <- data.frame(
    term = ref,
    feature = colnames(pred_df),
    estimate = rep(0, length(ref)),
    std.error = 0,
    statistic = 0,
    p.value = 0,
    lower = 0, 
    upper = 0,
    outcome = mod_df$outcome[1]
  ) %>%
    rbind(mod_df) %>%
    dplyr::arrange(feature, term)
  
  # get rid of feature from level var
  mod_df$term <- as_factor(str_remove_all(as.character(mod_df$term), 
                                          as.character(mod_df$feature)))
  
  # make sure no character vars
  mod_df[sapply(mod_df, is.character)] <-
    lapply(mod_df[sapply(mod_df, is.character)], as.factor)
  
  # construct df in correct format INCL. R2
  mod_df <- mod_df %>%
    rename(t = statistic) %>%
    rename(p = p.value) %>%
    rename(level = term) %>%
    mutate(statistic = "amce") %>%
    mutate(r_squared = summary(mod)$r.squared) %>%
    select(outcome, 
           statistic,
           feature,
           level,
           estimate,
           std.error,
           t,
           p,
           lower, 
           upper,
           r_squared)
  
  # feature headers for plotting
  header_fmt <- "(%s)"
  make_feature_headers <- function(x, fmt = "(%s)") {
    feature_levels <- rev(split(x$level, x$feature))
    
    for (i in seq_along(feature_levels)) {
      feature_levels[[i]] <- levels(x$level)[match(feature_levels[[i]], levels(x$level))]
      feature_levels[[i]] <- c(feature_levels[[i]], sprintf(fmt, names(feature_levels)[i]))
    }
    
    factor(as.character(x$level), levels = unique(unname(unlist(feature_levels))))
  }
  mod_df$level <- make_feature_headers(mod_df)
  to_merge <- data.frame(feature = unique(mod_df$feature), 
                         level = sprintf(header_fmt, unique(mod_df$feature)))
  mod_df <- merge(mod_df, to_merge, all = TRUE)
  
}