library(Rcpp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(starvz)
sourceCpp("integrate_step.cpp")

integrate.stepfunc.Rcpp <- function(inter, b, f) integrateStepFunc(inter, c(-Inf, b, Inf), c(0, f, 0))

getBreaks <- function(dfv = NULL) {
  if (is.null(dfv)) {
    return(NULL)
  }
  breaks <- c(dfv$Start, dfv$End[length(dfv$End)])
  return(breaks)
}

getSlices <- function(dfv = NULL, step = 100) {
  tstart <- dfv %>%
    .$Start %>%
    min()
  tend <- (dfv %>% .$End %>% max())
  # TODO: Explain how we ignore the last slice if tend is not multiple
  # of step. This will make us ignore all behavior from the beginning
  # of that last slice until tend. This is unimportant for visualization
  # purposes but it can be important for stats. On that case, replace
  # the second argument by =tend+step= to make sure all data is considered.
  slices <- c(seq(0, tend, step), tend)
  return(slices)
}

time_aggregation_prep <- function(dfw = NULL) {
  if (is.null(dfw)) {
    return(NULL)
  }

  dfw_initial <- dfw %>%
    rename(Task = "Value") %>%
    group_by(.data$ResourceId, .data$Task) %>%
    mutate(Value = 1) %>%
    select(
      -"Duration",
      -"Size", -"Depth", -"Params", -"JobId",
      -"Footprint", -"Tag",
      -"GFlop", -"X", -"Y", -"Subiteration",
      -"Resource", -"Outlier", -"Height",
      -"Position", 
    )

  # Define the first zero
  dfw_zero_1 <- dfw_initial %>%
    slice(1) %>%
    mutate(StartN = 0, EndN = .data$Start, Value = 0)

  # Define other zeroes
  dfw_zero_N <- dfw_initial %>% mutate(StartN = .data$End, EndN = lead(.data$Start), Value = 0)

  # Row bind them
  dfw_agg_prep <- dfw_zero_1 %>%
    bind_rows(dfw_zero_N) %>%
    mutate(Start = .data$StartN, End = .data$EndN) %>%
    select(-"StartN", -"EndN") %>%
    bind_rows(dfw_initial) %>%
    ungroup()

  # Set max end time for NA cases
  dfw_agg_prep <- dfw_agg_prep %>%
    filter(!complete.cases(.)) %>%
    mutate(End = max(dfw$End)) %>%
    bind_rows(dfw_agg_prep %>% filter(complete.cases(.))) %>%
    mutate(Duration = .data$End - .data$Start) %>%
    arrange(.data$ResourceId, .data$Task, .data$Start)

  return(dfw_agg_prep)
}

remyTimeIntegrationPrep <- function(dfv = NULL, myStep = 100) {
  if (is.null(dfv)) {
    return(NULL)
  }
  if (nrow(dfv) == 0) {
    return(NULL)
  }
  mySlices <- getSlices(dfv, step = myStep)
  tibble(Slice = mySlices, Value = c(remyTimeIntegration(dfv, slices = mySlices), 0) / myStep)
}

remyTimeIntegration <- function(dfv = NULL, slices = NULL) {
  if (is.null(dfv)) {
    return(NULL)
  }
  if (is.null(slices)) {
    return(NULL)
  }

  # Define breaks
  breaks <- getBreaks(dfv)

  # Define values on breaks
  values <- dfv$Value

  #result <- integrateStepFuncR(slices, breaks, values)
  result <- integrate.stepfunc.Rcpp(slices, breaks, values)

  return(result)
}

time_aggregation_do <- function(dfw_agg_prep = NULL, step = NA) {
  if (is.null(dfw_agg_prep)) {
    return(NULL)
  }
  if (is.na(step)) {
    return(NULL)
  }

  dfw_agg_prep %>%
    do(remyTimeIntegrationPrep(., myStep = step)) %>%
    mutate(Start = .data$Slice, End = lead(.data$Slice), Duration = .data$End - .data$Start) %>%
    ungroup() %>%
    na.omit()
}

kiter_transform <- function(df, factor_val, slice_size) {
  max_power <- df %>% distinct(Node, ResourceId) %>% nrow()
  
  df %>%
    mutate(
      Value = gsub("^lapack_", "", Value),
      Value = factor(Value, levels = c("dgeqrt", "dlarfb", "dtpqrt", "dtpmqrt")),
      Iteration = as.integer(Iteration / factor_val)
    ) %>%
    time_aggregation_prep() %>%
    group_by(ResourceId, Iteration, Task) %>%
    time_aggregation_do(step = slice_size) %>%
    filter(Value != 0) %>%
    mutate(Load.Core = Value * Duration) %>%
    group_by(Iteration, Slice, Task) %>%
    summarize(Load = sum(Load.Core), .groups = "drop") %>%
    mutate(P.Global.Load = Load / (max_power * slice_size)) %>%
    group_by(Iteration, Slice) %>%
    mutate(
      Load.P.cumsum = cumsum(P.Global.Load),
      X.min = Slice,
      X.max = Slice + slice_size,
      Y.min = (Iteration + (P.Global.Load - Load.P.cumsum)) * factor_val,
      Y.max = (Iteration - Load.P.cumsum) * factor_val
    )
}

# 2. Custom ggplot2 Layers
geom_kiter <- function(factor = 1, slice_size = 100, ...) {
  geom_rect(
    mapping = aes(xmin = X.min, xmax = X.max, ymin = Y.min, ymax = Y.max, fill = Task),
    data = function(df) kiter_transform(df, factor, slice_size),
    inherit.aes = FALSE,
    ...
  )
}

geom_kiter_makespan <- function(size = 5) {
  geom_label(
    mapping = aes(x = Start, y = 30, label = Label),
    data = function(df) {
      data.frame(
        Start = min(df$Start, na.rm = TRUE) / 1000,
        Label = paste(round(max(df$End, na.rm = TRUE) / 1000, 2), "seconds")
      )
    },
    hjust = 0, vjust = 0, fill = "#cce5ff", color = "black", 
    size = size, fontface = "bold", label.size = size / 10,
    inherit.aes = FALSE
  )
}

scale_kiter <- function() {
  list(
    scale_fill_manual(values = c("dgeqrt" = "#e41a1c", "dlarfb" = "#377eb8", "dtpqrt" = "#4daf4a", "dtpmqrt" = "#984ea3")),
    scale_y_reverse(),
    scale_x_continuous(labels = function(x) x / 1000),
    labs(x = "Time [seconds]", y = "Iteration")
  )
}

