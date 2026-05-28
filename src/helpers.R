# ---- Generic helpers & validation ---- #
# Pure, non-reactive helpers shared across the app.

# Truthiness ----

#' @description checks many types of objects and determines if they're truthy
#' @details Tests for real content, not logical truth. 0 is truthy (valid value).
#' @param x a value to be evaluated as `TRUE` or `FALSE`
#' @returns T/F
truthy <- function(x) {
  # non-values
  if (is.null(x) || inherits(x, "try-error")) {
    return(FALSE)
  }
  if (is.function(x)) {
    return(TRUE)
  }
  if (length(x) == 0) {
    return(FALSE)
  }

  # container types
  if (is.data.frame(x)) {
    return(nrow(x) > 0)
  }
  if (!is.atomic(x)) {
    return(TRUE)
  }

  # atomic: all-NA is false
  if (all(is.na(x))) {
    return(FALSE)
  }

  # type-specific checks
  switch(
    typeof(x),
    character = any(!is.na(x) & nzchar(x) & x != "NA"),
    logical = any(x, na.rm = TRUE),
    TRUE
  )
}

#' @description parses the comma-separated germ speed input, returns an ordered vector of numbers
#' @param x a string to parse
#' @returns a vector of parsed numbers from the string
parseSpeeds <- function(x) {
  parsed <- NULL
  suppressWarnings(
    try({
      vals <- strsplit(x, ",") |>
        unlist() |>
        parse_number() |>
        as.integer() |>
        unique() |>
        sort()
      vals <- vals[vals > 0]
      vals <- vals[vals <= 100]
      parsed <- vals
    })
  )
  return(parsed)
}


# Validation ----

#' @description checks a data column and returns whether it's valid and a message about it
#' @param col data vector to evaluate
#' @param expectedType checks that `col` is this type
#' @param minValue if `col` is numeric, checks this constraint
#' @param maxValue if `col` is numeric, checks this constraint
#' @returns boolean
validateCol <- function(col, expectedType, minValue, maxValue) {
  span_red <- \(...) span(style = "color: red;", ...)
  info <- NULL
  msg <- NULL
  valid <- FALSE

  if (anyNA(col)) {
    msg <- list(
      span_red("Warning: Missing value in data"),
      br()
    )
  }

  colType <- class(col)
  info <- list(paste("Type:", colType), br())

  # column type mismatch
  if (!is.na(expectedType) & colType != expectedType) {
    msg <- append(
      msg,
      list(
        span_red("Error: Incorrect column type, expected", expectedType),
        br()
      )
    )
  }

  # Numeric value checks
  if (colType == "numeric") {
    info <- append(
      info,
      list(
        paste0("Range: ", round(min(col), 2), " 🡒 ", round(max(col), 2)),
        br()
      )
    )

    # min check
    if (!is.na(minValue) & min(col) < minValue) {
      msg <- append(
        msg,
        list(
          span_red("Error: Min value less than", minValue),
          br()
        )
      )
    }

    # max check
    if (!is.na(maxValue) & max(col) > maxValue) {
      msg <- append(
        msg,
        list(
          span_red("Error: Max value greater than ", maxValue),
          br()
        )
      )
    }
  }

  # valid if no messages yet
  if (is.null(msg)) {
    msg <- list(span(strong("OK"), style = "color: blue;"))
    valid <- TRUE
  } else {
    msg <- head(msg, -1) # remove the last br()
  }

  list(
    valid = valid,
    ui = tagList(info, msg)
  )
}


# Column/choice helpers ----

#' @description generates a named list of unique values in a column and adds the count
#' @param df the data frame
#' @param col string specifying column name
#' @returns named list
getColChoices <- function(df, col) {
  df %>%
    count(.data[[col]]) |>
    select(n, everything()) |>
    mutate(n = sprintf("%s (n = %s)", .data[[col]], n)) |>
    deframe()
}

#' @description checks that all required columns are validated for each model
#' @param requirements boolean vector of column requirements
#' @param statuses boolean vector of current column readiness statuses
#' @returns T/F if statuses satisfy requirements
checkModelReady <- function(requirements, statuses) {
  compare <- sapply(1:nCols, function(i) {
    test <-
      (requirements[i] == TRUE & statuses[i] == TRUE) |
      (requirements[i] == FALSE)
    ifelse(length(test) == 0, FALSE, test)
  })
  all(compare)
}


# Germination speed pipeline ----
# Shared by the germination tab and the rate-based models (hydro / hydrothermal
# priming), which previously each inlined this near-identical pipeline.

#' @description adds a per-group incremental fraction (FracDiff) column
#' @param df data frame with CumTime, CumFraction
#' @param groups character vector of grouping columns
#' @returns df with FracDiff added, ungrouped
addFracDiff <- function(df, groups) {
  df %>%
    group_by(across(all_of(groups))) %>%
    arrange(across(all_of(groups)), CumTime, CumFraction) %>%
    mutate(FracDiff = CumFraction - lag(CumFraction, default = 0)) %>%
    ungroup()
}

#' @description rescales cumulative germination per group and interpolates the
#'   time to reach each target fraction (the germination-speed table)
#' @param df data frame that already has FracDiff (see addFracDiff)
#' @param groups character vector of grouping columns
#' @param fracs numeric vector of target germination percentages (0-100)
#' @returns long tibble with one row per group x target fraction: Frac, Time
interpolateGermSpeed <- function(df, groups, fracs) {
  df %>%
    mutate(MaxCumFrac = max(CumFraction), .by = all_of(groups)) %>%
    arrange(CumTime) %>%
    summarise(
      MaxCumFrac = max(MaxCumFrac),
      FracDiff = sum(FracDiff),
      .by = c(all_of(groups), CumTime)
    ) %>%
    mutate(
      CumFraction = cumsum(FracDiff) / sum(FracDiff) * MaxCumFrac,
      .by = all_of(groups)
    ) %>%
    group_by(across(all_of(groups))) %>%
    arrange(CumTime) %>%
    reframe({
      approx(
        CumFraction,
        CumTime,
        xout = fracs / 100,
        ties = "ordered",
        rule = 2
      ) %>%
        setNames(c("Frac", "Time")) %>%
        as_tibble() %>%
        drop_na()
    })
}
