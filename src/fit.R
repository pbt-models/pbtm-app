# ---- Model fitting core ---- #
# Pure fitting functions driven by a spec's predict() function. No reactives and
# no parent.frame() mutation (the previous buildModelParams() approach); inputs
# in, list out, so each piece is unit-testable.

#' @description gets the coefficient estimates from an nls model
#' @param model the nls model
#' @returns a named list
getModelCoefs <- function(model) {
  summary(model)$coefficients |>
    as_tibble(rownames = "Param") |>
    select(1:2) |>
    deframe() |>
    round(6) |>
    as.list()
}

#' @description resolve user-set params + range defaults into nls bounds
#' @param setParams named list/vector of user values (numeric) or NA
#' @param paramRanges named list of c(lower, start, upper)
#' @return list(lower=, start=, upper=, fixed=); fixed holds user-pinned values.
#'   A pinned param has lower == start == upper so the port algorithm holds it.
resolveParams <- function(setParams, paramRanges) {
  lower <- start <- upper <- fixed <- list()
  for (p in names(paramRanges)) {
    val <- setParams[[p]]
    if (truthy(val)) {
      val <- as.numeric(val)
      fixed[[p]] <- val
      lower[[p]] <- val
      start[[p]] <- val
      upper[[p]] <- val
    } else {
      rng <- paramRanges[[p]]
      lower[[p]] <- rng[1]
      start[[p]] <- rng[2]
      upper[[p]] <- rng[3]
    }
  }
  list(lower = lower, start = start, upper = upper, fixed = fixed)
}

#' @description generically fit a PBT model from a predict function
#' @param pred function(data, params_list) -> numeric predicted response vector.
#'   Should already be closed over maxFrac / transform / lower.tail as needed.
#' @param data data frame containing the response column and predictor columns
#' @param resolved output of resolveParams()
#' @param response name of the response column (e.g. "CumFraction" or "GR")
#' @return a named results list (coefs + PseudoR2) or a character error string
#'   (preserves the existing modelResults contract: list = success, string = error)
fitModel <- function(pred, data, resolved, response = "CumFraction") {
  obs <- data[[response]]
  paramNames <- names(resolved$start)

  # nls estimates the named scalar params; build a formula whose RHS calls a
  # closure that reassembles them into the list pred() expects.
  predEnv <- environment(pred)
  env <- new.env(parent = if (is.null(predEnv)) globalenv() else predEnv)
  env$obs <- obs
  env$.pbtModelFn <- function(...) pred(data, list(...))
  rhs <- sprintf(
    ".pbtModelFn(%s)",
    paste(sprintf("%s = %s", paramNames, paramNames), collapse = ", ")
  )
  form <- as.formula(paste("obs ~", rhs), env = env)

  suppressWarnings(
    tryCatch(
      {
        model <- nls(
          formula = form,
          start = resolved$start,
          lower = resolved$lower,
          upper = resolved$upper,
          algorithm = "port",
          control = list(warnOnly = TRUE)
        )
        buildResults(model, paramNames, resolved$fixed, obs)
      },
      error = function(cond) paste(cond[1])
    )
  )
}

#' @description build the results list from a fitted model (pure-core version)
#' @param model fitted nls object
#' @param paramNames character vector of parameter names
#' @param fixed named list of user-pinned values (win over fitted coefs)
#' @param observed observed response vector for the pseudo-R^2
#' @return named list of param values plus PseudoR2 (= cor(obs, pred)^2)
buildResults <- function(model, paramNames, fixed, observed) {
  coefs <- getModelCoefs(model)
  results <- list()
  for (p in paramNames) {
    results[[p]] <- if (!is.null(fixed[[p]])) fixed[[p]] else coefs[[p]]
  }
  results$PseudoR2 <- cor(observed, predict(model))^2
  results
}
