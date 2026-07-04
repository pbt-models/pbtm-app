# ---- Subpopulation mixture models ---- #
# A generic mixture-of-distributions layer over any CDF model spec. Per
# subpopulations.md, a seed lot may be a blend of distinct subpopulations whose
# summed germination matches the overall time course:
#
#   frac = maxFrac * Σ_j  w_j · Φ_j(...) ,   Σ w_j = 1,  w_j ≥ 0
#
# Each component j has its own copy of the model's parameters; mixing weights
# w_j use a stick-breaking parameterisation so each free weight stays in (0,1)
# independently (ideal for nls "port" bounds) while the weights form a valid
# simplex. Because every CDF spec already exposes predict(data, params), the
# mixture is fully generic — no per-model mixture code.

# Weights & prediction ----

#' @description stick-breaking weights from k-1 free fractions w1..w{k-1}
mixtureWeights <- function(p, k) {
  if (k == 1) {
    return(1)
  }
  w <- numeric(k)
  remaining <- 1
  for (j in seq_len(k - 1)) {
    f <- p[[paste0("w", j)]]
    w[j] <- remaining * f
    remaining <- remaining * (1 - f)
  }
  w[k] <- remaining
  w
}

#' @description extract a single component's parameter list from the flat list
componentParams <- function(spec, p, j) {
  setNames(
    lapply(spec$paramNames, function(nm) p[[paste0(nm, j)]]),
    spec$paramNames
  )
}

#' @description predicted response for a k-component mixture
#' @param spec a CDF model spec
#' @param p flat named param list: per-component params (e.g. Tb1, Tb2, ...) and
#'   free weights w1..w{k-1}. For k == 1, plain spec param names.
mixturePredict <- function(
  spec,
  data,
  p,
  k,
  maxFrac = 1,
  transform = identity
) {
  if (k == 1) {
    pp <- setNames(
      lapply(spec$paramNames, function(nm) p[[nm]]),
      spec$paramNames
    )
    return(spec$predict(data, pp, maxFrac = maxFrac, transform = transform))
  }
  w <- mixtureWeights(p, k)
  total <- 0
  for (j in seq_len(k)) {
    total <- total +
      w[j] *
        spec$predict(
          data,
          componentParams(spec, p, j),
          maxFrac = 1,
          transform = transform
        )
  }
  maxFrac * total
}


# Fitting ----

#' @description attach RSS / AIC / k to a results list for model comparison
attachFitStats <- function(res, spec, data, k, maxFrac, transform, npar) {
  obs <- data[[spec$response]]
  pred <- mixturePredict(spec, data, res, k, maxFrac, transform)
  rss <- sum((obs - pred)^2)
  n <- length(obs)
  res$k <- k
  res$npar <- npar
  res$RSS <- rss
  # Gaussian AIC up to an additive constant (fine for comparing k on same data)
  res$AIC <- n * log(rss / n) + 2 * (npar + 1)
  res
}

#' @description build (lower, start, upper) for a k-component mixture, spreading
#'   starting values along the subpop-distinguishing parameter to break symmetry
mixtureStart <- function(spec, ranges, k, base, seed) {
  set.seed(1000 + seed)
  lower <- start <- upper <- list()
  clamp <- function(x, nm) min(max(x, ranges[[nm]][1]), ranges[[nm]][3])

  for (nm in names(ranges)) {
    rng <- ranges[[nm]]
    lower[[nm]] <- rng[1]
    upper[[nm]] <- rng[3]
    start[[nm]] <- rng[2]
  }

  # per-component params seed from the single-population fit, jittered so the
  # restarts explore different basins (the pooled fit's dispersion is inflated,
  # so seeding every component there can collapse the mixture)
  for (j in 1:k) {
    for (nm in spec$paramNames) {
      fn <- paste0(nm, j)
      bval <- if (!is.null(base) && !is.null(base[[nm]])) {
        base[[nm]]
      } else {
        ranges[[fn]][2]
      }
      start[[fn]] <- clamp(bval * runif(1, 0.6, 1.4), fn)
    }
  }

  # spread the distinguishing parameter multiplicatively across components
  sp <- spec$subpopParam
  if (!is.null(sp)) {
    bval <- if (!is.null(base) && !is.null(base[[sp]])) {
      base[[sp]]
    } else {
      spec$params[[sp]][2]
    }
    for (j in 1:k) {
      factor <- (j - (k + 1) / 2)
      mult <- exp(factor * 0.9 * runif(1, 0.7, 1.3))
      start[[paste0(sp, j)]] <- clamp(bval * mult, paste0(sp, j))
    }
  }

  # free stick-breaking weights
  for (j in seq_len(k - 1)) {
    start[[paste0("w", j)]] <- runif(1, 0.3, 0.7)
  }

  list(lower = lower, start = start, upper = upper, fixed = list())
}

#' @description fit a k-component subpopulation mixture for a CDF spec
#' @param spec a CDF model spec
#' @param data the working data (fit response = spec$response)
#' @param k number of subpopulations (k == 1 is the ordinary single fit)
#' @param base optional single-population results list, used to seed starts
#' @param restarts number of perturbed multi-starts (best AIC wins)
#' @returns a results list (params + PseudoR2 + AIC/RSS/k) or an error string
fitMixture <- function(
  spec,
  data,
  k,
  base = NULL,
  maxFrac = 1,
  transform = identity,
  restarts = 12
) {
  if (k == 1) {
    resolved <- resolveParams(
      setNames(as.list(rep(NA, length(spec$params))), spec$paramNames),
      spec$params
    )
    pred <- function(d, p) {
      spec$predict(d, p, maxFrac = maxFrac, transform = transform)
    }
    res <- fitModel(pred, data, resolved, spec$response)
    if (is.list(res)) {
      res <- attachFitStats(
        res,
        spec,
        data,
        1,
        maxFrac,
        transform,
        length(spec$params)
      )
    }
    return(res)
  }

  # flat ranges: per-component params + free weights
  ranges <- list()
  for (j in 1:k) {
    for (nm in spec$paramNames) {
      ranges[[paste0(nm, j)]] <- spec$params[[nm]]
    }
  }
  for (j in seq_len(k - 1)) {
    ranges[[paste0("w", j)]] <- c(0.01, 0.5, 0.99)
  }
  npar <- length(ranges)

  pred <- function(d, p) mixturePredict(spec, d, p, k, maxFrac, transform)

  best <- NULL
  for (s in seq_len(restarts)) {
    resolved <- mixtureStart(spec, ranges, k, base, s)
    res <- fitModel(pred, data, resolved, spec$response)
    if (is.list(res)) {
      res <- attachFitStats(res, spec, data, k, maxFrac, transform, npar)
      if (is.finite(res$AIC) && (is.null(best) || res$AIC < best$AIC)) {
        best <- res
      }
    }
  }
  if (is.null(best)) {
    "Mixture model failed to converge; try fewer subpopulations."
  } else {
    best
  }
}

#' @description fit k = 1..maxK and pick the best by AIC (the "auto" option)
#' @returns list(best = results list for chosen k, fits = list by k,
#'   table = comparison tibble)
detectSubpops <- function(
  spec,
  data,
  maxK = 3,
  maxFrac = 1,
  transform = identity
) {
  base <- fitMixture(spec, data, 1, NULL, maxFrac, transform)
  fits <- list(`1` = base)
  baseList <- if (is.list(base)) base else NULL
  for (k in 2:maxK) {
    fits[[as.character(k)]] <- fitMixture(
      spec,
      data,
      k,
      baseList,
      maxFrac,
      transform
    )
  }

  ok <- Filter(is.list, fits)
  if (length(ok) == 0) {
    return(list(best = base, fits = fits, table = NULL))
  }

  tbl <- tibble(
    k = as.integer(names(ok)),
    npar = vapply(ok, function(r) r$npar, numeric(1)),
    PseudoR2 = vapply(ok, function(r) r$PseudoR2, numeric(1)),
    AIC = vapply(ok, function(r) r$AIC, numeric(1))
  ) %>%
    arrange(k) %>%
    mutate(dAIC = AIC - min(AIC))

  bestK <- tbl$k[which.min(tbl$AIC)]
  list(
    best = fits[[as.character(bestK)]],
    fits = fits,
    table = tbl,
    bestK = bestK
  )
}
