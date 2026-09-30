# ---- Plot helpers ---- #

#' @description plot helper
#' @param maxFrac horizontal cutoff value
#' @param xmin left edge of the shaded band (the axis minimum on a log axis,
#'   where -Inf would become NaN)
#' @returns list of ggproto objects to add to ggplot
addFracToPlot <- function(maxFrac, xmin = -Inf) {
  list(
    annotate(
      "rect",
      xmin = xmin,
      xmax = Inf,
      ymin = maxFrac,
      ymax = 1,
      fill = "grey",
      alpha = 0.1
    ),
    geom_hline(
      yintercept = maxFrac,
      color = "darkgrey",
      linetype = "dashed"
    )
  )
}

#' @description plot helper
#' @param `gg` the ggplot object
#' @param `params` list of params to add, including math notation
#' @param `y` initial max y value to anchor the text
#' @param `probit` TRUE if the y axis is on a probit scale: `y` is then the
#'   top of the visible fraction range and `yMin` its bottom, and the lines are
#'   spaced evenly in probit units
#' @param `yMin` bottom of the visible fraction range (probit axis only)
#' @param `x` x position to anchor the text at (the axis minimum on a log
#'   axis, where -Inf would become NaN)
#' @param `right` TRUE to anchor the text top-right instead of top-left
#' @returns updated ggplot object
addParamsToPlot <- function(
  gg,
  params,
  y,
  probit = FALSE,
  yMin = 1 - y,
  x = if (right) Inf else -Inf,
  right = FALSE
) {
  hjust <- if (right) c(1.05, 1.05) else c(-0.1, -0.15)
  # lay the text out in axis units, then map each position back to data units
  toData <- if (probit) pnorm else identity
  lineheight <- if (probit) (qnorm(y) - qnorm(yMin)) / 25 else y / 25
  y <- (if (probit) qnorm(y) else y) - lineheight

  gg <- gg +
    annotate(
      "text",
      x = x,
      y = toData(y),
      hjust = hjust[1],
      label = "Model parameters:",
      fontface = "bold"
    )

  for (par in params) {
    y <- y - lineheight

    gg <- gg +
      annotate(
        "text",
        x = x,
        y = toData(y),
        hjust = hjust[2],
        label = par,
        parse = TRUE
      )
  }

  gg
}


# Data-first curve building ----
# Predicted curves are built as real data (geom_line) rather than stat_function
# so they render identically in static ggplot and in ggplotly (which does not
# evaluate stat_function on a fine grid).

#' @description predicted CDF curve data over a CumTime grid for each unique
#'   combination of a model's factor levels (single fits and mixtures alike)
#' @param spec a CDF model spec
#' @param df the working data (provides factor levels and the time range)
#' @param fit a pbtm_fit
#' @param n number of grid points
#' @param xScale "linear" or "log"; a log time axis gets a log-spaced grid
#'   starting just below the earliest positive time
#' @returns tibble with the factor columns, CumTime, and `pred`
buildCdfCurveData <- function(spec, df, fit, n = 200, xScale = "linear") {
  combos <- distinct(df, across(all_of(spec$factors)))
  tmax <- max(df$CumTime, na.rm = TRUE)
  tseq <- if (xScale == "log") {
    tmin <- min(df$CumTime[df$CumTime > 0], na.rm = TRUE) / 1.5
    10^seq(log10(tmin), log10(tmax * 1.05), length.out = n)
  } else {
    seq(tmax / (n * 10), tmax * 1.05, length.out = n)
  }
  grid <- tidyr::expand_grid(combos, CumTime = tseq)
  grid$pred <- predict(fit, newdata = grid)
  grid
}


# Axis helpers ----
# Linearizing scales: every CDF model is maxFrac * pnorm(z), so on a probit
# fraction axis (fraction relative to maxFrac) each curve plots as z itself.

#' @description germination fraction y axis
#' @param probit TRUE for a probit axis, FALSE for linear
#' @param relative TRUE if the fraction is relative to the max germination
#' @returns list of ggplot components (scale + y label)
fractionAxis <- function(probit, relative = FALSE) {
  lab <- if (relative) {
    "Germination, % of maximum"
  } else {
    "Cumulative fraction germinated (%)"
  }
  if (!probit) {
    return(list(
      scale_y_continuous(labels = scales::percent, expand = expansion(c(0, .05))),
      labs(y = lab)
    ))
  }
  list(
    scale_y_continuous(
      transform = scales::transform_probit(),
      breaks = c(.001, .01, .05, .1, .25, .5, .75, .9, .95, .99, .999),
      labels = scales::label_percent(drop0trailing = TRUE)
    ),
    labs(y = paste(sub(" (%)", "", lab, fixed = TRUE), "(probit scale)"))
  )
}

#' @description continuous x axis, optionally log10
#' @param log TRUE for a log10 axis
#' @param xLeft left limit of a log axis (a finite edge for anchoring the
#'   max-germination band and annotations; -Inf would become NaN)
#' @param padLeft pad the left of a linear axis too (time axes start flush at 0)
xAxis <- function(log, xLeft = NA, padLeft = FALSE) {
  if (!log) {
    return(scale_x_continuous(
      breaks = scales::breaks_pretty(6),
      expand = expansion(c(if (padLeft) .05 else 0, .05))
    ))
  }
  scale_x_log10(
    # less than a decade: evenly spaced round numbers read better
    breaks = function(lims) {
      if (diff(log10(lims)) < 1) {
        scales::breaks_pretty(6)(lims)
      } else {
        scales::breaks_log(8)(lims)
      }
    },
    labels = scales::label_comma(drop0trailing = TRUE),
    limits = c(xLeft, NA),
    expand = expansion(c(0, .05))
  )
}

#' @description caption noting points that can't be drawn on the chosen axes
#' @param n number of dropped points
#' @param what description of the dropped points
droppedCaption <- function(n, what) {
  if (n > 0) {
    sprintf(
      "%d point%s at %s not shown on these axes.",
      n,
      if (n == 1) "" else "s",
      what
    )
  }
}


# Plot assemblers ----

#' @description assemble the cumulative-germination plot for a CDF model
#' @param spec a CDF model spec
#' @param df working data
#' @param model pbtm_fit (rv$lastGoodModel) or NULL if no successful fit
#' @param maxFrac scalar max cumulative fraction
#' @param interactive if TRUE, omit static-only plotmath annotations (added by
#'   the caller as a caption instead) for ggplotly compatibility
#' @param xScale "linear" or "log" (log10) time axis
#' @param yScale "linear" or "probit" fraction axis. Each fitted curve is
#'   maxFrac * pnorm(z), so on a probit axis the fraction is shown relative to
#'   maxFrac and the curve plots as z itself: exactly straight against log time
#'   for thermal time (log-normal in time), close to straight for the others.
buildCdfPlot <- function(
  spec,
  df,
  model,
  maxFrac,
  interactive = FALSE,
  xScale = "linear",
  yScale = "linear"
) {
  cfg <- spec$plot
  colorVar <- cfg$colorVar
  logX <- identical(xScale, "log")
  probit <- identical(yScale, "probit")

  # 0% / 100% germination (and t = 0 on a log axis) can't be drawn on these
  # scales; drop them explicitly and say so in a caption
  curveSource <- df
  yDiv <- if (probit) maxFrac else 1
  df$CumFraction <- df$CumFraction / yDiv
  keep <- !logX | df$CumTime > 0
  if (probit) keep <- keep & df$CumFraction > 0 & df$CumFraction < 1
  nDropped <- sum(!keep)
  df <- df[keep, , drop = FALSE]
  ylim <- if (probit) range(c(0.01, 0.99, df$CumFraction)) else c(0, 1)
  xLeft <- if (logX) {
    min(curveSource$CumTime[curveSource$CumTime > 0], na.rm = TRUE) / 1.5
  } else {
    -Inf
  }

  plt <- ggplot(
    df,
    aes(x = CumTime, y = CumFraction, color = as.factor(.data[[colorVar]]))
  )
  if (!probit) plt <- plt + addFracToPlot(maxFrac, xmin = xLeft)

  if (!is.null(cfg$shapeVar)) {
    plt <- plt +
      geom_point(aes(shape = as.factor(.data[[cfg$shapeVar]])), size = 2)
  } else {
    plt <- plt + geom_point(shape = 19, size = 2)
  }

  # start the log axis where the fitted curves do (see buildCdfCurveData), so
  # the band and annotations can anchor at a finite left edge
  plt <- plt +
    fractionAxis(probit, relative = probit && maxFrac < 1) +
    xAxis(logX, xLeft) +
    coord_cartesian(ylim = ylim) +
    labs(
      title = "Cumulative germination",
      x = if (logX) "Time (log scale)" else "Time",
      color = cfg$colorLab,
      shape = cfg$shapeLab,
      caption = droppedCaption(
        nDropped,
        if (probit) "0% or 100% germination" else "time 0"
      )
    ) +
    guides(color = guide_legend(reverse = cfg$legendReverse, order = 1)) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold", size = 14))

  if (is.list(model)) {
    k <- model$k
    curve <- buildCdfCurveData(spec, curveSource, model, xScale = xScale)
    curve$pred <- curve$pred / yDiv
    if (probit) {
      # keep the curves' asymptotic tails from reaching +/- Inf on the axis
      curve <- filter(curve, pred > ylim[1] / 2, pred < (1 + ylim[2]) / 2)
    }
    if (!is.null(cfg$lineTypeVar)) {
      plt <- plt +
        geom_line(
          data = curve,
          aes(
            x = CumTime,
            y = pred,
            color = as.factor(.data[[colorVar]]),
            linetype = as.factor(.data[[cfg$lineTypeVar]]),
            group = interaction(.data[[colorVar]], .data[[cfg$lineTypeVar]])
          )
        ) +
        guides(linetype = "none")
    } else {
      plt <- plt +
        geom_line(
          data = curve,
          aes(
            x = CumTime,
            y = pred,
            color = as.factor(.data[[colorVar]]),
            group = as.factor(.data[[colorVar]])
          )
        )
    }
    annotTop <- if (probit) ylim[2] else 1
    if (k > 1) {
      plt <- plt +
        labs(
          title = str_wrap(
            sprintf("%s (%d subpopulations)", cfg$fitTitle, k),
            width = 60
          )
        )
      # mixture has per-component params; show R^2 only (full details in the table)
      if (!interactive) {
        plt <- addParamsToPlot(
          plt,
          list(sprintf("~~R^2==%.3f", model$stats$pseudo_r2)),
          annotTop,
          probit = probit,
          yMin = ylim[1],
          x = xLeft
        )
      }
    } else {
      plt <- plt + labs(title = str_wrap(cfg$fitTitle, width = 60))
      if (!interactive) {
        plt <- addParamsToPlot(
          plt,
          spec$annotate(fitValues(model), fitTransform(model)),
          annotTop,
          probit = probit,
          yMin = ylim[1],
          x = xLeft
        )
      }
    }
  }

  plt
}

#' @description assemble the normalized plot for a single-population CDF fit:
#'   each observation is placed on the model's threshold axis (e.g. thermal
#'   time (T - T_b) * t, or psi - theta_H / t for hydrotime) and germination is
#'   shown relative to maxFrac, so every treatment collapses onto the one
#'   population distribution, a straight line on a probit axis. The dashed line
#'   marks the population median.
#' @param spec a CDF model spec
#' @param df working data (the data the model was fit to)
#' @param model single-population pbtm_fit, or NULL / a mixture (validated)
#' @param maxFrac scalar max cumulative fraction
#' @param interactive if TRUE, omit static-only plotmath annotations
#' @param xScale "linear" or "log"; log applies only to log-normal models
#'   (thermal time), whose threshold axis is positive
#' @param yScale "linear" or "probit" fraction axis
#' @param n number of points on the population curve
buildNormalizedPlot <- function(
  spec,
  df,
  model,
  maxFrac,
  interactive = FALSE,
  xScale = "linear",
  yScale = "probit",
  n = 200
) {
  cfg <- spec$plot
  validate(need(
    is.list(model),
    "Model results not yet available; adjust settings."
  ))
  validate(need(
    model$k == 1,
    "The normalized plot needs a single-population fit. Set Subpopulations to 1, or switch to the time-course plot."
  ))
  m <- pbtm::pbtm_models(spec$pbtm)
  p <- as.list(coef(model))
  transform <- fitTransform(model)
  logX <- identical(xScale, "log") && isTRUE(spec$logNormal)
  probit <- identical(yScale, "probit")

  # threshold-axis position of each observation (thermal time is plotted as
  # (T - T_b) * t rather than its log10, which pbtm uses as the threshold)
  toX <- m$normalized$x %||% m$threshold
  df$.x <- toX(df, p, transform)
  df$CumFraction <- df$CumFraction / maxFrac
  keep <- is.finite(df$.x)
  if (isTRUE(spec$logNormal)) keep <- keep & df$.x > 0
  if (probit) keep <- keep & df$CumFraction > 0 & df$CumFraction < 1
  nDropped <- sum(!keep)
  df <- df[keep, , drop = FALSE]
  validate(need(nrow(df) > 0, "No data points can be shown on these axes."))

  ylim <- if (probit) {
    range(c(0.01, 0.99, df$CumFraction))
  } else {
    c(0, max(1, df$CumFraction))
  }
  rng <- range(df$.x)
  xLeft <- if (logX) rng[1] / 1.2 else -Inf

  # the population distribution over the plotted range
  xs <- if (isTRUE(spec$logNormal)) {
    10^seq(log10(rng[1]), log10(rng[2]), length.out = n)
  } else {
    seq(rng[1], rng[2], length.out = n)
  }
  q <- if (isTRUE(spec$logNormal)) log10(xs) else xs
  line <- tibble(
    .x = xs,
    pred = pnorm(q, m$center(p), p$sigma, lower.tail = m$lower_tail)
  )
  if (probit) {
    line <- filter(line, pred > ylim[1] / 2, pred < (1 + ylim[2]) / 2)
  }
  median <- if (isTRUE(spec$logNormal)) 10^m$center(p) else m$center(p)

  xLab <- cfg$normLab
  if (identical(model$dose_transform, "log10")) {
    xLab <- sub("dose", "log10(dose)", xLab, fixed = TRUE)
  }
  if (logX) xLab <- paste(xLab, "(log scale)")

  plt <- ggplot(df, aes(x = .x, y = CumFraction)) +
    geom_vline(xintercept = median, color = "darkgrey", linetype = "dashed") +
    geom_line(data = line, aes(y = pred), color = "grey20", linewidth = 0.8)
  if (!is.null(cfg$shapeVar)) {
    plt <- plt +
      geom_point(
        aes(
          color = as.factor(.data[[cfg$colorVar]]),
          shape = as.factor(.data[[cfg$shapeVar]])
        ),
        size = 2
      )
  } else {
    plt <- plt +
      geom_point(aes(color = as.factor(.data[[cfg$colorVar]])), size = 2)
  }

  plt <- plt +
    fractionAxis(probit, relative = TRUE) +
    xAxis(logX, xLeft, padLeft = TRUE) +
    coord_cartesian(ylim = ylim) +
    labs(
      title = str_wrap(sprintf("Normalized %s model fit", tolower(spec$label)), 60),
      x = xLab,
      color = cfg$colorLab,
      shape = cfg$shapeLab,
      caption = droppedCaption(
        nDropped,
        # time 0 has no finite threshold; thermal time needs T above T_b
        paste(
          c(
            "time 0",
            if (isTRUE(spec$logNormal)) "a temperature at or below Tb",
            if (probit) "0% or 100% germination"
          ),
          collapse = " or "
        )
      )
    ) +
    guides(color = guide_legend(reverse = cfg$legendReverse, order = 1)) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold", size = 14))

  if (!interactive) {
    # decreasing curves (aging, inhibitor) fill the top left; use the top right
    plt <- addParamsToPlot(
      plt,
      spec$annotate(fitValues(model), transform),
      ylim[2],
      probit = probit,
      yMin = ylim[1],
      x = if (m$lower_tail) xLeft else Inf,
      right = !m$lower_tail
    )
  }

  plt
}

#' @description assemble the germination-rate plot for a rate model
#' @param spec a rate model spec
#' @param df the germination-speed table (pbtm::germ_speed: factor columns,
#'   Fraction, Time, GR)
#' @param model pbtm_fit (rv$lastGoodModel) or NULL
#' @param interactive if TRUE, omit static-only annotations
buildRatePlot <- function(spec, df, model, interactive = FALSE) {
  cfg <- spec$plot
  validate(need(
    is.list(model),
    "Model results not yet available; adjust settings."
  ))
  vals <- fitValues(model)

  df <- as_tibble(df)
  df$.theta <- spec$theta(df, vals)
  df$.gr <- df$GR
  ymax <- max(df$.gr, na.rm = TRUE)

  plt <- ggplot(
    df,
    aes(x = .theta, y = .gr, color = as.factor(.data[[cfg$colorVar]]))
  )

  ptAes <- aes()
  if (!is.null(cfg$shapeVar)) {
    ptAes <- modifyList(ptAes, aes(shape = as.factor(.data[[cfg$shapeVar]])))
  }
  if (!is.null(cfg$sizeVar)) {
    ptAes <- modifyList(ptAes, aes(size = as.factor(.data[[cfg$sizeVar]])))
  }
  if (!is.null(cfg$fixedSize)) {
    plt <- plt + geom_point(mapping = ptAes, size = cfg$fixedSize)
  } else {
    plt <- plt + geom_point(mapping = ptAes)
  }

  plt <- plt +
    scale_y_continuous(expand = expansion(c(0, .05))) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(6),
      expand = expansion(c(0, .05))
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    labs(
      x = cfg$xlab,
      y = "Germination rate",
      color = cfg$colorLab,
      shape = cfg$shapeLab,
      size = cfg$sizeLab
    ) +
    guides(
      color = guide_legend(reverse = cfg$legendReverse, order = 1),
      linetype = "none"
    ) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold", size = 14))

  plt <- plt +
    labs(title = cfg$fitTitle) +
    geom_abline(intercept = vals$gr_i, slope = vals$slope, color = "blue")
  if (!interactive) {
    plt <- addParamsToPlot(plt, spec$annotate(vals), ymax)
  }

  plt
}
