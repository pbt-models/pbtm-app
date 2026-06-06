# ---- Plot helpers ---- #

#' @description plot helper
#' @param maxFrac horizontal cutoff value
#' @returns list of ggproto objects to add to ggplot
addFracToPlot <- function(maxFrac) {
  list(
    annotate(
      "rect",
      xmin = 0,
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
#' @returns updated ggplot object
addParamsToPlot <- function(gg, params, y) {
  lineheight = y / 25
  y <- y - lineheight

  gg <- gg +
    annotate(
      "text",
      x = -Inf,
      y = y,
      hjust = -0.1,
      label = "Model parameters:",
      fontface = "bold"
    )

  for (par in params) {
    y <- y - lineheight

    gg <- gg +
      annotate(
        "text",
        x = -Inf,
        y = y,
        hjust = -0.15,
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

#' @description build predicted CDF curve data over a CumTime grid for each
#'   unique combination of a model's factor levels
#' @param spec a CDF model spec
#' @param df the working data (provides factor levels and the time range)
#' @param params named list of fitted/held parameter values
#' @param maxFrac scalar max cumulative fraction
#' @param transform dosage transform (identity or log10)
#' @param n number of grid points
#' @returns tibble with the factor columns, CumTime, and `pred`
buildCdfCurveData <- function(
  spec,
  df,
  params,
  maxFrac,
  transform = identity,
  n = 200
) {
  combos <- distinct(df, across(all_of(spec$factors)))
  tmax <- max(df$CumTime, na.rm = TRUE)
  tseq <- seq(tmax / (n * 10), tmax * 1.05, length.out = n)
  purrr::pmap_dfr(combos, function(...) {
    row <- list(...)
    nd <- tibble(CumTime = tseq)
    for (f in spec$factors) {
      nd[[f]] <- row[[f]]
    }
    nd$pred <- spec$predict(
      nd,
      params,
      maxFrac = maxFrac,
      transform = transform
    )
    nd
  })
}

#' @description predicted combined-mixture curve data over a CumTime grid for
#'   each factor-level combination (uses mixturePredict from fit-mixture.R)
#' @param res mixture results list; @param k number of subpopulations
buildMixtureCurveData <- function(
  spec,
  df,
  res,
  k,
  maxFrac,
  transform = identity,
  n = 200
) {
  combos <- distinct(df, across(all_of(spec$factors)))
  tmax <- max(df$CumTime, na.rm = TRUE)
  tseq <- seq(tmax / (n * 10), tmax * 1.05, length.out = n)
  purrr::pmap_dfr(combos, function(...) {
    row <- list(...)
    nd <- tibble(CumTime = tseq)
    for (f in spec$factors) {
      nd[[f]] <- row[[f]]
    }
    nd$pred <- mixturePredict(
      spec,
      nd,
      res,
      k,
      maxFrac = maxFrac,
      transform = transform
    )
    nd
  })
}


# Plot assemblers ----

#' @description assemble the cumulative-germination plot for a CDF model
#' @param spec a CDF model spec
#' @param df working data
#' @param model results list (rv$lastGoodModel) or NULL if no successful fit
#' @param maxFrac scalar max cumulative fraction
#' @param transform dosage transform
#' @param interactive if TRUE, omit static-only plotmath annotations (added by
#'   the caller as a caption instead) for ggplotly compatibility
buildCdfPlot <- function(
  spec,
  df,
  model,
  maxFrac,
  transform = identity,
  interactive = FALSE
) {
  cfg <- spec$plot
  colorVar <- cfg$colorVar

  plt <- ggplot(
    df,
    aes(x = CumTime, y = CumFraction, color = as.factor(.data[[colorVar]]))
  ) +
    addFracToPlot(maxFrac)

  if (!is.null(cfg$shapeVar)) {
    plt <- plt +
      geom_point(aes(shape = as.factor(.data[[cfg$shapeVar]])), size = 2)
  } else {
    plt <- plt + geom_point(shape = 19, size = 2)
  }

  plt <- plt +
    scale_y_continuous(
      labels = scales::percent,
      expand = expansion(c(0, .05))
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(6),
      expand = expansion(c(0, .05))
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      title = "Cumulative germination",
      caption = "Generated with the PBTM app",
      x = "Time",
      y = "Cumulative fraction germinated (%)",
      color = cfg$colorLab,
      shape = cfg$shapeLab
    ) +
    guides(color = guide_legend(reverse = cfg$legendReverse, order = 1)) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold", size = 14))

  if (is.list(model)) {
    k <- model$k %||% 1
    curve <- if (k > 1) {
      buildMixtureCurveData(spec, df, model, k, maxFrac, transform)
    } else {
      buildCdfCurveData(spec, df, model, maxFrac, transform)
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
    if (k > 1) {
      plt <- plt +
        labs(title = sprintf("%s (%d subpopulations)", cfg$fitTitle, k))
      # mixture has per-component params; show R^2 only (full details in the table)
      if (!interactive) {
        plt <- addParamsToPlot(
          plt,
          list(sprintf("~~R^2==%.3f", model$PseudoR2)),
          1
        )
      }
    } else {
      plt <- plt + labs(title = cfg$fitTitle)
      if (!interactive) {
        plt <- addParamsToPlot(plt, spec$annotate(model, transform), 1)
      }
    }
  }

  plt
}

#' @description assemble the germination-rate plot for a rate model
#' @param spec a rate model spec
#' @param df the germination-speed table (Frac, Time, factor columns)
#' @param model results list (rv$lastGoodModel) or NULL
#' @param interactive if TRUE, omit static-only annotations
buildRatePlot <- function(spec, df, model, interactive = FALSE) {
  cfg <- spec$plot
  validate(need(
    is.list(model),
    "Model results not yet available; adjust settings."
  ))

  df <- as_tibble(df)
  df$.theta <- cfg$theta(df, model)
  df$.gr <- 1 / df$Time
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
      caption = "Generated with the PBTM app",
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
    geom_abline(intercept = model$GRi, slope = model$Slope, color = "blue")
  if (!interactive) {
    plt <- addParamsToPlot(plt, spec$annotate(model), ymax)
  }

  plt
}
