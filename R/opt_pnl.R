#' Compute option leg pnl before accounting for premiums
#'
#' @param anchor Centre point of the pnl range, double
#' @param multiplier Number of underlyer instrument to which option holder is entitled, integer
#' @param days_to_exp Days to expiration, double
#' @param r Annualized rate of interest, double
#' @param b Annualized cost of carry, double
#' @param sigma Annualized volatility/standard deviation, double
#'
#' @return Matrix containing pnl for different levels of the underlyer
#'
#' @importFrom fOptions GBSOption
#' @importFrom purrr map_dbl
#' @importFrom purrr partial
#'
#' @examples
#'
#' \dontrun{
#'
#' compute_option_pnl <-
#'
#' }
#'
compute_option_pnl <- function(anchor, multiplier = 100, days_to_exp = 0, r = 0.05, b=0.05, sigma=0.1) {
  partial_options <- purrr::partial(fOptions::GBSOption,
                                    TypeFlag = tolower(self$option_type),
                                    X = self$strike_price,
                                    Time = days_to_exp/365,
                                    r = r,
                                    b = b,
                                    sigma = sigma
  )

  underlyer_space <- seq(signif(anchor / 2, 2), signif(anchor * 2, 2), by = 0.5)

  option_scenarios <- purrr::map_dbl(underlyer_space, ~ partial_options(S = .) %>% slot("price"))

  option_scenarios[is.nan(option_scenarios)] <- 0

  matrix(
    data = option_scenarios*multiplier,
    nrow = length(underlyer_space),
    ncol = 1, dimnames = list(underlyer_space, "profit")
  )
}

#' Compute strategy pnl
#'
#' @param days_to_exp Number of days to expiration, numeric
#'
#' @import data.table
#'
#' @return List of pnl, breakevens, max profit and max loss
#'
compute_strategy_pnl <- function(days_to_exp=0) {

  strikes <- purrr::map_dbl(self$legs, "strike_price")

  anchor <- round(mean(strikes))

  pnls <- lapply(self$legs, function(leg) leg$compute_option_pnl(anchor,
                                                                 days_to_exp = days_to_exp))

  opening_prices <- purrr::map(self$legs, "price")

  pnl_scen <- purrr::pmap(
    list(pnls, self$positions, opening_prices),
    ~ (..1 * ..2) + ..3 * -1 * ..2 * 100
  ) %>% purrr::reduce(`+`)


  pnl_dt <- data.table::data.table(underlyer = as.numeric(rownames(pnl_scen)),
                                   pnl = pnl_scen[,1])

  pnl <- NULL

  pnl_x <- pnl_dt[sign(pnl) != sign(shift(pnl)) | sign(pnl) != sign(shift(pnl, type = "lead"))  ]

  slope <- (pnl_x$pnl[1]-pnl_x$pnl[2])/(pnl_x$underlyer[1]-pnl_x$underlyer[2])

  self$pnl <- pnl_dt

  self$breakevens <- pnl_x$underlyer[1] - pnl_x$pnl[1]/slope

  self$max_profit <- max(pnl_scen)

  self$max_loss <- min(pnl_scen)
}

#' Plot strategy pnl
#'
#' @param days_to_exp Number of days until expiration for all legs in strategy
#'
#' @import plotly
#'
#' @return plotly plot of strategy pnl
#'
plot_strategy_pnl <- function(days_to_exp=0) {

  private$compute_strategy_pnl(days_to_exp)

  min_y <- min(self$pnl)*1.1

  max_y <- max(self$pnl)*1.1

  font_style <- list(
    "family" = "sans-serif",
    "size" = 16,
    "color" = "white"
  )

  plot_title <- list(
    "text" = "Strategy Profit at expiration",
    "font" = font_style
  )


  base_plot <- plotly::plot_ly() %>%
    plotly::add_trace(
      x = self$pnl$underlyer,
      y = self$pnl$pnl,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
    plotly::layout(
      showlegend = FALSE,
      plot_bgcolor = "#252525",
      paper_bgcolor = "#252525",
      yaxis = list("title" = "Profit/Loss", color = "white"),
      xaxis = list("title" = "Underlyer", color = "white"),
      title = plot_title
    )

  base_plot <- base_plot %>%
    plotly::add_segments(
      x = self$breakevens,
      xend = self$breakevens,
      y = 0,
      yend = max_y-5,
      color = I("grey")
    ) %>%
    plotly::layout(annotations = list(
      x = self$breakevens,
      y = max_y+1,
      text = glue::glue("Breakeven: {self$breakevens}"),
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      font = list(
        "family" = "sans-serif",
        "size" = 12,
        "color" = "white"
      )
    ))

  base_plot

}



#'@export
Option_Leg <- R6::R6Class(classname = "Option_Leg",
            public = list(strike_price = NA,
                          option_type = NA,
                          underlyer = NA,
                          expiry = NA,
                          price = NA,
                          initialize = function(strike,
                                                type,
                                                underlyer,
                                                expiry,
                                                price){

                            self$strike_price <- strike
                            self$option_type <- type
                            self$underlyer <- underlyer
                            self$expiry <- expiry
                            self$price <- price


                          },
                          compute_option_pnl = compute_option_pnl
                          ))
#'@export
Option_Strategy <- R6::R6Class(classname = "Option_Strategy",
                               public = list(legs = NA,
                                             positions = NA,
                                             pnl = NA,
                                             max_profit = NA,
                                             max_loss = NA,
                                             breakevens = NA,
                                             initialize = function(legs, positions){

                                               checkmate::assert_numeric(positions)

                                               checkmate::assert_list(legs, types = "Option_Leg")

                                               self$legs <- legs

                                               self$positions <- positions

                                             },

                                             plot_strategy_pnl =  plot_strategy_pnl),
                               private = list(compute_strategy_pnl = compute_strategy_pnl))


