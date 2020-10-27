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
compute_option_pnl <- function(underlyer_space, multiplier = 100, days_to_exp = 0, r = 0.005, b=0.01) {

  time_to_expiration <- as.numeric(self$expiry-self$price_date)/365

  sigma <- fOptions::GBSVolatility(price = self$price,
                                   TypeFlag = self$option_type,
                                   S = self$underlyer_price,
                                   X = self$strike_price,
                                   Time = time_to_expiration,
                                   r = r,
                                   b = b)


  partial_options <- purrr::partial(fOptions::GBSOption,
                                    TypeFlag = self$option_type,
                                    X = self$strike_price,
                                    Time = days_to_exp,
                                    r = r,
                                    b = b,
                                    sigma = sigma
  )

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
compute_strategy_pnl <- function(days_to_exp=0, underlyer_pct_move = 0.1) {

  exp_dates <- lubridate::as_date(purrr::map_dbl(self$legs, "expiry"))

  earliest_exp_date <- min(exp_dates)

  legs_days_to_exp <- as.numeric(exp_dates-earliest_exp_date)+days_to_exp

  strikes <- purrr::map_dbl(self$legs, "strike_price")

  max_underyler_price <- round(max(strikes)*(1+underlyer_pct_move))

  min_underyler_price <- round(min(strikes)*(1-underlyer_pct_move))

  if (unique(legs_days_to_exp)==0){

    underlyer_space <- sort(c(unique(strikes), max_underyler_price, min_underyler_price))

  }else {

    underlyer_space <- seq(from =  min_underyler_price, to = max_underyler_price, length.out = 50)

  }

  pnls <- purrr::map2(self$legs,
                      legs_days_to_exp,
                      .f =  function(leg, days_to_exp) leg$compute_option_pnl(underlyer_space,
                                                                 days_to_exp = days_to_exp))

  opening_prices <- purrr::map(self$legs, "price")

  pnl_scen <- purrr::pmap(
    list(pnls, self$positions, opening_prices),
    ~ (..1 * ..2) + ..3 * -1 * ..2 * 100
  ) %>% purrr::reduce(`+`)


  pnl_dt <- data.table::data.table(underlyer = as.numeric(rownames(pnl_scen)),
                                   pnl = pnl_scen[,1])

  pnl <- underlyer <-  NULL

  pnl_dt[, breakeven:=underlyer-pnl/((pnl-shift(pnl,1))/(underlyer-shift(underlyer,1)))]

  self$pnl <- pnl_dt[, !c("breakeven")]

  self$breakevens <- pnl_dt[is.finite(breakeven), breakeven]

  self$max_profit <- max(pnl_scen)

  self$max_loss <- min(pnl_scen)
}

#' Calculate breakevens
#'
#' @param pnl_crossing dataframe containing a single pnl crossing
#'
#' @return breakeven
#'
calculate_brkeven <- function(pnl_crossing){

  slope <- (pnl_crossing$pnl[1L]-pnl_crossing$pnl[2L])/(pnl_crossing$underlyer[1L]-pnl_crossing$underlyer[2L])

  pnl_crossing$underlyer[1L] - pnl_crossing$pnl[1L]/slope

}

#' Clip strategy pnl plot
#'
#' @return clipped pnl data.table
#'
clip_pnl <- function(pnl_dt){

    pnl_dt[, delta:=(pnl-shift(pnl, n = 5)!=0) | (pnl-shift(pnl, n = -5)!=0)]

  pnl_dt <- na.omit(pnl_dt)

  pnl_dt[,incl:=abs(pnl)<=1.5*mean(abs(pnl))]

  invisible(pnl_dt[delta==T & incl==T])

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

  min_y <- min(self$pnl)*1.2

  max_y <- max(self$pnl)*1.2

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

  base_plot

}



#'@export
Option_Leg <- R6::R6Class(classname = "Option_Leg",
            public = list(strike_price = NA,
                          option_type = NA,
                          underlyer_name = NA,
                          underlyer_price = NA,
                          price_date = NA,
                          expiry = NA,
                          price = NA,
                          initialize = function(strike,
                                                type,
                                                underlyer_name,
                                                underlyer_price,
                                                price_date,
                                                expiry,
                                                price){

                            self$strike_price <- strike
                            self$option_type <- tolower(type)
                            self$underlyer_name <- underlyer_name
                            self$underlyer_price <- underlyer_price
                            self$price_date <- as.Date(price_date)
                            self$expiry <- as.Date(expiry)
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

                                               checkmate::assert(length(positions)==length(legs))

                                               checkmate::assert_list(legs, types = "Option_Leg")

                                               self$legs <- legs

                                               self$positions <- positions

                                             },

                                             plot_strategy_pnl =  plot_strategy_pnl),
                               private = list(compute_strategy_pnl = compute_strategy_pnl,
                                              clip_pnl = clip_pnl))


#' Create strategy from dataframe
#'
#' @param strat_dt
#' @param days_to_exp
#'
#' @return
#'
#'
create_strat <- function(strat_dt, days_to_exp = 0){

  legs <- Map(optPnL::Option_Leg$new,
              strat_dt$Strike,
              strat_dt$Type,
              strat_dt$Underlyer,
              strat_dt$Underlyer_price,
              strat_dt$price_date,
              strat_dt$Expiry,
              strat_dt$Price)

  optPnL::Option_Strategy$new(legs = legs, positions = strat_dt$Position)

}



