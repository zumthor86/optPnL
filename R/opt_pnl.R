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

  partial_options <- purrr::partial(fOptions::GBSOption,
                                    TypeFlag = self$option_type,
                                    X = self$strike_price,
                                    Time = days_to_exp/365,
                                    r = r,
                                    b = b,
                                    sigma = self$implied_vol
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

  if (length(unique(legs_days_to_exp))==1){

    underlyer_space <- sort(c(unique(strikes), max_underyler_price, min_underyler_price))

  }else {

    underlyer_space <- round(seq(from =  min_underyler_price, to = max_underyler_price, length.out = 50), digits = 2)

  }

  pnls <- purrr::map2(self$legs,
                      legs_days_to_exp,
                      .f =  function(leg, days_to_exp) leg$compute_option_pnl(underlyer_space,
                                                                 days_to_exp = days_to_exp))

  opening_prices <- purrr::map_dbl(self$legs, "price")

  pnl_scen <- purrr::pmap(
    list(pnls, self$positions, opening_prices),
    ~ calculate_pnl(pnl = ..1, position = ..2, opening_price = ..3)
  ) %>% purrr::reduce(`+`)

  pnl_dt <- data.table::data.table(underlyer = as.numeric(rownames(pnl_scen)),
                                   pnl = pnl_scen[,1])

  pnl <- underlyer <-  NULL

  pnl_dt[, breakeven:=underlyer-pnl/((pnl-shift(pnl,1))/(underlyer-shift(underlyer,1)))]

  self$pnl <- pnl_dt[, !c("breakeven")]

  self$breakevens <- pnl_dt[(pnl*shift(pnl,1))<0, breakeven]

  self$max_profit <- max(pnl_scen)

  self$max_loss <- min(pnl_scen)
}

#' Compute option price scenarios at a given point in time
#'
#' @param scenario_datetime Datetime at which to compute option price scenarios
#' @param option_leg Option Leg object
#' @param vol_min Lower bound of volatility scenarios as a fraction of current volatility
#' @param vol_max Upper bound of volatility scenarios as a fraction of current volatility
#' @param n_scenarios Number of scenarios to compute for underlyer volatility
#' @param underlyer_min Minimum underlyer price
#' @param underlyer_max Maximum underlyer price
#' @param underlyer_prices Vector of underlyer closing prices, matching the option leg prices
#' @param underlyer_margin Integer specifying padding at margins of PnL graph
#'
#' @return Matrix of option prices for different underlyer prices and volatility
#' @export
#'
#' @importFrom purrr partial
#' @importFrom fOptions GBSOption
#' @importFrom purrr cross_df
#' @importFrom purrr map2_dbl
#' @importFrom dplyr mutate
#' @importFrom utils tail
#' @importFrom rlang .data
#' @examples
compute_option_scenarios <- function(scenario_datetime,
                                     vol_change = 0.3,
                                     underlyer_change=0.1,
                                     n_scenarios = 20,
                                     multiplier = 100) {

  time_to_mat <- compute_ttm_years(scenario_datetime, expiry = self$expiry)

  if (time_to_mat < 0) {
    warning("Scenario datetime is after option expiry", call. = FALSE)
    time_to_mat <- 0
  }

  partial_options <- purrr::partial(fOptions::GBSOption,
                                    TypeFlag = self$option_type,
                                    X = self$strike_price,
                                    Time = time_to_mat,
                                    r = 0.005, b = 0
  )

  underlyer_space <- round(seq(self$underlyer_price*(1-underlyer_change),
                         self$underlyer_price*(1+underlyer_change),
                         length.out = n_scenarios), digits = 4)

  volatility_space <- round(seq(self$implied_vol*(1-vol_change),
                         self$implied_vol*(1+vol_change),
                         length.out = n_scenarios), digits = 4)


  option_scenarios <- purrr::cross_df(list("vol" = volatility_space, "underlyer" = underlyer_space)) %>%
    dplyr::mutate(price = purrr::map2_dbl(.data$vol, .data$underlyer, ~ partial_options(S = .y, sigma = .x) %>% slot("price")))

  option_scenarios$price[is.nan(option_scenarios$price)] <- 0

  matrix(
    data = option_scenarios$price*multiplier,
    nrow = length(underlyer_space),
    ncol = n_scenarios,
    byrow = TRUE,
    dimnames = list(underlyer_space, volatility_space)
  )
}
#' Plot an existing positions PnL scenarios at a given time
#'
#' @param strategy Strategy object
#' @param scenario_datetime Time at which to evaluate scenarios
#' @param underlyer_margin Integer specifying padding at margins of PnL graph
#'
#' @return 3D surface plot of PnL scenarios at given time
#'
#' @export
#'
#' @examples
plot_strategy_scenarios <- function(scenario_datetime, underlyer_change) {

    options_scenarios <- purrr::map(
    self$legs,
    ~ .x$compute_option_scenarios(scenario_datetime = scenario_datetime,
                                  underlyer_change = underlyer_change)
  )

  pnl_scen <- purrr::pmap(
    list(options_scenarios, self$opening_prices, self$positions),
    ~ calculate_pnl(pnl = ..1, position = ..3, opening_price = ..2)
  ) %>% purrr::reduce(`+`)

  plot_scenario_surface(pnl_scen)
}

#' Plot option scenarios
#'
#' @param scenario_matrix Matrix of underlyer/volatility permutations
#'
#' @return 3D surface plot of scenarios
#'
#' @examples
plot_scenario_surface <- function(scenario_matrix) {
  font_style <- list(
    "family" = "sans-serif",
    "size" = 16,
    "color" = "white"
  )

  legend_style <- list(
    "font" = list(
      "family" = "sans-serif",
      "size" = 12,
      "color" = "white"
    )
  )

  breakeven <- matrix(data = 0, nrow = nrow(scenario_matrix), ncol = ncol(scenario_matrix))

  plotly::plot_ly(showscale = FALSE) %>%
    plotly::add_surface(
      x = ~ colnames(scenario_matrix),
      y = ~ rownames(scenario_matrix),
      z = ~scenario_matrix, name = "Scenarios",
      hovertemplate = paste0("Volatility: %{x}<br>Underlyer: %{y}<br>Price: %{z}")
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(color = "white", title = list(text = "Volatility Space", font = font_style)),
        yaxis = list(color = "white", title = list(text = "Underlyer Space", font = font_style)),
        zaxis = list(color = "white", title = list(text = "Price", font = font_style))
      ),
      legend = legend_style,
      plot_bgcolor = "#252525",
      paper_bgcolor = "#252525"
    ) %>%
    plotly::add_surface(
      x = ~ colnames(scenario_matrix),
      y = ~ rownames(scenario_matrix),
      z = ~breakeven,
      hoverinfo = "none",
      color = I("white"),
      opacity = 0.5
    )
}

#' Compute the time until maturity of an option, measured in years
#'
#' @param current_time Current DateTime
#' @param expiry DateTime of option expiry
#'
#' @return Time to maturity, measured in years
#' @export
#' @importFrom lubridate interval
#' @importFrom lubridate int_length
#' @importFrom lubridate dyears
#' @examples
compute_ttm_years <- function(current_time, expiry) {
  time_interval <- lubridate::interval(current_time, expiry)

  lubridate::int_length(time_interval) / as.numeric(lubridate::dyears())
}


#' Calculate strategy pnl for single option
#'
#' @param pnl Double : pnl vector
#' @param position Double : position in that option
#' @param opening_price Double
#' @param multiplier Integer : option multiplier
#'
#' @return
#' @export
#'
calculate_pnl <- function(pnl,
                          position,
                          opening_price,
                          multiplier = 100){

  pnl*position - opening_price*position*multiplier


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
                          time_to_expiration = NA,
                          implied_vol = NA,

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

                            self$time_to_expiration <- as.numeric(self$expiry-self$price_date)/365

                            self$implied_vol <- fOptions::GBSVolatility(price = self$price,
                                                             TypeFlag = self$option_type,
                                                             S = self$underlyer_price,
                                                             X = self$strike_price,
                                                             Time = self$time_to_expiration,
                                                             r = 0.005,
                                                             b = 0)




                          },
                          compute_option_pnl = compute_option_pnl,
                          compute_option_scenarios  = compute_option_scenarios
                          ))
#'@export
Option_Strategy <- R6::R6Class(classname = "Option_Strategy",
                               public = list(legs = NA,
                                             positions = NA,
                                             pnl = NA,
                                             max_profit = NA,
                                             max_loss = NA,
                                             breakevens = NA,
                                             strikes = NA,
                                             opening_prices = NA,
                                             expiries = NA,
                                             option_types = NA,
                                             initialize = function(legs, positions){

                                               checkmate::assert_numeric(positions)

                                               checkmate::assert(length(positions)==length(legs))

                                               checkmate::assert_list(legs, types = "Option_Leg")

                                               self$legs <- legs

                                               self$positions <- positions

                                               self$strikes <- purrr::map_dbl(self$legs, "strike_price")

                                               self$opening_prices <- purrr::map_dbl(self$legs, "price")

                                               self$expiries <- purrr::map(self$legs, "expiry")

                                               self$option_types <- purrr::map_chr(self$legs, "option_type")

                                               names(self$legs) <- paste(self$strikes,
                                                                         self$option_types,
                                                                         as.character(self$expiries))

                                             },

                                             plot_strategy_pnl =  plot_strategy_pnl,
                                             plot_strategy_scenarios = plot_strategy_scenarios),
                               private = list(compute_strategy_pnl = compute_strategy_pnl))


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



