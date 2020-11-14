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
compute_option_pnl <- function(underlyer_space,
                               multiplier = 100,
                               days_to_exp = 0:5,
                               r = 0.005,
                               b=0.,
                               implied_vol = self$implied_vol) {

  partial_options <- purrr::partial(fOptions::GBSOption,
                                    TypeFlag = self$option_type,
                                    X = self$strike_price,
                                    # Time = days_to_exp/365,
                                    r = r,
                                    b = b,
                                    sigma = implied_vol
  )

  pnl_day_grid <- expand.grid(underlyer = underlyer_space,
                              days_to_exp = days_to_exp/365,
                              stringsAsFactors = FALSE)

  option_scenarios <- purrr::map2_dbl(pnl_day_grid$underlyer,
                                      pnl_day_grid$days_to_exp,
                                      ~ partial_options(S = .x, Time = .y) %>%
                                        slot("price"))

  option_scenarios[is.nan(option_scenarios)] <- 0

  matrix(
    data = option_scenarios*multiplier,
    nrow = length(underlyer_space),
    ncol = length(days_to_exp),
    dimnames = list(underlyer_space, days_to_exp)
  )
}

#' Compute selected option greek for given vol+price+time
#'
#' @param underlyer_price
#' @param ttm
#' @param implied_vol
#' @param greek
#'
#' @return
#' @export
#'
compute_option_greeks <- function(underlyer_space,
                                  ttm = self$time_to_expiration,
                                  implied_vol = self$implied_vol,
                                  greeks = c("delta", "gamma", "vega", "theta")){

  partial_greeks <- purrr::partial(fOptions::GBSGreeks,TypeFlag = self$option_type,
                                                       X = self$strike_price,
                                                       Time = ttm,
                                                       r = 0.005,
                                                       b = 0,
                                                       sigma = implied_vol)

  greek_grid <- expand.grid(underlyer = underlyer_space,
                            greeks = greeks,
                            stringsAsFactors = FALSE)


  option_scenarios <- purrr::map2_dbl(greek_grid$underlyer,
                                      greek_grid$greeks, ~ partial_greeks(S = .x, Selection = .y))


  matrix(
    data = option_scenarios,
    nrow = length(underlyer_space),
    ncol = length(greeks),
    dimnames = list(underlyer_space, greeks)
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

  earliest_exp_date <- min(self$expiries)

  pnl_dates <- earliest_exp_date-days_to_exp


  legs_days_to_exp <- lapply(as.numeric(self$expiries-earliest_exp_date),
                             function(dte) dte + days_to_exp)

  underlyer_range <- private$calculate_scenario_space(pct_move = underlyer_pct_move, option_attribute = "strikes")

  if (self$is_single_expiry_strat()){

    underlyer_space <- sort(c(unique(self$strikes), min(underlyer_range), max(underlyer_range)))

  }else {

    underlyer_space <- underlyer_range

  }

  pnls <- purrr::map2(self$legs,
                      legs_days_to_exp,
                      .f =  function(leg, days_to_exp) leg$compute_option_pnl(underlyer_space,
                                                                 days_to_exp = days_to_exp))

  pnl_scen <- purrr::pmap(
    list(pnls, self$positions, self$opening_prices),
    ~ aggregate_strategy(option_measure = ..1, position = ..2, opening_price = ..3)) %>%
    purrr::reduce(`+`)

  colnames(pnl_scen) <- format(pnl_dates, "%Y-%m-%d")

  private$finalize_pnl(pnl_scen, pnl_dates)

}


finalize_pnl <- function(pnl_scen, pnl_dates) {

  for (date in pnl_dates) {

    date <- format(lubridate::as_date(date), "%Y-%m-%d")

    pnl_dt <- data.table::data.table(underlyer = as.numeric(rownames(pnl_scen)),
                                     pnl = pnl_scen[,date])

    pnl <- underlyer <- NULL

    pnl_dt[, breakeven:=underlyer-pnl/((pnl-shift(pnl,1))/(underlyer-shift(underlyer,1)))]

    self$pnl[[date]] <- pnl_dt[, !c("breakeven")]

    self$breakevens[[date]] <- pnl_dt[(pnl*shift(pnl,1))<0, breakeven]

    self$max_profit[[date]] <- max(pnl_scen)

    self$max_loss[[date]] <- min(pnl_scen)

  }

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

  time_to_mat <- private$compute_ttm_years(scenario_datetime, expiry = self$expiry)

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

  underlyer_space <- private$calculate_scenario_space(pct_move = underlyer_change,
                                              option_attribute = "strikes",
                                              n_scenarios = n_scenarios)

  volatility_space <- private$calculate_scenario_space(pct_move = vol_change,
                                               option_attribute = "implied_vols",
                                               n_scenarios = n_scenarios)

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
    ~ aggregate_strategy(pnl = ..1, position = ..3, opening_price = ..2)) %>%
    purrr::reduce(`+`)

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
aggregate_strategy <- function(option_measure,
                          position,
                          opening_price,
                          multiplier = 100){

  option_measure*position - opening_price*position*multiplier


}

#' Plot strategy pnl
#'
#' @param days_to_exp
#'
#' @return plotly plot
#' @export
#'
plot_strategy_pnl <- function(days_to_exp=0) {

  private$compute_strategy_pnl(days_to_exp)


  self$pnl_plot <- Option_Plot$new(x = self$pnl[[1]]$underlyer,
                  y = self$pnl[[1]]$pnl,
                  title = "Strategy PnL")

  if (length(days_to_exp)!=1){

    for (pnl in self$pnl[-1]) {

      self$pnl_plot$add_curve(x = pnl$underlyer,
                              y = pnl$pnl)

    }

  }


  self$pnl_plot$plot



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
                          greeks = NA,

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

                            self$time_to_expiration <- private$compute_ttm_years(current_time = self$price_date,
                                                                         expiry = self$expiry)


                            self$implied_vol <- fOptions::GBSVolatility(price = self$price,
                                                             TypeFlag = self$option_type,
                                                             S = self$underlyer_price,
                                                             X = self$strike_price,
                                                             Time = self$time_to_expiration,
                                                             r = 0.005,
                                                             b = 0)

                          },
                          compute_option_pnl = compute_option_pnl,
                          compute_option_scenarios  = compute_option_scenarios,
                          compute_option_greeks = compute_option_greeks
                          ), private = list(compute_ttm_years = compute_ttm_years))
#'@export
Option_Strategy <- R6::R6Class(classname = "Option_Strategy",
                               public = list(legs = NA,
                                             positions = NA,
                                             pnl = list(),
                                             max_profit = list(),
                                             max_loss = list(),
                                             breakevens = list(),
                                             strikes = NA,
                                             opening_prices = NA,
                                             expiries = NA,
                                             option_types = NA,
                                             implied_vols = NA,
                                             pnl_plot = NA,
                                             initialize = function(legs, positions){

                                               checkmate::assert_numeric(positions)

                                               checkmate::assert(length(positions)==length(legs))

                                               checkmate::assert_list(legs, types = "Option_Leg")

                                               self$legs <- legs

                                               self$positions <- positions

                                               self$strikes <- purrr::map_dbl(self$legs, "strike_price")

                                               self$opening_prices <- purrr::map_dbl(self$legs, "price")

                                               self$expiries <- lubridate::as_date(purrr::map_dbl(self$legs, "expiry"))

                                               self$option_types <- purrr::map_chr(self$legs, "option_type")

                                               self$implied_vols <- purrr::map_dbl(self$legs, "implied_vol")

                                               names(self$legs) <- paste(self$strikes,
                                                                         self$option_types,
                                                                         format(self$expiries, "%Y-%m-%d"))

                                             },
                                             is_single_expiry_strat = function(){

                                               min(self$expiries)==max(self$expiries)

                                             },

                                             plot_strategy_pnl =  plot_strategy_pnl,
                                             plot_strategy_scenarios = plot_strategy_scenarios),
                               private = list(compute_strategy_pnl = compute_strategy_pnl,
                                              aggregate_strategy = aggregate_strategy,
                                              finalize_pnl = finalize_pnl,
                                              calculate_scenario_space = function(pct_move,
                                                                                  option_attribute = "strikes",
                                                                                  n_scenarios=50){

                                                max_scenario <- round(max(self[[option_attribute]])*(1+pct_move))

                                                min_scenario <- round(min(self[[option_attribute]])*(1-pct_move))

                                                round(seq(from =  min_scenario,
                                                          to = max_scenario,
                                                          length.out = n_scenarios),
                                                      digits = 2)

                                              }))
#'@export
Option_Plot <- R6::R6Class(classname = "Option_Plot",
                           public = list(initialize = function(x,
                                                               y,
                                                               x_lab = "Underlyer",
                                                               y_lab = "Option_measure",
                                                               title = "Option_Scenarios"){


                                           self$plot_title$text <- title

                                           self$plot <- plotly::plot_ly() %>%
                                             plotly::add_trace(
                                               x = x,
                                               y = y,
                                               type = "scatter",
                                               mode = "lines+markers"
                                             ) %>%
                                             plotly::layout(
                                               showlegend = FALSE,
                                               plot_bgcolor = "#252525",
                                               paper_bgcolor = "#252525",
                                               yaxis = list("title" = y_lab, color = "white"),
                                               xaxis = list("title" = x_lab, color = "white"),
                                               title = self$plot_title
                                             )


                                         },
                                         add_curve = function(x, y){

                                           self$plot <- self$plot %>%
                                             plotly::add_trace(x=x,
                                                               y=y,
                                                               type="scatter",
                                                               mode = "lines+markers")



                           },plot_title = list(
                             "text" = NA,
                             "font" = list(
                               "family" = "sans-serif",
                               "size" = 16,
                               "color" = "white"
                             )
                           ), plot = NA))



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



