% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opt_pnl.R
\name{compute_option_scenarios}
\alias{compute_option_scenarios}
\title{Compute option price scenarios at a given point in time}
\usage{
compute_option_scenarios(scenario_datetime, vol_change = 0.3,
  underlyer_change = 0.1, n_scenarios = 20, multiplier = 100)
}
\arguments{
\item{scenario_datetime}{Datetime at which to compute option price scenarios}

\item{n_scenarios}{Number of scenarios to compute for underlyer volatility}

\item{option_leg}{Option Leg object}

\item{vol_min}{Lower bound of volatility scenarios as a fraction of current volatility}

\item{vol_max}{Upper bound of volatility scenarios as a fraction of current volatility}

\item{underlyer_min}{Minimum underlyer price}

\item{underlyer_max}{Maximum underlyer price}

\item{underlyer_prices}{Vector of underlyer closing prices, matching the option leg prices}

\item{underlyer_margin}{Integer specifying padding at margins of PnL graph}
}
\value{
Matrix of option prices for different underlyer prices and volatility
}
\description{
Compute option price scenarios at a given point in time
}
