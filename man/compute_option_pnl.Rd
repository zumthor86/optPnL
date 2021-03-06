% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opt_pnl.R
\name{compute_option_pnl}
\alias{compute_option_pnl}
\title{Compute option leg pnl before accounting for premiums}
\usage{
compute_option_pnl(underlyer_space, multiplier = 100, days_to_exp = 0,
  r = 0.005, b = 0, implied_vol = self$implied_vol)
}
\arguments{
\item{multiplier}{Number of underlyer instrument to which option holder is entitled, integer}

\item{days_to_exp}{Days to expiration, double}

\item{r}{Annualized rate of interest, double}

\item{b}{Annualized cost of carry, double}

\item{anchor}{Centre point of the pnl range, double}

\item{sigma}{Annualized volatility/standard deviation, double}
}
\value{
Matrix containing pnl for different levels of the underlyer
}
\description{
Compute option leg pnl before accounting for premiums
}
