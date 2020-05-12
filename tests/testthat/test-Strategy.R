test_that("multiplication works", {

  l1 <-  Option_Leg$new(strike = 200,
                        type = "C",
                        underlyer = "BABA",
                        expiry = "2020-04-24",
                        price = 14.4)

  l2 <-  Option_Leg$new(strike = 227.5,
                        type = "C",
                        underlyer = "BABA",
                        expiry = "2020-04-24",
                        price = 0.58)

  strat <- Option_Strategy$new(legs = list(l1, l2), positions = c(-1,1))

  testthat::expect_is(strat$plot_strategy_pnl(), class = "plotly")


})



