
test_that("Strat1 works", {

  testthat::expect_is(strats$`EEM.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})

test_that("Strat2 works", {

  testthat::expect_is(strats$`UBER.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})

test_that("Strat3 works", {

  testthat::expect_is(strats$`F.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})


test_that("Strat4 works", {

  testthat::expect_is(strats$`HYG.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})

test_that("Strat5 works", {

  testthat::expect_is(strats$`GE.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})





