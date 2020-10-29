
test_that("Call spread works", {

  testthat::expect_is(strats$`EEM.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})

test_that("Ratio put spread works", {

  testthat::expect_is(strats$`UBER.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})

test_that("Ratio put spread works", {

  testthat::expect_is(strats$`F.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})


test_that("Put spread works", {

  testthat::expect_is(strats$`HYG.2020-06-12`$plot_strategy_pnl(),
                      class = "plotly")


})

test_that("Iron Condor works", {

  testthat::expect_is(strats$`MRK.2020-10-23`$plot_strategy_pnl(),
                      class = "plotly")


})

test_that("Double Calendar spread works", {

  testthat::expect_is(strats$`SPY.2020-10-30`$plot_strategy_pnl(),
                      class = "plotly")

})





