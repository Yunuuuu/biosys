testthat::test_that("`kraken_tools()` works as expected", {
    for (script in KrakenToolsScripts) {
        kraken_tools(script, help = TRUE)
    }
})
