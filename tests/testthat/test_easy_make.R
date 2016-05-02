test_that("Makefile Builds", {
	testthat::skip_on_cran()
	expected <- readLines("expected_Makefile")
	wd <- getwd()

	setwd( gsub("tests/testthat", "inst/test_project", wd))
	easy_make(
		detect_dependencies()
	)
	actual <- readLines("Makefile")
	setwd(wd)

	expect_equal(
		expected,
		actual
	)
}
)
