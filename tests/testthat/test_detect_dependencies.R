
test_that("dependencies are detected", {

	load("expected_dependencies.RData")

        deps <- detect_dependencies(system.file("test_project",
                                                package = "easyMake"))

	expect_equal(deps,
                     as.data.frame(expected_dependencies))
})
