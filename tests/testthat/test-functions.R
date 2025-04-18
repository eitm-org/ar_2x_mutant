require(testthat)
require(here)
source(here("R", "functions.R"))


test_that("function returns a string to a local directory that exists", {
  expect(dir.exists(dropbox_downloader("https://www.dropbox.com/scl/fo/sfi0emihuw9hrrpgga37m/ACcWHePD-ryJIKUMcb1QLho?rlkey=jteaorvj3b929yzeupcqfcdzt&dl=0")),
         "your function returned a directory that does not exist")
})

#TODO: add tests for:
#   - can_it_plate
#   - xl2plater
#   - make_concs_num