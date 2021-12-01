test_that("f01a works", {
  expect_equal(
    f01a(example_data_01()),
    7)
})


test_that("f01b slider works", {
  expect_equal(
    f01b_slider(example_data_01()),
    5)
})

test_that("f01b windowed works", {
  expect_equal(
    f01b_windowed(example_data_01()),
    5)
})
