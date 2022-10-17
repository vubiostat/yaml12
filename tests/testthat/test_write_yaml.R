context("write_yaml")

test_that("output_is_written_to_a_file_when_a_filename_is_specified",
{
  filename <- tempfile()
  write_yaml(1:3, filename)
  output <- readLines(filename)
  unlink(filename)
  expect_equal(c("- 1", "- 2", "- 3"), output)
})
