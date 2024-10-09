test_that("Testing a download of a big query if correct", {
  apiCallKolada <- AdvanceRConnectingToAPI$new(kpi=c("N25026"),year=c("2022"))
  
  expect_true(class(apiCallKolada)[1] == "AdvanceRConnectingToAPI")
  expect_true(is.data.frame(apiCallKolada$fetchedData))
  expect_equal(nrow(apiCallKolada$fetchedData),3561)
  expect_equal(apiCallKolada$status,200)
})


test_that("Testing the inputs and outputs of your functions works", {
  expect_equal(AdvanceRConnectingToAPI$new(searchText="föräldrapenning")$url,"http://api.kolada.se/v2/kpi?title=f%C3%B6r%C3%A4ldrapenning")
  expect_equal(AdvanceRConnectingToAPI$new(searchText="parental allowance",sweText=FALSE)$url,"http://api.kolada.se/v2/kpi?title=F%C3%B6r%C3%A4ldrapenning")
})

test_that("Testing AdvanceRConnectingToAPI throws an error with insufficient fields", {
  expect_error(AdvanceRConnectingToAPI$new(kpi=c("N25026")), 
               regexp = "*Either two fields.*or Search Text must be provided.")
  expect_error(AdvanceRConnectingToAPI$new(kpi=c("N25026"),year=c("2022"),municipality=c("1860")), 
               regexp = "*Either two fields.*or Search Text must be provided.")
})
