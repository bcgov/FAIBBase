test_that("checkSize_remeas.R: check the unreasonable remeasured sizes for a given subject.", {
  library(data.table)
  library(testthat)
  testdata_org <- rbind(data.table(treeid = "100-1", measuretime = 1:5,
                                   dbh = seq(5, 20, length.out = 5)),
                        data.table(treeid = "100-2", measuretime = 1:5,
                                   dbh = seq(10, 30, length.out = 5)))
  ###################################
  ## test error messege for tolerance
  ###################################
  expect_error(checkSize_remeas(subjectID = testdata_org$treeid,
                                measNo = testdata_org$measuretime,
                                size = testdata_org$dbh,
                                tolerance = -0.1))
  expect_error(checkSize_remeas(subjectID = testdata_org$treeid,
                                measNo = testdata_org$measuretime,
                                change = "a",
                                size = testdata_org$dbh,
                                tolerance = 0.1))

  testdata <- data.table::copy(testdata_org)
  testdata[treeid == "100-2" & measuretime == 2, dbh := NA]
  expect_error(checkSize_remeas(subjectID = testdata$treeid,
                                measNo = testdata$measuretime,
                                change = "increase",
                                size = testdata$dbh,
                                tolerance = 0.1))

  testdata <- rbind(testdata_org,
                    data.table(treeid = "100-1", measuretime = 5,
                               dbh = 7))
  ## test error for multiple sizes on one measurement
  expect_error(checkSize_remeas(subjectID = testdata$treeid,
                                measNo = testdata$measuretime,
                                size = testdata$dbh))

  #########################################
  #### test increase
  #########################################
  ## test all pass scenario for increased sizes and based on tolerance zero
  output1 <- checkSize_remeas(subjectID = testdata_org$treeid,
                              measNo = testdata_org$measuretime,
                              size = testdata_org$dbh)
  output2 <- checkSize_remeas(subjectID = testdata_org$treeid,
                              measNo = testdata_org$measuretime,
                              change = "increase",
                              size = testdata_org$dbh)
  expect_identical(output1$pass,
                   c(NA, rep(TRUE, 4), NA, rep(TRUE, 4)))
  expect_identical(output1, output2)
  rm(output1, output2)

  ## test some failure scenarios for increased sizes based on tolerance zero
  testdata <- data.table::copy(testdata_org)
  testdata[treeid == "100-1" & measuretime == 3, dbh := 8.75] # zero from previous measurement
  testdata[treeid == "100-1" & measuretime == 5, dbh := 16] # decreased 0.25
  testdata[treeid == "100-2" & measuretime == 2, dbh := 9] # decreased 1

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measNo = testdata$measuretime,
                             size = testdata$dbh)
  expect_identical(output$pass,
                   c(NA, TRUE, FALSE, TRUE, FALSE, NA, FALSE, rep(TRUE, 3)))
  rm(output)

  ## test some failure scenarios for increased sizes based on tolerance 1
  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measNo = testdata$measuretime,
                             size = testdata$dbh,
                             tolerance = 1)
  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, TRUE, TRUE, NA, FALSE, rep(TRUE, 3)))
  rm(output)

  ## test one measurement on one subject
  testdata <- data.table::copy(testdata_org)
  testdata <- testdata[!(treeid == "100-1" & measuretime > 1),]

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measNo = testdata$measuretime,
                             size = testdata$dbh)
  expect_identical(output$pass,
                   c(NA, NA, rep(TRUE, 4)))
  rm(output, testdata, testdata_org)

  ##############################
  ## test decrease
  ##############################
  testdata_org <- rbind(data.table(treeid = "100-1", measuretime = 5:1,
                                   dbh = seq(5, 20, length.out = 5)),
                        data.table(treeid = "100-2", measuretime = 5:1,
                                   dbh = seq(10, 30, length.out = 5)))
  ## test all passed
  output <- checkSize_remeas(subjectID = testdata_org$treeid,
                             measNo = testdata_org$measuretime,
                             change = "decrease",
                             size = testdata_org$dbh)
  expect_identical(output$pass,
                   c(NA, rep(TRUE, 4), NA, rep(TRUE, 4)))
  rm(output)

  ## test some failure at tolerance zero
  testdata <- data.table::copy(testdata_org)
  testdata[treeid == "100-1" & measuretime == 2,
           dbh := 20.01] # increased 0.01
  testdata[treeid == "100-1" & measuretime == 4,
           dbh := 13.50] # increased 1
  testdata[treeid == "100-2" & measuretime == 4,
           dbh := 20.5] # increased 0.5
  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measNo = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh)
  expect_identical(output$pass,
                   c(NA, FALSE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), FALSE, TRUE))
  rm(output)

  ## test some failure at tolerance of 0.01
  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measNo = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh,
                             tolerance = 0.01)
  #    subjectID measNo  size  pass
  # 1:     100-1      1 20.00    NA
  # 2:     100-1      2 20.01 FALSE
  # 3:     100-1      3 12.50  TRUE
  # 4:     100-1      4 13.50 FALSE
  # 5:     100-1      5  5.00  TRUE
  # 6:     100-2      1 30.00    NA
  # 7:     100-2      2 25.00  TRUE
  # 8:     100-2      3 20.00  TRUE
  # 9:     100-2      4 20.50 FALSE
  # 10:     100-2      5 10.00  TRUE
  expect_identical(output$pass,
                   c(NA, FALSE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), FALSE, TRUE))
  rm(output)

  ## test some failure at tolerance of 0.5
  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measNo = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh,
                             tolerance = 0.5)
  #    subjectID measNo  size  pass
  # 1:     100-1      1 20.00    NA
  # 2:     100-1      2 20.01  TRUE
  # 3:     100-1      3 12.50  TRUE
  # 4:     100-1      4 13.50 FALSE
  # 5:     100-1      5  5.00  TRUE
  # 6:     100-2      1 30.00    NA
  # 7:     100-2      2 25.00  TRUE
  # 8:     100-2      3 20.00  TRUE
  # 9:     100-2      4 20.50 FALSE
  # 10:     100-2      5 10.00  TRUE
  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), FALSE, TRUE))
  rm(output)
  })
