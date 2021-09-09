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
                                measTime = testdata_org$measuretime,
                                size = testdata_org$dbh,
                                toleranceMethod = "wrong"))
  expect_error(checkSize_remeas(subjectID = testdata_org$treeid,
                                measTime = testdata_org$measuretime,
                                size = testdata_org$dbh,
                                toleranceAbs = -1))
  expect_error(checkSize_remeas(subjectID = testdata_org$treeid,
                                measTime = testdata_org$measuretime,
                                size = testdata_org$dbh,
                                toleranceRel = -1))
  expect_error(checkSize_remeas(subjectID = testdata_org$treeid,
                                measTime = testdata_org$measuretime,
                                change = "a",
                                size = testdata_org$dbh,
                                toleranceAbs = 0.1))

  testdata <- rbind(testdata_org,
                    data.table(treeid = "100-1", measuretime = 5,
                               dbh = 7))
  ## test error for multiple sizes on one measurement
  expect_error(checkSize_remeas(subjectID = testdata$treeid,
                                measTime = testdata$measuretime,
                                size = testdata$dbh))

  ## check failure due to missing size
  testdata <- data.table::copy(testdata_org)[treeid == "100-2",]
  testdata[treeid == "100-2" & measuretime == 2, dbh := NA]
  output <- checkSize_remeas(subjectID = testdata$treeid,
                                measTime = testdata$measuretime,
                                change = "increase",
                                size = testdata$dbh,
                                toleranceAbs = 0.1)
  expect_equivalent(output,
                   data.table(subjectID = "100-2",
                              measTime = 1:5,
                              size = c(10, NA, 20, 25, 30),
                              pass = c(NA, FALSE, TRUE, TRUE, TRUE),
                              reason = c(NA, "missing size", NA, NA, NA),
                              memo = as.numeric(NA)))


  #########################################
  #### test increase
  #########################################
  ## test all pass scenario for increased sizes and based on tolerance zero
  output1 <- checkSize_remeas(subjectID = testdata_org$treeid,
                              measTime = testdata_org$measuretime,
                              size = testdata_org$dbh)
  output2 <- checkSize_remeas(subjectID = testdata_org$treeid,
                              measTime = testdata_org$measuretime,
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
# break both tolerance
  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             size = testdata$dbh)
  expect_identical(output$pass,
                   c(NA, TRUE, FALSE, TRUE, FALSE, NA, FALSE, rep(TRUE, 3)))
  expect_identical(output$reason,
                   c(NA, NA, "break both tolerance", NA, "break both tolerance", NA,
                     "break both tolerance", rep(NA, 3)))
  rm(output)

  ## test some failure scenarios for increased sizes based on tolerance 1
  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             size = testdata$dbh,
                             toleranceAbs = 1)
  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, TRUE, TRUE, NA, FALSE, rep(TRUE, 3)))
  expect_identical(output$reason,
                   c(rep(NA, 6), "break both tolerance", rep(NA, 3)))
  rm(output)

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             size = testdata$dbh,
                             toleranceMethod = "either",
                             toleranceAbs = 0.1,
                             toleranceRel = 0.01)

  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, TRUE, FALSE, NA, FALSE, rep(TRUE, 3)))
  expect_identical(output$reason,
                   c(NA, NA, NA, NA, "break either tolerance", NA,
                     "break either tolerance", rep(NA, 3)))
  rm(output)

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             size = testdata$dbh,
                             toleranceMethod = "absolute",
                             toleranceAbs = 0.5,
                             toleranceRel = 0.01)

  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, TRUE, TRUE, NA, FALSE, rep(TRUE, 3)))
  expect_identical(output$reason,
                   c(NA, NA, NA, NA, NA, NA,
                     "break absolute tolerance", rep(NA, 3)))
  rm(output)

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             size = testdata$dbh,
                             toleranceMethod = "relative",
                             toleranceRel = 0.05)

  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, TRUE, TRUE, NA, FALSE, rep(TRUE, 3)))
  expect_identical(output$reason,
                   c(NA, NA, NA, NA, NA, NA,
                     "break relative tolerance", rep(NA, 3)))
  rm(output)

  ## test one measurement on one subject
  testdata <- data.table::copy(testdata_org)
  testdata <- testdata[!(treeid == "100-1" & measuretime > 1),]

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
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
                             measTime = testdata_org$measuretime,
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
                             measTime = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh)
  #    subjectID measTime  size  pass          reason
  # 1:     100-1        1 20.00    NA            <NA>
  # 2:     100-1        2 20.01 FALSE break tolerance
  # 3:     100-1        3 12.50  TRUE            <NA>
  # 4:     100-1        4 13.50 FALSE break tolerance
  # 5:     100-1        5  5.00  TRUE            <NA>
  # 6:     100-2        1 30.00    NA            <NA>
  # 7:     100-2        2 25.00  TRUE            <NA>
  # 8:     100-2        3 20.00  TRUE            <NA>
  # 9:     100-2        4 20.50 FALSE break tolerance
  #10:     100-2        5 10.00  TRUE            <NA>

  expect_identical(output$pass,
                   c(NA, FALSE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), FALSE, TRUE))
  expect_identical(output$reason,
                   c(NA, "break both tolerance", NA, "break both tolerance", NA,
                     NA, rep(NA, 2), "break both tolerance", NA))
  rm(output)

  ## test some failure at tolerance of 0.01
  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh,
                             toleranceAbs = 0.01)
  #    subjectID measTime  size  pass          reason
  # 1:     100-1        1 20.00    NA            <NA>
  # 2:     100-1        2 20.01 FALSE break tolerance
  # 3:     100-1        3 12.50  TRUE            <NA>
  # 4:     100-1        4 13.50 FALSE break tolerance
  # 5:     100-1        5  5.00  TRUE            <NA>
  # 6:     100-2        1 30.00    NA            <NA>
  # 7:     100-2        2 25.00  TRUE            <NA>
  # 8:     100-2        3 20.00  TRUE            <NA>
  # 9:     100-2        4 20.50 FALSE break tolerance
  #10:     100-2        5 10.00  TRUE            <NA>
  expect_identical(output$pass,
                   c(NA, FALSE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), FALSE, TRUE))
  expect_identical(output$reason,
                   c(NA, "break both tolerance", NA, "break both tolerance", NA,
                     NA, rep(NA, 2), "break both tolerance", NA))

  rm(output)

  ## test some failure at tolerance of 0.5
  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh,
                             toleranceMethod = "either",
                             toleranceAbs = 0.8,
                             toleranceRel = 0.02)

  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), FALSE, TRUE))
  expect_identical(output$reason,
                   c(NA, NA, NA, "break either tolerance", NA,
                     NA, rep(NA, 2), "break either tolerance", NA))
  rm(output)

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh,
                             toleranceMethod = "both",
                             toleranceAbs = 0.8,
                             toleranceRel = 0.02)

  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), TRUE, TRUE))
  expect_identical(output$reason,
                   c(NA, NA, NA, "break both tolerance", NA,
                     NA, rep(NA, 4)))
  rm(output)

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh,
                             toleranceMethod = "absolute",
                             toleranceAbs = 0.8,
                             toleranceRel = 0.02)

  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), TRUE, TRUE))
  expect_identical(output$reason,
                   c(NA, NA, NA, "break absolute tolerance", NA,
                     NA, rep(NA, 4)))
  rm(output)

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             change = "decrease",
                             size = testdata$dbh,
                             toleranceMethod = "relative",
                             toleranceAbs = 0.8,
                             toleranceRel = 0.02)

  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, FALSE, TRUE,
                     NA, rep(TRUE, 2), FALSE, TRUE))
  expect_identical(output$reason,
                   c(NA, NA, NA, "break relative tolerance", NA,
                     NA, rep(NA, 2), "break relative tolerance", NA))
  rm(output)





  ## test the extreme change rate
  testdata <- rbind(data.table(treeid = "100-1", measuretime = 1:5,
                                   dbh = seq(5, 20, length.out = 5)))
  # current change rate is 3.75
  testdata[measuretime == 2, dbh := 9] # increment is 4 from previous measurement
  testdata[measuretime == 4, dbh := 16.6] # increment is 4.1 from previous measurement

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             size = testdata$dbh,
                             maxChangeRate = 4)
  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, FALSE, TRUE))
  expect_identical(output$reason,
                   c(NA, NA, NA, "abnormal change rate", NA))
  rm(testdata, output)

  testdata <- rbind(data.table(treeid = "100-1", measuretime = 1:5,
                                   dbh = seq(20, 5, length.out = 5)))
  # current change rate is 3.75 increased
  testdata[measuretime == 2, dbh := 16] # decreased by 4 from previous measurement
  testdata[measuretime == 4, dbh := 8.4] # increment is 4.1 from previous measurement

  output <- checkSize_remeas(subjectID = testdata$treeid,
                             measTime = testdata$measuretime,
                             size = testdata$dbh,
                             change = "decrease",
                             maxChangeRate = 4)
  expect_identical(output$pass,
                   c(NA, TRUE, TRUE, FALSE, TRUE))
  expect_identical(output$reason,
                   c(NA, NA, NA, "abnormal change rate", NA))
  rm(testdata, output)
  })
