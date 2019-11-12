test_that("checkMissing_remeas.R: the missing measurements are not correctly checked.", {
  library(data.table)
  library(testthat)
  ## check error
  expect_error(checkMissing_remeas(subjectID = "1",
                                   measNo = c(1, 2, 2, 3), # duplicate measurement 2
                                   intendedMeasNo = 1:5,
                                   deadCode = NULL)) # no dead mode
  expect_error(checkMissing_remeas(subjectID = "1",
                                   measNo = c(1, 2, 2, 3), # duplicate measurement 2
                                   intendedMeasNo = 1:5,
                                   deadCode = "D", # dead mode
                                   LDStatus = c("0", "0", "1", "D")))

  expect_error(checkMissing_remeas(subjectID = "1",
                                   measNo = c(1:5),
                                   intendedMeasNo = 1:10,
                                   deadCode = "1",
                                   LDStatus = c("0", "0", "1", "0", "1"))) # can not pass dead check
  #             live, live, dead, live, dead
  # check the pass scenario with dead mode off
  thedata <- rbind(data.table(treeid = "1", measures = 1:5), # all 5 records
                   data.table(treeid = "2", measures = 3:5)) # last three measurements

  output <- checkMissing_remeas(subjectID = thedata$treeid,
                                measNo =  thedata$measures,
                                intendedMeasNo = 1:5,
                                deadCode = NULL)
  expect_equal(output,
               data.table(subjectID = c("1", "2"),
                          pass = TRUE,
                          missingMeasNo = as.numeric(NA),
                          missingReason = as.character(NA)))
  rm(output)
  # check the pass scenario with dead mode on
  thedata <- rbind(data.table(treeid = "1",
                              measures = 1:5,
                              ld = 0), # all live records
                   data.table(treeid = "2",
                              measures = 1:3, # first 3 measurements
                              ld = c(0, 0, 1)), # dead at 3
                   data.table(treeid = "3",
                              measures = 3:5, # last 3 measurements
                              ld = c(0, 0, 1)), # dead at 5
                   data.table(treeid = "4",
                              measures = 3:5, # last 3 measurements
                              ld = c(0, 1, 1)), # dead at 4 with 5 recorded as well
                   data.table(treeid = "5",
                              measures = 3:4, # middle 2 measurements
                              ld = c(0, 1))) # dead at 4

  output <- checkMissing_remeas(subjectID = thedata$treeid,
                                measNo =  thedata$measures,
                                intendedMeasNo = 1:5,
                                deadCode = 1,
                                LDStatus = thedata$ld)
  expect_equal(output,
               data.table(subjectID = c("1", "2", "3", "4", "5"),
                          pass = TRUE,
                          missingMeasNo = as.numeric(NA),
                          missingReason = as.character(NA)))
  rm(output)


  ## check some failure scenarios with dead mode off,
  ## check did not measure to the last intended measurement
  output <- checkMissing_remeas(subjectID = thedata$treeid,
                                measNo =  thedata$measures,
                                intendedMeasNo = 1:5,
                                deadCode = NULL)
  expect_equal(output,
               data.table(subjectID = c("1", "2", "2", "3", "4", "5"),
                          pass = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE),
                          missingMeasNo = c(NA, 4, 5, NA, NA, 5),
                          missingReason = c(NA, "missing tail", "missing tail", NA,
                                            NA, "missing tail")))
  #    subjectID  pass missingMeasNo missingReason
  # 1:         1  TRUE            NA          <NA>
  # 2:         2 FALSE             4  missing tail
  # 3:         2 FALSE             5  missing tail
  # 4:         3  TRUE            NA          <NA>
  # 5:         4  TRUE            NA          <NA>
  # 6:         5 FALSE             5  missing tail
    rm(output)



  ## check failure cases with dead mode on
  thedata1 <- thedata[!(treeid == "1" & measures == 3),] # remove measurement 3 of tree 1
  thedata1 <- thedata1[!(treeid == "2" & measures == 2),] # remove measurement 2 of tree 2
  thedata1 <- thedata1[!(treeid == "3" & measures == 4),] # remove measurement 4 of tree 3
  output1 <- checkMissing_remeas(subjectID = thedata1$treeid,
                                 measNo =  thedata1$measures,
                                 intendedMeasNo = 1:5,
                                 deadCode = 1,
                                 LDStatus = thedata1$ld)
  expect_equal(output1,
               data.table(subjectID = c("1", "2", "3", "4", "5"),
                          pass = c(FALSE, FALSE, FALSE, TRUE, TRUE),
                          missingMeasNo = c(3, 2, 4, NA, NA),
                          missingReason = c(rep("missing middle", 3), rep(NA, 2))))
  thedata1[, ld := as.character(ld)]
  thedata1[treeid == 1 & measures == 2, ld := "test1"]
  thedata1[treeid == 2 & measures == 1, ld := "test2"]
  thedata1[treeid == 5 & measures == 3, ld := "test3"]
  output2 <- checkMissing_remeas(subjectID = thedata1$treeid,
                                 measNo =  thedata1$measures,
                                 intendedMeasNo = 1:5,
                                 deadCode = "1",
                                 LDStatus = thedata1$ld)
  expect_identical(output1, output2)
  rm(output1, output2, thedata1)

  ## check missing middle measurements with dead mode off
  thedata <- thedata[treeid == "1" & measures != 2,] # missing 2th measurement
  thedata <- thedata[measures != 5,] # missing 5th measurement
  output <- checkMissing_remeas(subjectID = thedata$treeid,
                                measNo =  thedata$measures,
                                intendedMeasNo = 1:5,
                                deadCode = NULL)
  expect_equal(output,
               data.table(subjectID = "1",
                          pass = c(FALSE, FALSE),
                          missingMeasNo = c(2, 5),
                          missingReason = c("missing middle", "missing tail")))

  rm(output)
})
