test_that("checkLD_remeas.R: the live dead status codes are not correctly checked.", {
  library(data.table)
  library(testthat)
  ## check error
  expect_error(checkLD_remeas(subjectID = "1",
                              measNo = 1:5,
                              LDStatus = c(0, 0, 0, 1, 1),
                              liveCode = 0,
                              deadCode = "D")) # status does not have D
  expect_error(checkLD_remeas(subjectID = "1",
                              measNo = 1:5,
                              LDStatus = c("0", "0", "0", "1", "D"),
                              liveCode = "0",
                              deadCode = "1")) # status does not have 1
  expect_error(checkLD_remeas(subjectID = "1",
                              measNo = c(1:3, 3:5),
                              LDStatus = c("0", "0", "0", "0", "1", "1"),
                              liveCode = "0",
                              deadCode = "1")) # multiple records on measurement 3
  # check the below
  thedata <- rbind(data.table(treeid = "1", measures = 1:5, status = "L"), # all live records
                   data.table(treeid = "2", measures = 1:5, status = "D"), # dead from start
                   data.table(treeid = "3", measures = 1:5, status = c("L", "L", "L", "L", "D")), # dead at last time
                   data.table(treeid = "4", measures = 1:5, status = c("D", "L", "L", "L", "L")), # dead at first time
                   data.table(treeid = "5", measures = 1:5, status = c("L", "L", "L", "D", "D")), # dead at forth time, and remains dead
                   data.table(treeid = "6", measures = 1:5, status = c("L", "L", "D", "L", "D")), # dead at third time, and back to live at forth
                   data.table(treeid = "7", measures = 1:5, status = c("L", "L", "D", "D", "L"))) # dead at third time, and remains dead at 4, changed to live at 5
  output1 <- checkLD_remeas(subjectID = thedata$treeid,
                            measNo =  thedata$measures,
                            LDStatus = thedata$status,
                            liveCode = "L",
                            deadCode = "D")
  thedata <- rbind(data.table(treeid = "1", measures = 1:5, status = 0), # all live records
                   data.table(treeid = "2", measures = 1:5, status = 1), # dead from start
                   data.table(treeid = "3", measures = 1:5, status = c(0, 0, 0, 0, 1)), # dead at last time
                   data.table(treeid = "4", measures = 1:5, status = c(1, 0, 0, 0, 0)), # dead at first time
                   data.table(treeid = "5", measures = 1:5, status = c(0, 0, 0, 1, 1)), # dead at forth time, and remains dead
                   data.table(treeid = "6", measures = 1:5, status = c(0, 0, 1, 0, 1)), # dead at third time, and back to live at forth
                   data.table(treeid = "7", measures = 1:5, status = c(0, 0, 1, 1, 0))) # dead at third time, and remains dead at 4, changed to live at 5
  output2 <- checkLD_remeas(subjectID = thedata$treeid,
                            measNo =  thedata$measures,
                            LDStatus = thedata$status,
                            liveCode = 0,
                            deadCode = 1)
  expect_identical(output1$pass,
                   c(rep(TRUE, 3), FALSE, TRUE, rep(FALSE, 2)))
  expect_identical(output1$pass, output2$pass)

  thedata <- thedata[sample(1:nrow(thedata), nrow(thedata), replace = FALSE),] # using random order
  output3 <- checkLD_remeas(subjectID = thedata$treeid,
                            measNo =  thedata$measures,
                            LDStatus = thedata$status,
                            liveCode = 0,
                            deadCode = 1)
  expect_identical(output1$pass, output3$pass)
})
