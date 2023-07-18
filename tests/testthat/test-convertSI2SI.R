test_that("convertSI2SI.R: check site index conversion between species.", {
  library(data.table)
  library(testthat)
  # test warning
  expect_warning(convertSI2SI(spCode_to = "bc",
                              availableSI = "at=10"))
  # test at
  si_final <- round(convertSI2SI(spCode_to = "at",
                                 availableSI = "sw = 30, pl = 40"),
                    2)
  expect_equal(si_final,
               round(-4.768112309  + 1.253446979 * 30, 2))

  si_final <- round(convertSI2SI(spCode_to = "at",
                                 availableSI = "bl = 30, pl = 40"),
                    2)
  expect_equal(si_final,
               round(-7.216706405  + 1.457496490 * 30, 2))

  # test ba
  si_final <- round(convertSI2SI(spCode_to = "ba",
                                 availableSI = "hw = 35, ss = 36"),
                    2)
  expect_equal(si_final,
               round(1.928007878 + 0.789940825 *36, 2))

  si_final <- round(convertSI2SI(spCode_to = "ba",
                                 BECZone = "CDF",
                                 availableSI = "hw = 35, ss = 36, bl = 34.4"),
                    2)
  expect_equal(si_final,
               round(-1.977317550 + 0.986193290 *35, 2))

  si_final <- round(convertSI2SI(spCode_to = "ba",
                                 BECZone = "CDF",
                                 availableSI = "bl = 34.4"),
                    2)
  expect_equal(si_final,
               34.4)

  # test coastal fd
  si_final <- round(convertSI2SI(spCode_to = "Fd",
                                 BECZone = "CDF",
                                 availableSI = "hw = 35, pl = 36"),
                    2)
  expect_equal(si_final,
               round(0.480533930 + 1.112347050 *35, 2))

  # test interior fd
  si_final <- round(convertSI2SI(spCode_to = "Fd",
                                 BECZone = "ESSF",
                                 availableSI = "hw = 35, pl = 36"),
                    2)
  expect_equal(si_final,
               round(0.708411210 + 0.934579440 *36, 2))

})
