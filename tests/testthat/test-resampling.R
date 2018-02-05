context("resampling")

test_that("preperiodogram resampling works", {
resample <- zeitgebr:::resample


a <- c(1.11,2,3,4,5,6,7,8,9,10, 13,14,15,18, 20)
x <- rnorm(length(a))
data <- behavr::behavr(data.table(id=1, t=a, x=x, key="id"), metadata = data.table(id=1, test="A", key="id"))


t_half <- resample(data, "x", 2)
t_one <- resample(data, "x", 1)
t_three <- resample(data, "x", 1/3)

expect_identical(unique(diff(t_half[,t])), 1/2)
expect_identical(unique(diff(t_one[,t])), 1)
expect_identical(unique(diff(t_three[,t])), 3)

})



