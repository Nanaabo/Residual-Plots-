# ---------------------------------------------------------------
# Residual diagnostics: all four plots in ONE figure (base R graphics)
#   A  Histogram of residuals + normal curve
#   B  Q-Q plot
#   C  Normal P-P plot
#   D  Standardised residuals vs fitted values
# ---------------------------------------------------------------

# ---- 1. Data and model -----------------------------------------
# Simulated example: replace `dat` and the formula with your own model,
# e.g. fit <- lm(Performance ~ Expert + FormalAuth + PersonalModel + Facilitator, data = mydata)
set.seed(123)
n   <- 144
dat <- data.frame(x1 = runif(n, 2, 12), x2 = rnorm(n))
err <- -rgamma(n, shape = 6, scale = 0.1) + 6 * 0.1 + rnorm(n, 0, 0.15)
dat$y <- 1 + 0.9 * dat$x1 + 0.3 * dat$x2 + err

fit  <- lm(y ~ x1 + x2, data = dat)
res  <- residuals(fit)
sres <- rstandard(fit)
fv   <- fitted(fit)
dv   <- all.vars(formula(fit))[1]

# ---- 2. Colours ------------------------------------------------
col_main   <- "blue3"
col_border <- "firebrick4"   # bar/point outlines
col_line   <- "firebrick"       # reference lines
col_smth   <- "black"     
col_grid   <- "white"

# ---- 3. Plotting function (2 x 2 layout) -----------------------
draw_diagnostics <- function() {
  op <- par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 3, 0),
            las = 1, cex.main = 1, font.main = 3)
  on.exit(par(op))

  # A: histogram + normal curve
  hist(res, breaks = 30, freq = FALSE,
       col = adjustcolor(col_main, 0.65), border = "white",
       main = "Histogram of residuals", xlab = "Residuals", ylab = "Density")
  curve(dnorm(x, mean(res), sd(res)), add = TRUE, col = col_line, lwd = 2.5)
  legend("topright", bty = "n", cex = 0.85,
         legend = c("Residuals", "Normal distribution"),
         fill   = c(adjustcolor(col_main, 0.65), NA), border = c("white", NA),
         lty    = c(NA, 1), lwd = c(NA, 2.5), col = c(NA, col_line),
         merge = TRUE)
  mtext("A", side = 3, adj = 0, line = 1, font = 2, cex = 1.2)

  # B: Q-Q plot
  qqnorm(res, pch = 19, cex = 0.7, col = adjustcolor(col_main, 0.6),
         main = "Q-Q plot of residuals",
         xlab = "Theoretical quantiles", ylab = "Sample quantiles")
  grid(col = col_grid, lty = 1)
  qqline(res, col = col_line, lwd = 2.5)
  mtext("B", side = 3, adj = 0, line = 1, font = 2, cex = 1.2)

  # C: normal P-P plot
  z   <- sort(sres)
  obs <- ppoints(length(z))
  exp <- pnorm(z, mean(z), sd(z))
  plot(obs, exp, pch = 1, cex = 0.8, col = col_main,
       xlim = c(0, 1), ylim = c(0, 1),
       main = paste0("Normal P-P plot of standardised residuals\nDependent variable: ", dv),
       xlab = "Observed cum prob", ylab = "Expected cum prob")
  grid(col = col_grid, lty = 1)
  abline(0, 1, col = col_line, lwd = 2.5)
  mtext("C", side = 3, adj = 0, line = 1, font = 2, cex = 1.2)

  # D: standardised residuals vs fitted values
  plot(fv, sres, pch = 19, cex = 0.7, col = adjustcolor(col_main, 0.6),
       main = "Residuals vs fitted values",
       xlab = "Predicted values", ylab = "Standardised residuals")
  grid(col = col_grid, lty = 1)
  abline(h = 0, col = col_line, lwd = 2.5)
  lines(lowess(fv, sres), col = col_smth, lwd = 2.5)
  mtext("D", side = 3, adj = 0, line = 1, font = 2, cex = 1.2)

  mtext("Regression diagnostics: a closer look at the residuals",
        outer = TRUE, line = 0.5, font = 2, cex = 1.4)
}

# ---- 4. Show on screen and save to file -------------------------
draw_diagnostics()

png("residual_diagnostics.png", width = 3600, height = 3000, res = 300)
draw_diagnostics()
dev.off()
