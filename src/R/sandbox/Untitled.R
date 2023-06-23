
scs_items <- d2 |> 
  dplyr::select(all_of(starts_with("scs_")))

psych::alpha(scs_items)

# raw_alpha = 0.8

pos_id <- c(5, 12, 19, 23, 26, 3, 7, 10, 15,  9, 14, 17, 22)
neg_id <- c(1, 8, 11, 16, 21, 4, 13, 18, 25, 2, 6, 20, 24)

pos_items <- scs_items[, pos_id]
neg_items <- scs_items[, neg_id]

psych::alpha(pos_items)
# raw_alpha 0.87

psych::alpha(neg_items)
# raw_alpha 0.92

var(rowSums(scs_items)) * 0.8
# 133.0744

var(rowSums(pos_items)) * 0.87
# 73.56472

var(rowSums(neg_items)) * 0.92
# 134.3864

cov(rowSums(pos_items), rowSums(neg_items))
# -32.14314

(41.42^2) / (73.56 * 133.07)
# 0.1752663

(102.24^2) / (134.39 * 133.07)
# 0.5845135



library(ltm)
fit1p <- grm(pos_items, constrained = TRUE)
fit2p <- grm(pos_items, constrained = FALSE)
anova(fit1p,fit2p)

plot(fit2p, lwd = 2, cex = 0.8, legend = TRUE, cx = "topright",
     xlab = "SC", cex.main = 1, cex.lab = 1, cex.axis = 1)

plot(fit2p, type = "IIC", lwd = 2,
     cex = 0.8, legend = TRUE,
     cx = "topleft", xlab = "SC P", cex.main = 1, cex.lab = 1, cex.axis = 1)

plot(fit2p, type = "IIC", items = 0,
     lwd = 2, xlab = "P", cex.main = 1, cex.lab = 1, cex.axis = 1)


fit1n <- grm(neg_items, constrained = TRUE)
fit2n <- grm(neg_items, constrained = FALSE)
anova(fit1n,fit2n)

plot(fit2n, lwd = 2, cex = 0.8, legend = TRUE, cx = "topright",
     xlab = "SC", cex.main = 1, cex.lab = 1, cex.axis = 1)

plot(fit2n, type = "IIC", lwd = 2,
     cex = 0.8, legend = TRUE,
     cx = "topleft", xlab = "SC N", cex.main = 1, cex.lab = 1, cex.axis = 1)

plot(fit2n, type = "IIC", items = 0,
     lwd = 2, xlab = "N", cex.main = 1, cex.lab = 1, cex.axis = 1)

unidimTest(grm(neg_items, constrained = FALSE))
