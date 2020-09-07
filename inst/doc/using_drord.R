## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(drord)

# load data
data(covid19)

# look at first 3 rows
head(covid19, 3)

## ------------------------------------------------------------------------
(fit1 <- drord(out = covid19$out, treat = covid19$treat, 
               covar = covid19[ , "age_grp", drop = FALSE]))

## ----cdf-plot, fig.width = 4, fig.cap="Plot of treatment-specific CDF. Black bars are pointwise 95% confidence intervals; gray bars are 95% simultaneous confidence bands."----
# plot of CDF
cdf_plot <- plot(fit1, dist = "cdf", 
                 treat_labels = c("Treatment", "Control"),
                 out_labels = c("Death", "Death or intubation"))
cdf_plot$plot + ggsci::scale_fill_nejm()

## ----pmf-plot, fig.width = 5, fig.cap="Plot of treatment-specific PMF. Black bars are pointwise 95% confidence intervals; gray bars are 95% simultaneous confidence bands."----
# plot of PMF
pmf_plot <- plot(fit1, dist = "pmf",
                 treat_labels = c("Treatment", "Control"),
                 out_labels = c("Death", "Intubation", "None"))
pmf_plot$plot + ggsci::scale_fill_nejm()    

## ----pmf-plot-with-text, fig.width = 5, fig.cap="Plot of treatment-specific PMF with text added."----
pmf_plot$plot + 
  ggsci::scale_fill_nejm() + 
  ggplot2::geom_text(ggplot2::aes(y = 0, 
                     label = sprintf("%1.2f", Proportion)), 
                     position = ggplot2::position_dodge(0.9),
                     color = "white", 
                     vjust = "bottom")

## ------------------------------------------------------------------------
(fit2 <- drord(out = covid19$out, treat = covid19$treat, 
               covar = covid19[ , "age_grp", drop = FALSE],
               out_form = "factor(age_grp)",
               treat_form = "factor(age_grp)"))

## ------------------------------------------------------------------------
# age_grp treated as numeric in fit1
fit1$out_mod$treat1

# age_grp treated as factor in fit2
fit2$out_mod$treat1

## ------------------------------------------------------------------------
# use vglm to fit model instead
fit3 <- drord(out = covid19$out, treat = covid19$treat, 
              covar = covid19[ , "age_grp", drop = FALSE],
              out_model = "vglm")

# view model output
fit3$out_mod$treat1

## ------------------------------------------------------------------------
(fit4 <- drord(out = covid19$out, treat = covid19$treat, 
               covar = covid19[ , "age_grp", drop = FALSE],
               ci = "bca", nboot = 20, # use more bootstrap samples in practice!!!
               param = "mann_whitney", # only compute mann-whitney estimator
               est_dist = FALSE)) # save time by not computing CIs for CDF/PMF

## ------------------------------------------------------------------------
(fit5 <- drord(out = covid19$out, treat = covid19$treat, 
               covar = covid19[ , "age_grp", drop = FALSE],
               stratify = TRUE))

## ------------------------------------------------------------------------
(fit6 <- drord(out = covid19$out, treat = covid19$treat, 
               covar = covid19[ , "age_grp", drop = FALSE],
               stratify = TRUE, out_form = "1",
               param = "weighted_mean"))

# compare to sample means
(samp_mean_treat1 <- mean(covid19$out[covid19$treat == 1]))
(samp_mean_treat0 <- mean(covid19$out[covid19$treat == 0]))

## ------------------------------------------------------------------------
# missingness probability
miss_out_prob <- plogis(-2 + as.numeric(covid19$age_grp < 5))
miss_out <- rbinom(nrow(covid19), 1, miss_out_prob) == 1
out_with_miss <- covid19$out
out_with_miss[miss_out] <- NA

# correctly model missingness
(fit7 <- drord(out = out_with_miss, treat = covid19$treat, 
               covar = covid19[ , "age_grp", drop = FALSE], 
               treat_form = "I(age_grp < 5)"))

## ------------------------------------------------------------------------
# collapse categories to make a binary outcome
covid19_binary_out <- as.numeric(covid19$out == 3)

# call drord, only requesting weighted mean with equal 
# weights; which in this case equals the risk difference
(fit8 <- drord(out = covid19_binary_out, treat = covid19$treat,
               covar = covid19[ , "age_grp", drop = FALSE],
               param = "weighted_mean",
               est_dist = FALSE)) # must be FALSE to run

# confirm that glm was used for outcome model
class(fit8$out_mod$treat1)

## ------------------------------------------------------------------------
sessionInfo()

