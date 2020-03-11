## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  fig.width = 12,
  fig.height = 7,
  out.width = "100%"
)

## ----eval = FALSE-------------------------------------------------------------
#  # install.packages("drat")
#  drat::addRepo("R4EPI")
#  install.packages("apyramid")

## ----eval = FALSE-------------------------------------------------------------
#  # install.packages("remotes")
#  remotes::install_github("R4EPI/apyramid")

## ----load_packages------------------------------------------------------------
library("apyramid")
library("ggplot2")   # load ggplot2 to control plot aesthetics
library("outbreaks") # load the outbreaks package for linelist data
old_theme <- theme_set(theme_classic(base_size = 18))

## ----flu----------------------------------------------------------------------
flu <- outbreaks::fluH7N9_china_2013

# data preparation (create age groups from ages)
autocut <- function(x) {
  cut(x, breaks = pretty(x), right = TRUE, include.lowest = TRUE)
}
flu$age_group <- autocut(as.integer(flu$age))
levels(flu$gender) <- c("Female", "Male")
head(flu)

flup <- age_pyramid(flu, age_group, split_by = gender)
flup

## ----flu2---------------------------------------------------------------------
flup + 
  scale_fill_grey(guide = guide_legend(order = 1)) + 
  theme(text = element_text(size = 18, family = "serif")) + 
  theme(panel.background = element_rect(fill = "#ccffff")) + 
  theme(plot.background = element_rect(fill = "#ffffcc")) + 
  theme(legend.background = element_blank()) +
  labs(
    x       = "Age group (years)",
    y       = "Number of cases",
    fill    = "Gender",
    title   = "136 cases of influenza A H7N9 in China",
    caption = "Source: https://doi.org/10.5061/dryad.2g43n"
  )

## ----flu3---------------------------------------------------------------------
age_pyramid(flu, age_group, split_by = gender, na.rm = FALSE)

## ----us2018-------------------------------------------------------------------
us_labels <- labs(
  x = "Age group", 
  y = "Thousands of people", 
  title = "US Cenus Data 2018",
  caption = "source: https://census.gov/data/tables/2018/demo/age-and-sex/2018-age-sex-composition.html"
)

data(us_2018)
us_2018
p <- age_pyramid(us_2018, age_group = age, split_by = gender, count = count)
p + us_labels

## ----us2018_factor------------------------------------------------------------
data(us_ins_2018) # stratified by gender and health insurance status
data(us_gen_2018) # stratified by gender and generational status
p_ins <- age_pyramid(us_ins_2018, age_group = age, split_by = gender, stack_by = insured, count = count)
p_gen <- age_pyramid(us_gen_2018, age_group = age, split_by = gender, stack_by = generation, count = count)
p_ins + us_labels
p_gen + us_labels

## ----srvyr--------------------------------------------------------------------
library(srvyr, warn.conflicts = FALSE)
data(api, package = "survey")

dstrata <- apistrat %>%
  mutate(apicat = cut(api00, pretty(api00), include.lowest = TRUE, right = TRUE)) %>%
  as_survey_design(strata = stype, weights = pw)
 
age_pyramid(dstrata, apicat, split_by = stype)

theme_set(old_theme)

