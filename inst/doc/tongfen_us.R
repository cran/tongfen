## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
	message = FALSE,
	warning = FALSE,
	collapse = TRUE,
  eval = nzchar(Sys.getenv("COMPILE_VIG")),
	comment = "#>"
)

## -----------------------------------------------------------------------------
library(tongfen)
library(dplyr)
library(ggplot2)
#library(mountainmathHelpers)

## -----------------------------------------------------------------------------
variables=c(population="H011001",households="H013001")

meta <- c(2000,2010) %>%
  lapply(function(year){
      v <- variables %>% setNames(paste0(names(.),"_",year))
      meta_for_additive_variables(paste0("dec",year),v)
    }) %>%
  bind_rows()
meta

## ----results='hide'-----------------------------------------------------------
census_data <- get_tongfen_us_census(regions = list(state="CA"), meta=meta, level="tract") %>%
  mutate(change=population_2010/households_2010-population_2000/households_2000) 

## -----------------------------------------------------------------------------
census_data %>% names()

## -----------------------------------------------------------------------------
census_data %>%
  mutate(c=cut(change,c(-Inf,-0.5,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.5,Inf))) %>%
  ggplot() +
  geom_sf(aes(fill=c), size=0.05) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title="Bay area change in average household size 2000-2010", fill=NULL) +
  #geom_water() + geom_roads() +
  coord_sf(datum=NA,xlim=c(-122.6,-121.7),ylim=c(37.2,37.9))

