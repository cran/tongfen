## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
	message = FALSE,
	warning = FALSE,
	collapse = TRUE,
  eval = nzchar(Sys.getenv("COMPILE_VIG")),
	comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tongfen)
library(ggplot2)

## -----------------------------------------------------------------------------
station_buffers <- cancensus::COV_SKYTRAIN_STATIONS

## -----------------------------------------------------------------------------
meta <- meta_for_ca_census_vectors(c(limat = "v_CA16_2540"))
station_data <- tongfen_estimate_ca_census(station_buffers,meta,level = "DA",intersection_level = "CT", 
                                           quiet=TRUE, na.rm = TRUE)

## -----------------------------------------------------------------------------
cov_background <- cancensus::get_census("CA16",regions=list(CSD="5915022"),geo_format = "sf", quiet=TRUE)

ggplot(station_data) +
  geom_sf(data=cov_background) +
  geom_sf(aes(fill=limat/100),alpha=0.7) +
  scale_fill_viridis_c(labels=scales::percent) +
  coord_sf(datum=NA) +
  labs(title="Share of people in low icome in Vancouver Skytrain station neighbourhoods",
       fill="Share of people\nin LIM-AT",
       caption="StatCan Census 2016")

