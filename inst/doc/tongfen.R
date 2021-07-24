## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
#	message = FALSE,
#	warning = FALSE,
	collapse = TRUE,
  eval = nzchar(Sys.getenv("COMPILE_VIG")),
	comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tongfen)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cancensus)

## -----------------------------------------------------------------------------
vsb_regions <- list(CSD=c("5915022","5915803"),
                    CT=c("9330069.01","9330069.02","9330069.00"))

geo_identifiers <- c()
years <- seq(2001,2016,5)
geo_identifiers <- paste0("GeoUIDCA",substr(as.character(years),3,4))
data <- years %>% 
  lapply(function(year){
  dataset <- paste0("CA",substr(as.character(year),3,4))
  uid_label <- paste0("GeoUID",dataset)
  get_census(dataset, regions=vsb_regions, geo_format = 'sf', level="CT", quiet=TRUE) %>%
    sf::st_sf() %>%
    rename(!!as.name(uid_label):=GeoUID) %>%
    mutate(Year=year)
}) %>% setNames(years)

## -----------------------------------------------------------------------------
data %>%
  bind_rows() %>%
  ggplot() +
  geom_sf(fill="steelblue",colour="brown") +
  coord_sf(datum=NA) +
  facet_wrap("Year") +
  labs(title="Vancouver census tracts",caption="StatCan Census 2001-2016")

## -----------------------------------------------------------------------------
correspondence <- estimate_tongfen_correspondence(data, geo_identifiers,
                                                   tolerance=200, computation_crs=3347)
head(correspondence)

## -----------------------------------------------------------------------------
tongfen_area_check <- check_tongfen_areas(data,correspondence)

tongfen_area_check %>% 
  filter(max_log_ratio>0.1)

## -----------------------------------------------------------------------------
mismatched_tongfen_ids <- tongfen_area_check %>%
  filter(max_log_ratio>0.1) %>% 
  pull(TongfenID)
mismatch_correspondence <- correspondence %>% 
  filter(TongfenID %in% mismatched_tongfen_ids)


c(2001,2016) %>% 
  lapply(function(year){
    tongfen_aggregate(data,mismatch_correspondence,base_geo = year) %>%
      mutate(Year=year)
  }) %>%
  bind_rows() %>%
  ggplot() +
  geom_sf(data=sf::st_union(data[[4]])) +
  geom_sf(fill="steelblue",colour="brown") +
  coord_sf(datum=NA) +
  facet_wrap("Year") +
  labs(title="Tongfen area mismatch check",caption="StatCan Census 2001-2016")

## -----------------------------------------------------------------------------
years %>% 
  lapply(function(year){
    tongfen_aggregate(data,correspondence,base_geo = year) %>%
      mutate(Year=year)
  }) %>%
  bind_rows() %>%
  ggplot() +
  geom_sf(fill="steelblue",colour="brown") +
  coord_sf(datum=NA) +
  facet_wrap("Year") +
  labs(title="Tongfen aggregates visual inspection",caption="StatCan Census 2001-2016")

## -----------------------------------------------------------------------------
meta <- meta_for_additive_variables(years,"Population")
meta

## -----------------------------------------------------------------------------
breaks = c(-0.15,-0.1,-0.075,-0.05,-0.025,0,0.025,0.05,0.1,0.2,0.3)
labels = c("-15% to -10%","-10% to -7.5%","-7.5% to -5%","-5% to -2.5%","-2.5% to 0%","0% to 2.5%","2.5% to 5%","5% to 10%","10% to 20%","20% to 30%")
colors <- RColorBrewer::brewer.pal(10,"PiYG")

compute_population_change_metrics <- function(data) {
 geometric_average <- function(x,n){sign(x) * (exp(log(1+abs(x))/n)-1)}
 data %>%
  mutate(`2001 - 2006`=geometric_average((`Population_2006`-`Population_2001`)/`Population_2001`,5),
         `2006 - 2011`=geometric_average((`Population_2011`-`Population_2006`)/`Population_2006`,5),
         `2011 - 2016`=geometric_average((`Population_2016`-`Population_2011`)/`Population_2011`,5),
         `2001 - 2016`=geometric_average((`Population_2016`-`Population_2001`)/`Population_2001`,15)) %>%
  gather(key="Period",value="Population Change",c("2001 - 2006","2006 - 2011","2011 - 2016","2001 - 2016")) %>%
  mutate(Period=factor(Period,levels=c("2001 - 2006","2006 - 2011","2011 - 2016","2001 - 2016"))) %>%
   mutate(c=cut(`Population Change`,breaks=breaks, labels=labels))
}

## -----------------------------------------------------------------------------
plot_data <- tongfen_aggregate(data,correspondence,meta=meta,base_geo = "2001")  %>%
  compute_population_change_metrics()

ggplot(plot_data,aes(fill=c)) +
  geom_sf(size=0.1) +
  scale_fill_manual(values=setNames(colors,labels)) +
  facet_wrap("Period",ncol=2) +
  coord_sf(datum=NA) +
  labs(fill="Average Annual\nPopulation Change",
       title="Vancouver population change",
       caption = "StatCan Census 2001-2016")

