## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(tongfen)
library(sf)

## -----------------------------------------------------------------------------
data("vancouver_elections_data_2015",package="tongfen")
data("vancouver_elections_data_2019",package="tongfen")
data("vancouver_elections_geos_2015",package="tongfen")
data("vancouver_elections_geos_2019",package="tongfen")

## -----------------------------------------------------------------------------
party_colours <- c(
  "People's Party"="#4a3389",
  Conservative="#0C499C",
  "Bloc Québécois"="#02819E",
  Liberal="#A50B0B",
  NDP="#DA3D00",
  "Green Party"="#2E8724",
  "Independent"="#676767",
  Other="yellow"
)

raw_vote_data <- bind_rows(
  vancouver_elections_geos_2015 %>% 
    left_join(vancouver_elections_data_2015 %>%
                group_by(PD_2015) %>%
                top_n(1,Votes),by="PD_2015") %>%
    mutate(Year="2015"),
  vancouver_elections_geos_2019 %>% 
    left_join(vancouver_elections_data_2019 %>%
                group_by(PD_2019) %>%
                top_n(1,Votes),by="PD_2019") %>%
    mutate(Year="2019")
) %>%
  mutate(Party=case_when(grepl("NDP",Party) ~ "NDP",
                        Party %in% names(party_colours) ~ Party,
                        TRUE ~ "Other"))

ggplot(raw_vote_data) +
  geom_sf(aes(fill=Party),size=0.2,color="black") +
  facet_wrap("Year") +
  scale_fill_manual(values=party_colours) +
  theme(legend.position = "bottom") +
  coord_sf(datum=NA) +
  labs(title="Canada federal election winning candidate party by polling district", 
       caption = "Data: Elections Canada")

## -----------------------------------------------------------------------------
correspondence <- estimate_tongfen_correspondence(
  data = list(vancouver_elections_geos_2015,vancouver_elections_geos_2019),
  geo_identifiers = c("PD_2015","PD_2019"),
  method = "estimate",
  tolerance = 30) 

vote_data <- correspondence %>% 
  left_join(vancouver_elections_data_2019 %>% select(PD_2019,Party,Votes_2019=Votes),by="PD_2019") %>%
  left_join(vancouver_elections_data_2015 %>% select(PD_2015,Party,Votes_2015=Votes),by=c("PD_2015","Party")) %>%
  group_by(TongfenID,Party) %>% 
  summarize_at(vars(starts_with("Votes")),sum,na.rm=TRUE) %>%
  group_by(TongfenID) %>%
  mutate(Total_2019=sum(Votes_2019,na.rm=TRUE),
         Total_2015=sum(Votes_2015,na.rm=TRUE)) %>%
  mutate(Share_2015=Votes_2015/Total_2015,
         Share_2019=Votes_2019/Total_2019) %>%
  ungroup() 

## -----------------------------------------------------------------------------
base_geo <- vancouver_elections_geos_2019 %>% 
  left_join(correspondence,by=c("PD_2019")) %>%
  group_by(TongfenID) %>%
  summarise() 

## ----fig.height=3-------------------------------------------------------------
focus_area <- base_geo[base_geo$TongfenID=="59039_51",]

bbox <- focus_area %>% st_buffer(200) %>% st_bbox() %>% st_as_sfc()

compare_geos <- rbind(vancouver_elections_geos_2015 %>% mutate(type="2015") %>% select(type) ,#%>% st_transform(4326),
      vancouver_elections_geos_2019 %>% mutate(type="2019") %>% select(type) ,#%>% st_transform(4326),
      base_geo %>% mutate(type="TongFen") %>% select(type)) #%>% st_transform(4326)) %>% 

ridings <- vancouver_elections_geos_2019 %>% 
  group_by(FED_NUM) %>%
  summarize(.groups="drop") 


ggplot(compare_geos) +
  facet_wrap("type") +
  geom_sf(size=0.25) +
  geom_sf(data=ridings,size=0.75,fill=NA) +
  geom_sf(data=bbox,color="red",fill=NA,size=1) +
  coord_sf(datum = NA) +
  labs(title="Federal electoral poll district boundaries TongFen",caption="Elections Canada")

## ----fig.height=3-------------------------------------------------------------
bb <- st_bbox(bbox)
ggplot(compare_geos) +
  facet_wrap("type") +
  geom_sf(size=0.25) +
  geom_sf(data=ridings,size=0.75,fill=NA) +
  geom_sf(data=bbox,color="red",fill=NA,size=1) +
  coord_sf(datum = NA, xlim=c(bb$xmin,bb$xmax),ylim=c(bb$ymin,bb$ymax)) +
  labs(title="Federal electoral poll district boundaries TongFen",caption="Elections Canada")

## -----------------------------------------------------------------------------
main_parties <- c("Liberal","Conservative","Green Party","NDP-New Democratic Party")

plot_data <- base_geo %>%
  left_join(vote_data %>% 
              filter(Party %in% main_parties) %>%
              mutate(Party=recode(Party,"NDP-New Democratic Party"="NDP")),
            by="TongfenID") 

plot_data2 <- vancouver_elections_geos_2019 %>% 
  group_by(FED_NUM) %>%
  summarize(.groups="drop") 

plot_data %>% 
  ggplot(aes(fill=Share_2019-Share_2015)) +
  #mountainmathHelpers::geom_water() +
  geom_sf(size=0.01) +
  geom_sf(data=plot_data2,size=0.5,fill=NA) +
  scale_fill_gradient2(labels=function(d)scales::percent(d,suffix="pp")) +
  facet_wrap("Party") +
  coord_sf(datum = NA) +
  labs(title="Percentage point change in share of polling station votes 2015-2019",
       fill="Percentage point\nchange 2015-2019", caption="Elections Canada")

## -----------------------------------------------------------------------------
correspondence2 <- estimate_tongfen_correspondence(
  data = list(vancouver_elections_geos_2015,vancouver_elections_geos_2019),
  geo_identifiers = c("AP_2015","AP_2019"),
  method = "estimate",
  tolerance = 30) 

advance_votes_data <- correspondence2 %>%
  left_join(vancouver_elections_data_2019 %>% select(AP_2019=PD_2019,Party,Votes_2019=Votes),by="AP_2019") %>%
  left_join(vancouver_elections_data_2015 %>% select(AP_2015=PD_2015,Party,Votes_2015=Votes),by=c("AP_2015","Party")) %>%
  group_by(TongfenID,Party) %>% 
  summarize_at(vars(starts_with("Votes")),sum,na.rm=TRUE) %>%
  group_by(TongfenID) %>%
  mutate(Total_2019=sum(Votes_2019,na.rm=TRUE),
         Total_2015=sum(Votes_2015,na.rm=TRUE)) %>%
  mutate(Share_2015=Votes_2015/Total_2015,
         Share_2019=Votes_2019/Total_2019) %>%
  ungroup() 

## -----------------------------------------------------------------------------
advanced_base_geo <- vancouver_elections_geos_2019 %>% 
  left_join(correspondence2,by=c("AP_2019")) %>%
  group_by(TongfenID) %>%
  summarise()

plot_data <- advanced_base_geo %>%
  left_join(advance_votes_data %>% 
              filter(Party %in% main_parties) %>%
              group_by(TongfenID) %>%
              complete(Party=main_parties,fill=list(Share_2015=0,Share_2019=0)) %>%
              mutate(Party=recode(Party,"NDP-New Democratic Party"="NDP")),
            by="TongfenID") 



plot_data %>% 
  ggplot(aes(fill=Share_2019-Share_2015)) +
  geom_sf(size=0.1) +
  geom_sf(data=ridings,size=0.5,fill=NA) +
  scale_fill_gradient2(labels=function(d)scales::percent(d,suffix="pp")) +
  theme(legend.position="bottom", legend.key.width = unit(2.5, "cm")) +
  facet_wrap("Party") +
  coord_sf(datum=NA) +
  labs(title="Percentage point change in share of advance poll votes 2015-2019",
       fill="Percentage point\nchange 2015-2019",
       caption="Elections Canada")


