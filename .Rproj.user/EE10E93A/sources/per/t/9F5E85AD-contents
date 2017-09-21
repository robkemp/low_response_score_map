library(leaflet)
library(leaflet.extras)
library(readr)
library(dplyr)
library(tidyr)
library(tigris)
library(htmlwidgets)
library(sf)






dat=read_csv("PDB_2015_Block_Group.csv")


d=dat%>%
  select(GIDBG, State, County, County_name, Tract, LAND_AREA, Low_Response_Score)%>%
  filter(State==53)

geo=st_geometry(block_groups(state=53))

pl=places(state=53)

pl=st_geometry(pl%>%
  filter(LSAD%in%c(25,43)))
pll=st_cast(pl, "MULTILINESTRING")

dg=geo_join(geo, d, "GEOID", "GIDBG")

popup <- paste0("Low Response Score: ", as.character(round(dg$Low_Response_Score, 2)))

popup2 <-as.character(pl$NAME)

pal <- colorNumeric("Blues", NULL, n = 6)

m=leaflet() %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons(data = dg, 
              fillColor = ~pal(dg$Low_Response_Score), 
              fillOpacity = 0.5, 
              weight = 0.2, 
              popup = popup,
              group="Low Response Score") %>%
  addPolygons(data = pll, 
              # fillColor = "transparent",
              color = "black",
              weight = 3,
              # popup = popup2,
              # highlightOptions = highlightOptions(color = "purple", weight = 2,
              #                                     bringToFront = TRUE),
              group="Place Bounds")%>%

  addLayersControl(
    overlayGroups = c("Low Response Score", "Place Bounds"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addLegend(pal = pal, 
            values = dg$Low_Response_Score, 
            position = "bottomright", 
            title = "Low Response Scores")%>%
  addSearchOSM()%>%
  addControl("<P><B>Hint!</B> Search for ...<br/><ul><li>Renton</li><li>Spokane</li><li>Chelan</li><li>Etc.</li></ul></P>",
                                  position='bottomleft')

saveWidget(m, file="m.html")