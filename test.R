install.packages("maps")
install.packages("mapproj")

library(reshape2) # for melt

install.packages("rworldmap")
library(rworldmap)

mapCountryData()


data(countryExData)
head(countryExData)

countrycode("Somalia","country.name","iso3c")
d[d$region=="SOM",]

all <- map_data("world")
all$region[!is.na(countrycode(all$region,"country.name","iso3c"))] <- countrycode(all$region,"country.name","iso3c")[!is.na(countrycode(all$region,"country.name","iso3c"))]


te.geomap(c,indicator){
  
  d=te.get.hist.multi.free.na(c,te.countries(c,"G200"),indicator,"last")
  
  d$region[!is.na(countrycode(d$Country,"country.name","iso3c"))] <- countrycode(d$Country,"country.name","iso3c")[!is.na(countrycode(d$Country,"country.name","iso3c"))]
  d[is.na(countrycode(d$Country,"country.name","iso3c")),]$region <- substr(d[is.na(countrycode(d$Country,"country.name","iso3c")),]$Country,1,3)
  d$Value <- as.numeric(d$Value)
  
  cnames <-aggregate(cbind(long, lat) ~ region, data = all, FUN = function(x) mean(range(x)))
  cnames$angle <-0
  head(cnames)
  
  ggplot(d, aes(map_id = region)) + 
    geom_map(aes(fill = Value),colour="grey10", map = all) + 
    expand_limits(x = all$long, y = all$lat) +
    scale_fill_gradient2(low="red", high=muted("green"),midpoint = mean(d$Value, na.rm=TRUE)) +
    theme(legend.position = "bottom",
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          axis.text =  element_blank(),
          panel.background = element_rect(fill='#D6E0FF')) +
    #geom_text(cnames, aes(long, lat, label = region, angle=angle, map_id=NULL), size=2.5) + 
    guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) 
  
  
  
}
