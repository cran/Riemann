## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("Riemann")

## ----eval=FALSE---------------------------------------------------------------
#  if (!require("devtools")) {
#    install.packages("devtools")
#  }
#  devtools::install_github("kyoustat/Riemann")

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(Riemann)

## ----message=FALSE, warning=FALSE, fig.align='center', fig.width=5------------
# load the necessary libraries for visualization
if (!requireNamespace("usmap",  quietly = TRUE)){install.packages("usmap")}
if (!requireNamespace("ggplot2",quietly = TRUE)){install.packages("ggplot2")}
library(usmap)
library(ggplot2)

# load the cities dataset and apply converting
data("cities")
dfcities = data.frame(lon=cities$coord[,2], lat=cities$coord[,1], tt=cities$names)
mycities = usmap::usmap_transform(dfcities)

# visualize
myplot <- usmap::plot_usmap(regions="states", exclude=c("AK")) + 
  geom_point(data=mycities, aes(x=lon.1, y=lat.1), alpha=.5) + 
  labs(title="60 Populated Cities in the US")
plot(myplot)

## ----message=FALSE, warning=FALSE, fig.align='center', fig.width=7------------
# (WRAPPING) Spherical Data
myriem = wrap.sphere(cities$cartesian)

# (INFERENCE) Compute the mean & Convert back to Geographic Coordinates
intmean = as.vector(riem.mean(myriem)$mean) # vectorization for convenience
geomean = sphere.xyz2geo(intmean)           # (lat, lon) will be returned

# (EXTRA)
# Compute Euclidean average of Geographic Coordinates
eucmean = as.double(base::colMeans(cities$coord))

# Merge two means
twomean = rbind(geomean, eucmean)
dfmeans = data.frame(lon=twomean[,2], lat=twomean[,1], type=c("geometric","euclidean"))
vismean = usmap::usmap_transform(dfmeans)

# visualize
myplot <- myplot + 
  geom_point(data=vismean, aes(x=lon.1, y=lat.1, col=type), size=3) + 
  ggrepel::geom_label_repel(data=mycities, aes(x=lon.1, y=lat.1, label=tt),
                                         size = 2, alpha = 0.5,
             label.r = unit(0.25, "lines"), label.size = 0.1,
             segment.color = "red", segment.size = 0.5) + 
  labs(title="Populated Cities and Central Locations")
plot(myplot)

