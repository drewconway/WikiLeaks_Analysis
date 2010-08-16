source('wikileaks_analysis.R')

radial.distance <- function(latitude, longitude)
{
  chaghcharan.latitude <- 34.5225
  chaghcharan.longitude <- 65.251667
  
  if (is.na(longitude) || is.na(latitude))
  {
    return(NA)
  }
  else
  {
    return(sqrt((latitude - chaghcharan.latitude)^2 + (longitude - chaghcharan.longitude)^2))
  }
}

afg <- transform(afg, RadialDistance = apply(afg,
                                             1,
                                             function(r)
                                             {
                                               radial.distance(as.numeric(r['Latitude']),
                                                               as.numeric(r['Longitude']))
                                             }))

latitude.angle <- function(longitude, radius)
{
  chaghcharan.latitude <- 34.5225
  chaghcharan.longitude <- 65.251667
  
  if (is.na(longitude) || is.na(radius))
  {
    return(NA)
  }
  else
  {
    return(asin((longitude - chaghcharan.longitude) / radius))
  }
}

afg <- transform(afg, LatitudeAngle = apply(afg,
                                            1,
                                            function(r)
                                            {
                                              latitude.angle(as.numeric(r['Longitude']), as.numeric(r['RadialDistance']))
                                            }))

pdf('images/polar_coordinates_plot.pdf')
ggplot(afg, aes(x = log(RadialDistance), y = LatitudeAngle)) +
  geom_point()
dev.off()

