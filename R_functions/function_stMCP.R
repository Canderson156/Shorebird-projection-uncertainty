# creates an mcp out of sf points objects source: https://rdrr.io/github/juoe/sdmflow/src/R/mcp.R


st_mcp <- function(x, percentile = 95) {

    centroid <- sf::st_centroid(sf::st_union(x))
    dist <- as.numeric(sf::st_distance(x, centroid))
    within_percentile_range <- dist <= quantile(dist, percentile/100)
    x_filter <- st_union(x[within_percentile_range, ])
    st_convex_hull(x_filter)

}
