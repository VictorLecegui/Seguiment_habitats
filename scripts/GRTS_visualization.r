library(sf)
library(spsurvey)
library(ggplot2)
library(dplyr)
library(units)
library(mapSpain)

# Bounding box for Catalonia (approximate)

boscos <- read_sf("data/MHTCv3_boscos_RegionsHIC/MHTCv3_boscos_RegionsHIC.shp")


cat_poly <- esp_get_ccaa("Catalunya") |> 
            st_transform(25831)



# Generate 50 random points

set.seed(123)

pts <- st_sample(
  cat_poly,
  size = 50,
  type = "random"
) |> st_as_sf()




plot(cat_poly$geometry)
plot(pts, add = T)
pts$id <- 1:50

# spsurvey sampling frame
pts_frame <- sp_frame(pts)

grts_res <- grts(
  pts,
  n_base = 30
)

# Selected vs unselected GRTS

selected <- grts_res$sites_base |> mutate(sel = "Selected")

unselected <- pts |> 
  filter(!id %in% selected$id) |> 
  mutate(sel = "Unselected")

all_pts <- dplyr::bind_rows(selected, unselected)


#### tasselation

bb <- st_bbox(cat_poly)

grid_lvl3 <- st_make_grid(
  st_as_sfc(bb),
  n = c(8, 8),   # 2^3 × 2^3
  square = TRUE
)

grid_lvl3 <-  st_sf(grid_id = 1:length(grid_lvl3))

library(ggplot2)

ggplot() +
  geom_sf(data = cat_poly, fill = "grey95", color = "black") +
  geom_sf(data = grid_lvl3, fill = NA, color = "black", alpha = 0.3) +
  geom_sf(data = unselected, color = "grey60", size = 2) +
  geom_sf(data = selected, color = "red", size = 2) +
  labs(
    title = "GRTS sampling in Catalonia",
    subtitle = "ETRS89 / UTM 31N (EPSG:25831)"
  ) +
  theme_minimal()

### Ordering 1D line: 

coords <- st_coordinates(pts)

# Normalize coordinates (unitless, but geometry is metric)
nx <- (coords[,1] - min(coords[,1])) / diff(range(coords[,1]))
ny <- (coords[,2] - min(coords[,2])) / diff(range(coords[,2]))

# Morton-like ordering (educational proxy)
order_idx <- order(nx + 2 * ny)

ordered_pts <- pts[order_idx, ]
ordered_pts$order <- 1:nrow(ordered_pts)

# Probability line
# Desired sample size
n <- 30
N <- nrow(ordered_pts)

# Equal inclusion probabilities
ordered_pts$pi <- n / N

# Cumulative probability line
ordered_pts$start <- cumsum(dplyr::lag(ordered_pts$pi, default = 0))
ordered_pts$end   <- ordered_pts$start + ordered_pts$pi

set.seed(42)  # for reproducibility
u1 <- runif(1, 0, 1)

u <- u1 + 0:(n - 1)

u_df <- data.frame(u = u)

ordered_pts$selected <- FALSE

for (i in seq_along(u)) {
  ordered_pts$selected <- ordered_pts$selected |
    (u[i] >= ordered_pts$start & u[i] < ordered_pts$end)
}

### FInal plot

library(ggplot2)

ggplot(ordered_pts) +
  # Probability segments
  geom_segment(
    aes(
      x = start,
      xend = end,
      y = 0,
      yend = 0,
      color = selected
    ),
    linewidth = 6
  ) +
  # u1, u1+1, ..., u1+29
  geom_point(
    data = u_df,
    aes(x = u, y = 0),
    color = "black",
    size = 3
  ) +
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "grey80"),
    labels = c("Not selected", "Selected"),
    name = "Point status"
  ) +
  labs(
    title = "GRTS systematic sampling on the probability line",
    subtitle = paste(
      "ETRS89 / UTM 31N — u₁ =",
      round(u1, 3),
      "| n = 30 from N = 50"
    ),
    x = "Cumulative inclusion probability",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.y = element_blank()
  )
