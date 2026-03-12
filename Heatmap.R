---
  title: "Three Japan map of E.coli for Emmie"
output: html_notebook
---
  
  ```{r}
#install.packages("devtools")
library(devtools)
#devtools::install_github("ropensci/rnaturalearthhires")
#install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "ggplot2", "dplyr"))
library(rnaturalearthhires)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
#library(dplyr)
library(tidyverse)

```

# Create the dataframe

library(dplyr)
library(readr)  # optional, read_csv is faster than base R

# 1️⃣ Read the CSV files
FQ_df <- read_csv("/Users/emmyobrien/Desktop/Final Thesis Results/FQ_prevalence.csv")
CS_df <- read_csv("/Users/emmyobrien/Desktop/Final Thesis Results/CS_prevalence.csv")
PM_df <- read_csv("/Users/emmyobrien/Desktop/Final Thesis Results/PM_prevalence.csv")

# 2️⃣ Rename the prevalence columns so they are unique
FQ_df <- FQ_df %>% rename(FQ_prevalence = Prevalence)
CS_df <- CS_df %>% rename(CS_prevalence = Prevalence)
PM_df <- PM_df %>% rename(PM_prevalence = Prevalence)

# 3️⃣ Join the three data frames by Prefecture
combined_df <- FQ_df %>%
  inner_join(CS_df, by = "Prefecture") %>%
  inner_join(PM_df, by = "Prefecture")

# 4️⃣ View the combined data frame
head(combined_df)


Pref_df  <- combined_df %>%
  mutate(
    FQ_prevalence = round(FQ_prevalence, 1),
    CS_prevalence = round(CS_prevalence, 1),
    PM_prevalence = round(PM_prevalence, 1)
  )

# View the rounded data
head(Pref_df)

Pref_df <- Pref_df %>%
  select(Prefecture, FQ_prevalence, CS_prevalence, PM_prevalence)


japan_map <- ne_states(country = "Japan", returnclass = "sf")

name_corrections <- c(
  "Ōita" = "Oita",
  "Kōchi" = "Kochi",
  "Hyōgo" = "Hyogo",
  "Kyōto" = "Kyoto",
  "Ōsaka" = "Osaka",
  "Hokkaidō" = "Hokkaido"
)

# Replace names in japan_map$name
japan_map$name <- recode(japan_map$name, !!!name_corrections)

# mock data with prefectural polygons
japan_map <- japan_map %>%
  left_join(Pref_df, by = c("name" = "Prefecture"))

# pref code 
japan_map <- japan_map %>%
  mutate(pref_no = as.integer(sub("JP-", "", iso_3166_2)))


```


```{r}
# testing based on Emmie's original script
ggplot(japan_map) +
  geom_sf(aes(fill = FQ_prevalence)) +              # Color by prevalence
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +  # Color scale
  labs(title = "Fluoroquinolone Resistance Prevalence by Prefecture 2008-2023",
       fill = "Prevalence (%)") +
  theme_minimal()
```



```{r}
# preparing three japan maps plus index map of prefectures

japan_main <- st_crop(
  japan_map,
  xmin = 120, xmax = 146,
  ymin = 20,  ymax = 46        # excluding 南鳥島!
)

japan_simple <- st_simplify(japan_main, dTolerance = 0.1)  

japan_simple <- japan_simple %>% 
  mutate(
    region = if_else(is.na(region), "Kyushu", region),  
    # assign "Kyusyu" for two prefecture in Kyusyu Region having na for $region
    region = factor(region)  # for controling region order
  )

#------- label points for region names
region_sf <- japan_simple %>%   
  group_by(region) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
region_points <- st_point_on_surface(region_sf)

region_label_df <- cbind(
  st_drop_geometry(region_sf),
  st_coordinates(region_points)
)
colnames(region_label_df)[c(ncol(region_label_df)-1, ncol(region_label_df))] <- c("lon", "lat")

#------- label points for prefecture names
label_points <- st_point_on_surface(japan_simple)
label_df <- cbind(st_drop_geometry(japan_simple), st_coordinates(label_points))
colnames(label_df)[c(ncol(label_df)-1, ncol(label_df))] <- c("lon", "lat")

label_df <- label_df %>% # manual move for Okinawa label
  mutate(
    lon = if_else(pref_no == 47, lon - 1.0, lon),   
    lat = if_else(pref_no == 47, lat - 1.0, lat)    
  )

```

```{r}
# mapping layout (2 by 2)
# prevalence 1 map + prevalence 2 map
# prevalence 3 map + index map

library(ggrepel)
library(patchwork)

# function for ggplot2-based prevalence map 
p_base_fill <- function(fillvar, subtitle) {
  ggplot(japan_simple) +
    geom_sf(aes(fill = .data[[fillvar]]), color = "black", size = 0.1) +
    coord_sf(expand = FALSE) +
    scale_fill_viridis_c(
      option   = "rocket",
      na.value = "grey90",
      direction = -1,
      begin = 0.5,       # controlling the darkest colour
      name     = "Prevalence (%)",
      limits   = c(0, 50) # using the same range for the three prevalences
    ) +
    labs(subtitle = subtitle) +
    theme_void() +
    theme(
      plot.subtitle = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",           # no legend
      plot.margin = margin(2, 2, 2, 2)
    )
}

p1 <- p_base_fill("FQ_prevalence", "Fluoroquinolones")
p2 <- p_base_fill("CS_prevalence", "Third-Generation Cephalosporins")
p3 <- p_base_fill("PM_prevalence", "Carbapenems")

#-------- index map
p_index <-  ggplot() +
  geom_sf(
    data  = japan_simple,
    aes(fill = region),
    color = "grey40",
    size  = 0.1,
    alpha = 0.5
  ) +
  #  ggplot(japan_simple) +
  #geom_sf(fill = "grey98", color = "black", size = 0.1) +
  geom_text_repel(               # region name labels 
    data = region_label_df,
    aes(x = lon, y = lat, label = region),
    size     = 6,
    seed        = 123,   # for reproducability
    fontface = "bold",
    alpha    = 0.2,
    force = 1,
    box.padding   = unit(0.7, "lines"),  # important: spacer between labels
    point.padding = unit(0.7, "lines"),  # distance between label points and label text
    max.overlaps  = Inf,                 # prevent omitting labels
    min.segment.length = 0               # draw lines of labels 
  ) +
  coord_sf(expand = FALSE) +
  scale_fill_brewer(
    palette = "Pastel1", 
    name    = "Region"
  ) +
  geom_text_repel(  # prefecture name labels
    data = label_df,
    aes(x = lon, y = lat, label = pref_no),
    size        = 2, 
    fontface    = "bold",
    seed        = 123,   # for reproducability
    box.padding = 0.2,
    point.padding = 0.1,
    force = 0.001,      # this is an important parameter for automatic adjustments of label locations
    force_pull = 1,    # ...
    min.segment.length = 0.01,
    segment.size  = 0.2,  
    max.overlaps  = Inf 
  ) +
  coord_sf(expand = FALSE) +
  labs(subtitle = "Prefecture/Region index") +
  theme_bw() +
  theme(
    plot.subtitle    = element_text(hjust = 0.5, face = "bold"),
    axis.text        = element_text(size = 8),
    axis.title       = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey85", size = 0.3),
    panel.border     = element_blank(),   
    plot.margin      = margin(2, 6, 4, 6)
  )

p1_legend <- p_base_fill("FQ_prevalence", "Fluoroquinolones") +
  theme(legend.position = "right")

# the following part needs patchwork package
p_all <- ((p1_legend | p2) /
            (p3        | p_index)) +
  plot_layout(guides = "collect") &  
  theme(
    legend.position      = "bottom", 
    legend.justification = c(0, 0), 
    legend.box.just      = "left",
    legend.margin        = margin(t = -2, r = 0, b = 2, l = 10)  # manual adjustment
  )

p_all

# dpi should be 600 for paper submission
ggsave("Fig_AMR_2by2maps_test.png", p_all, width = 180, height = 220, units = "mm", dpi = 200)

# if you want to edit the graphics manually, use this for obtaining a pdf file which can be edited by a vector graphic software like Adobe-Illustrator.
#ggsave("Fig_AMR_2by2maps.pdf", p_all, width = 180, height = 220, units  = "mm", device = cairo_pdf) 


```



Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
