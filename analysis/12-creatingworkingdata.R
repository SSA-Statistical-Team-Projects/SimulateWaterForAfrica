#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Generating Working Dataset
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will add climatic and A1 level information to the grids

rm(list=ls())
gc(reset = TRUE)

# Loading Packages
pak_list <- c("raster", "sf", "tidyverse","dplyr")

lapply(pak_list, function(pkg) {
  suppressMessages(suppressWarnings(
    library(pkg, character.only = TRUE)
  ))
})
lapply(pak_list, library, character.only = TRUE)

sf::sf_use_s2(FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crop_df <- readRDS("data-clean/working_data/gaez/gaez_crop_year_analysis_final.RDS")

clm_df <- readRDS("data-clean/working_data/terra-climate/climate_data_working.RDS")

grid_polygon <- st_read("data-clean/working_data/gaez/grid_polygon/unique_grid_polygons.shp")

#loading Admin1 shapefiles for SSA
ssa_a1_shp <- st_read("data-raw/shapefile/SSA_Admin1/gsap_v2_1/gsap_v2_1.shp")

ssa_a1_shp <- ssa_a1_shp %>%
  dplyr::filter(region %in% c("SSA","MNA")) %>% #including MENA because of some data inconsistencies in this map
  dplyr::mutate(a1_id = row_number()) %>%
  dplyr::select(-id)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merging data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crop_df <- left_join(crop_df %>%
                       as.data.frame(),
                     clm_df %>%
                       as.data.frame() %>%
                       dplyr::select(ends_with(c("srno","_sq","cubed",
                                                 "winter","spring","fall","summer","mean"))),
                     by = c("srno"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Assigning Admin1 to data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Checking if CRSs match
crs(ssa_a1_shp) == crs(grid_polygon) # FALSE
st_crs(ssa_a1_shp) <- st_crs(grid_polygon)

# Matching grid_polygon with the a1_id it has the maximum overlap with
joined_data <- st_join(grid_polygon,
                       ssa_a1_shp %>%
                         dplyr::select(c(a1_id,geometry)),
                       by.feature = TRUE)

# Step 1: Identify srnos with only one st_join result
unique_srno <- joined_data %>%
  as.data.frame() %>%
  dplyr::group_by(srno) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count == 1) %>%
  dplyr::select(-count) %>%
  dplyr::filter(!is.na(a1_id))

dup_srno <- joined_data %>%
  as.data.frame() %>%
  dplyr::group_by(srno) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1 | is.na(a1_id)) %>% #saving those which didn't match at all for the intersection
  dplyr::select(-count)


# check
dim(unique_srno)[1] + dim(dup_srno)[1] == dim(joined_data)[1] # TRUE

dup_srno <- dup_srno %>%
  as.data.frame() %>%
  dplyr::distinct(srno) %>%
  dplyr::mutate(for_int = 1)


# Step 2: Calculate intersections and keep the highest area one for duplicates
sub_grid_poly <- left_join(grid_polygon,
                           dup_srno,
                           by = "srno")

sub_grid_poly <- sub_grid_poly %>%
  dplyr::filter(for_int == 1) %>%
  dplyr::select(srno,geometry)

dup_joined_data <- st_intersection(sub_grid_poly,
                                   ssa_a1_shp %>%
                                     dplyr::select(a1_id,geometry)) # this automatically drops the unmatched grids

dup_joined_data$intersection_area = st_area(dup_joined_data)

# Keeping the row with the highest intersection area
dup_joined_data <- dup_joined_data %>%
  dplyr::group_by(srno) %>%
  dplyr::filter(intersection_area == max(intersection_area)) %>%
  dplyr::ungroup()

# For those grids (srno) which have more than one grids with the exact same intersection area, I will randomly assign them to one of the admin boundaries
sev_area_selected <- dup_joined_data %>%
  dplyr::group_by(srno) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(count > 1) %>%
  dplyr::group_by(srno) %>%
  dplyr::sample_n(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-count)

# Trying to add an additional tolerance / buffer to the unmatched grids to see if they can be matched
unmatched_df <- joined_data %>%
  as.data.frame() %>%
  dplyr::filter(is.na(a1_id)) %>%
  dplyr::mutate(unmatched = 1) %>%
  dplyr::select(srno,unmatched)

unmatched_poly <- left_join(grid_polygon,
                            unmatched_df,
                            by = "srno")
unmatched_poly <- unmatched_poly %>%
  dplyr::filter(unmatched == 1) %>%
  dplyr::select(srno,geometry)

unmatched_poly_tol <- st_buffer(unmatched_poly, dist = 0.001)

unmatched_int <- st_intersection(unmatched_poly_tol,
                                 ssa_a1_shp %>%
                                   dplyr::select(a1_id,geometry))
unmatched_int %>% dplyr::distinct(srno) %>% dim() #they're all unique

unmatched_int <- unmatched_int %>%
  as.data.frame() %>%
  dplyr::select(srno,a1_id)

# Final assigned / matched data
# adding
# 1) the unique srno-a1_id matches from the st_join
# 2) best matches in the st_intersect (using)
# 3) randomly selected matches when the intersection was non-unique in intersection area
# 4) matching those with no matches using a small tolerance buffer

final_df <- bind_rows(unique_srno,
                      dup_joined_data %>%
                        dplyr::group_by(srno) %>%
                        dplyr::mutate(count = n()) %>%
                        dplyr::ungroup() %>%
                        dplyr::filter(count == 1) %>%
                        dplyr::select(-count),
                      unmatched_int,
                      sev_area_selected)


crop_df <- left_join(crop_df,
                     final_df %>%
                       as.data.frame() %>%
                       dplyr::select(srno,a1_id),
                     by = "srno")

crop_df <- crop_df %>%
  dplyr::mutate(crop_a1_int = paste0(crop_code,"x",a1_id),
                any_irri = case_when(har_irri > 0 ~ 1,
                                     har_tot > 0 & (is.na(har_irri) | har_irri == 0) ~ 0,
                                     TRUE ~ NA_real_),
                irri_intensity = har_irri/har_tot)

saveRDS(crop_df,
        "data-clean/working_data/estimation/a1_crop_year_level_working_data.RDS")