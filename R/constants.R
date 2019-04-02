# constants

# HUC_WM regions
huc2_hucwm <- tibble::tibble(huc2 = as.character(10:18),
                             hucwm = c(10:13, "COLO", "COLO", 16, "PNW", 18),
                             huc2_name = c("Missouri",
                                           "Arkansas-White-Red",
                                           "Texas-Gulf",
                                           "Rio Grande",
                                           "Upper Colorado",
                                           "Lower Colorado",
                                           "Great Basin",
                                           "Pacific Northwest",
                                           "California"))

# Region 01 New England
# Region 02 Mid-Atlantic
# Region 03 South Atlantic-Gulf
# Region 04 Great Lakes
# Region 05 Ohio
# Region 06 Tennessee
# Region 07 Upper Mississippi
# Region 08 Lower Mississippi
# Region 09 Souris-Red-Rainy
# Region 10 Missouri
# Region 11 Arkansas-White-Red
# Region 12 Texas-Gulf
# Region 13 Rio Grande
# Region 14 Upper Colorado
# Region 15 Lower Colorado
# Region 16 Great Basin
# Region 17 Pacific Northwest
# Region 18 California
# Region 19 Alaska (Old numbering system)
# Region 20 Hawaii
# Region 21 Caribbean
