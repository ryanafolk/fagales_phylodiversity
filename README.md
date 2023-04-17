# fagales_phylodiversity
 Analyses for the paper entitled, "Spatial phylogenetics of Fagales: Investigating the history of temperate forests." `Fagales_FigureMaking.R` gives examples of mapping functions used to prepare figures for the main text; `fagales.R` is the main downstream analysis script. `model_selection.xlsx` gives model selection results pertaining to one of the main text tables. Folders are as follows in subsections:

## nodulation
Contains proportion of nodulating species, both as `.tif` and `.csv`. Hereafter unless otherwise specified data points are grid cell statistics.

## Fagales_CSVs_ToShare
`.csv` files for remainder of grid cell statistics: SR, RPD, randomizations, and additional results not presented in the main text (raw PD, PMPD, RPE). These are the files used in `fagales.R` for grid cell statistics.

## Fagales_Figures_updated
`.pdf` and `.png` files for raw figures, including SR, RPD, and randomizations, as well as additional results not presented in the main text.

## Randomization_Outputs
Raw `.tif` (GeoTIFF) files representing randomization results.

## climate_data
Joined climate data to grid cell statistics in `.csv` format, with names formatted as follows: `[environmental predictor]_[grid cell statistic].csv`.

## global_occurrence_maps
Occurrence data and alpha hulls for every species included in the niche models.

## phylogenetic_regionalization_redone
Regionalization output from the program Biodiverse (load `.nex`). The `.tif` file is GeoTIFF and can be read by standard GIS software (regions encoded as consecutive integers starting with zero). The `.txt.` file gives the numbers of regions together with RGB values for the figures given in the main text.
