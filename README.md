## Multi-decadal coastline dynamics controlled by migrating sub-tidal mudbanks

[![GitHub release](https://img.shields.io/endpoint?color=blue&label=v0.2&url=https%3A%2F%2Fgithub.com%2Fjobbo90%2Foffshore_boundary%2Freleases%2F)](https://github.com/jobbo90/offshore_boundary/releases/)

![Alt text](https://github.com/jobbo90/offshore_boundary/tree/main/results/GIF/pos230000-animation.gif)


## Description

The approach is described in detail in the following publications;
1. Data driven approach to derive image end-members for linear spectral unmixing: https://doi.org/10.1016/j.jag.2020.102252
2. Time series analysis of coastline changes in response to migrating mudbanks (submitted 07-2021)

The coastline position estimates for Suriname are available through the Google Earth Engine (GEE) code editor via: 
https://code.earthengine.google.com/?accept_repo=users/jobdevries90/MangroMud

## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── data               
│   ├── processed      
│   ├── raw
├── results
│	├── GIF
│	├── methodology_figures
│	├── temp_maps
│	├── Validation
└── src

```


## usage
In order to create coastline position estimates and quantify annual changes with respect to alongshore migrating mudbanks the following steps are required.
1. Separate land and water by applying Otsu thresholding approach in GEE
2. Define image end-members for the purpose of linear spectral unmixing (LSU) in GEE
3. Intersection of the shorelines with pre-defined shore-normal transects in GEE
4. Extract mud abundance estimates from the LSU for each transect from GEE 
5. post-process the estimated coastline postions in R
6. post-process mud abundances for estimating the pressence or absence of mudbanks in R


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
