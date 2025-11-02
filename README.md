# Bird Watch Visualization App

## About This Project

This is our **Assignment 3** for **Information Visualisation (GEOM90007)** at the University of Melbourne. We built an interactive app that shows bird sightings around Melbourne using R Shiny and Tableau.

The app lets users explore where different bird species have been spotted in Melbourne, when they were seen, and how bird diversity varies across the city. We used real data from BirdLife Australia to make this visualization.

## Our Team

- **Sarathi Thirumalai Soundararajan** - [sthirumalais@student.unimelb.edu.au](mailto:sthirumalais@student.unimelb.edu.au)
- **Eby Thomas** - [thomas.e2@student.unimelb.edu.au](mailto:thomas.e2@student.unimelb.edu.au)
- **Harish Kannan** - [harish.kannan@student.unimelb.edu.au](mailto:harish.kannan@student.unimelb.edu.au)
- **Mahek Jain** - [mahek.jain@student.unimelb.edu.au](mailto:mahek.jain@student.unimelb.edu.au)

## Data We Used

We got our bird sighting data from **BirdLife Australia's Birdata** database through the **Atlas of Living Australia**. The dataset has over 32,000 bird observation records within the **City of Melbourne (LGA: Melbourne)**.

We gratefully acknowledge **BirdLife Australia** and the **Atlas of Living Australia** for making these data openly available for educational purposes.

### Data Citation

> Atlas of Living Australia occurrence download at [https://biocache.ala.org.au/occurrences/search](https://biocache.ala.org.au/occurrences/search) accessed on 20 October 2025.  
> Descriptive and Image content retrieved via the Wikipedia REST API accessed on 20 October 2025.
> Audio content accessed via the Xeno-Canto API on 20 October 2025 for academic, non-commercial use.
> Morphological trait data derived from the AVONET global bird dataset and merged with species occurrence records using the scientific name field accessed on 22 October 2025.
> Spatial boundary and locality shapefiles obtained from the City of Melbourne Open Data Portal and Geoscape Administrative Boundaries accessed on 25 October 2025.

**DOIs:**  
- Atlas of Living Australia: [https://doi.org/10.26197/ala.aaf6d193-fcff-4c92-9f10-607c1fbda846](https://doi.org/10.26197/ala.aaf6d193-fcff-4c92-9f10-607c1fbda846)

**Data sources:**

- **BirdLife Australia (2019).** *Birdata* ([http://www.birdata.com.au](http://www.birdata.com.au)).  
  Accessed via the ALA on (01/03/2019).  
  For more information: [https://collections.ala.org.au/public/show/dr359](https://collections.ala.org.au/public/show/dr359)

- **Records provided by BirdLife Australia**, accessed through ALA website.  
  For more information: [https://collections.ala.org.au/public/show/dp28](https://collections.ala.org.au/public/show/dp28)

- **Wikipedia contributors (2025).** *Species descriptions and representative images retrieved via the Wikipedia REST API.*  
  API endpoint: [https://en.wikipedia.org/api/rest_v1/](https://en.wikipedia.org/api/rest_v1/)  
  Documentation: [https://www.mediawiki.org/wiki/API:REST_API](https://www.mediawiki.org/wiki/API:REST_API)

- **City of Melbourne (Open Data Portal).** *Municipal Boundary Dataset* (City of Melbourne shapefile).  
  Downloaded from [https://data.melbourne.vic.gov.au/explore/dataset/municipal-boundary/export/](https://data.melbourne.vic.gov.au/explore/dataset/municipal-boundary/export/)  

- **Geoscape Australia / Commonwealth of Australia (2025).**  
  *VIC Suburb/Locality Boundaries – Geoscape Administrative Boundaries (GDA2020).*  
  Downloaded from [https://data.gov.au/data/dataset/vic-suburb-locality-boundaries-geoscape-administrative-boundaries/resource/14a2bec8-cb31-428c-a5eb-c298f466c46d](https://data.gov.au/data/dataset/vic-suburb-locality-boundaries-geoscape-administrative-boundaries/resource/14a2bec8-cb31-428c-a5eb-c298f466c46d)  
  Licensed under **CC BY 4.0**.

- **Tobias, J.A., et al. (2022).** *AVONET: Morphological, ecological and geographical data for all birds.*  
  Downloaded from [https://figshare.com/s/b990722d72a26b5bfead](https://figshare.com/s/b990722d72a26b5bfead).  
  Merged with pre-processed occurrence data using the scientific name field.

- **Xeno-Canto Foundation.** *Xeno-Canto — Bird sounds from around the world* [Dataset].  
  Includes bird call recordings displayed with per-track attribution.  
  Global Biodiversity Information Facility (GBIF) DOI: [https://doi.org/10.15468/qv0ksn](https://doi.org/10.15468/qv0ksn).  
  Licensed under **CC BY-NC-ND 4.0** and **CC BY-NC-SA 4.0**.  
  Terms of Use: [https://xeno-canto.org/about/terms](https://xeno-canto.org/about/terms)

  

### Limitations & Future Work

- Audio data was not available for **8 bird species**, resulting in missing playback functionality for those entries.  
- The **morphological dataset (AVONET)** has been prepared but not yet integrated into the current interface.  
  Future versions will incorporate this dataset to enhance the visual and analytical components of the application.
