\name{basque}
 \docType{data}
 \alias{basque}
 \title{Panel Data from Spanish Regions to demonstrate the use of the Synthetic Control Method}
 \description{The dataset contains information from 1955--1997
on 17 Spanish regions. It was used by Abadie and Gardeazabal (2003), which studied the economic effects of
conflict, using the terrorist conflict in the Basque Country as a case study. This
paper used a combination of other Spanish regions to construct a synthetic
control region resembling many relevant economic characteristics of
the Basque Country before the onset of political terrorism in the 1970s.
The data contains per-capita GDP (the outcome variable), as well as
population density, sectoral production, investment, and human capital
(the predictor variables) for the relevant years, and is used here to demonstrate the implementation
of the synthetic control method with the synth library.}
\usage{basque}
\format{A panel dataframe made up of 18 units: 1 treated (no 17; the Basque country) and 16 control regions (no. 2-16,18).
Region no. 1 is the average for the whole country of Spain. 1 outcome variable (gdpcap). 13 predictor variables 
(6 sectoral production shares, 6 highest eductional attainment categories, population density, and the investment rate).
Region names and numbers are stored in regionno and regionname. 42 time periods (1955 - 1997).
All columns have self-explanatory column names.}
\source{}
\references{Abadie and Gardeazabal (2003)}
\keyword{datasets}

