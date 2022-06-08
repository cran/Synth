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
\usage{data(basque)}
\format{A panel dataframe made up of 18 units: 1 treated (no 17; the Basque country) and 16 control regions (no. 2-16,18).
Region no. 1 is the average for the whole country of Spain. 1 outcome variable (gdpcap). 13 predictor variables 
(6 sectoral production shares, 6 highest educational attainment categories, population density, and the investment rate).
Region names and numbers are stored in regionno and regionname. 42 time periods (1955 - 1997).
All columns have self-explanatory column names. For reference the variables are:
\itemize{
\item \preformatted{regionno}: Region Number.
\item \preformatted{regionname}: Region Name.
\item \preformatted{year}: Year.
\item \preformatted{gdpcap}: real GDP per capita (in 1986 USD, thousands).
\item \preformatted{sec.agriculture}: production in agriculture, forestry, and fishing sector as a percentage of
total production.
\item \preformatted{sec.energy}: production in energy and water sector as a percentage of
total production.
\item \preformatted{sec.industry}: production in industrial sector as a percentage of
total production.
\item \preformatted{sec.construction}: production in construction and engineering sector as a percentage of
total production.
\item \preformatted{sec.energy}: production in marketable services sector as a percentage of
total production.
\item \preformatted{sec.energy}: production in Nonmarketable services sector as a percentage of
total production.
\item \preformatted{school.illit}: number of illiterate persons.
\item \preformatted{school.prim}: number of persons with primary education or without studies.
\item \preformatted{school.med}: number of persons with some high school education.
\item \preformatted{school.high}: number of persons with high school degree.
\item \preformatted{school.post.high}: number of persons with tertiary education. 
\item \preformatted{popdens}: population density (persons per square kilometer).
\item \preformatted{invest}: gross total investment as a share of GDP.
}
}
\source{Abadie, A. and Gardeazabal, J. (2003) Economic Costs of Conflict: A Case Study of the Basque Country \emph{American Economic Review} 93 (1) 113--132.
            
Abadie, A., Diamond, A., Hainmueller, J. (2011). Synth: An R Package for Synthetic Control Methods in Comparative Case Studies. \emph{Journal of Statistical Software} 42 (13) 1--17.

}



