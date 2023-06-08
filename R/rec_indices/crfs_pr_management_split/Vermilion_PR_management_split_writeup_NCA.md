---
title: "CRFS PR Index Allocation at Cap Mencocino for Vermilion in 2021"
author: "Melissa H. Monk"
date: "July 08, 2021"
output:
  bookdown::pdf_document2: 
    keep_tex: true
    keep_md: true
header-includes:
  - \usepackage{booktabs}
  - \usepackage{longtable}
  - \usepackage{array}
  - \usepackage{multirow}
  - \usepackage{wrapfig}
  - \usepackage{float}
  - \usepackage{colortbl}
  - \usepackage{pdflscape}
  - \usepackage{tabu}
  - \usepackage{threeparttable}
  - \usepackage[normalem]{ulem}
  - \usepackage{makecell}
  - \usepackage{xcolor}
  - \usepackage{placeins}
---

<!-- Common lat/long 
Cape Mendocino, Pt. Conception and OR border -->
\newcommand\CapeM{$40^\circ 10^\prime N$}
\newcommand\PtC{$34^\circ 27^\prime N$}
\newcommand\CAOR{$42^\circ 00^\prime N$}









## Allocation of Yield Among Federal Management Areas


The 2021 northern California base model for vermilion represents U.S. 
waters between \PtC and the California-Oregon border \CapeM. Federal management of 
the minor shelf rockfish, which includes vermilion, is based on areas north and south of \CapeM, 
near Cape Mendocino. Therefore, yield estimates from the northern California base 
model must be divided between the northern and southern management areas in order 
to determine the contribution of vermilion to the minor nearshore rockfish overfishing limit (OFL).

Allocation of the OFL could, ideally, be based on a fishery-independent survey of 
abundance, but lacking that information several alternatives exist. Previous 
allocations have used catch as a proxy for abundance when no other information 
was available [@Dick2010; @Dick2011]. Recent catches of vermilion in the recreational 
and commercial sectors suggest that roughly 4.8% and 2.8%, respectively, of catches 
in these sectors are landed north of Cape Mendocino (Tables \@ref(tab:rec-split) and \@ref(tab:com-split)). Removals for the recreational fleet are in numbers of fish and removals 
from the commercial fleet are in mt, to be consistent with the assessment inputs. 

Recent advances in habitat mapping allow us to estimate the relative amount of reef 
habitat within state waters (0-3 nm) in each area, e.g., the [California Seafloor Mapping Project](https://walrus.wr.usgs.gov/mapping/csmp/).
If we assumed that average density of vermilion is constant over the assessed area, 
the fraction of vermilion occurring north of Cape Mendocino would be equal to the 
fraction of habitat in the same area:  approximately 18% (pers. comm. Rebecca Miller, UCSC). However, the assumption of equal density may not be accurate, and no direct estimates of 
density are available from a fishery-independent survey with adequate spatial coverage.

As was proposed in the 2017 blue/deacon rockfish complex assessment we combined existing habitat information with a proxy for fish density – catch per unit effort. Although data from the CRFS onboard CPFV observer 
program are more precise in terms of total catch, effort, and location, relatively few 
samples available north of Cape Mendocino. Sampling coverage for the dockside 
survey is spatially more complete, in that numerous samples exist in the northern 
management area. We therefore used the private boat (PR1) CPUE data to develop a spatial 
index (with CPUE assumed proportional to density), and multiplied the area-specific 
CPUE estimates by the amount of habitat to produce a spatial index of relative abundance.
Data were filtered using the same methods detailed in the assessment for the CRFS 
private boat dockside index. Years prior to 2016 were subsequently dropped as well as 2020 due to reduced sampling during COVID-19, to create an index that is representative of recent catch rates in each area. Sample sizes (number of trips) for the final data set are shown in Table \@ref(tab:number-trips).

Vermilion rockfish is a shelf species and we recognize that there is a fraction of the 
population and rocky habitat outside of state waters.  However, due to depth closures that began in 2002, samples from deeper waters are not available, nor is the associated habitat 
data.  This method assumes the same proportion of habitat outside state waters north and 
south of Cape Mendocino.  We explored limiting the data to only angler-reported trips inside state waters. However, the accuracy of the angler-reported trip location is unknown and the 
trip may represent catches from both inside and outside state waters. Filtering based on 
angler-reported area fished did not affect the final result, so we retained all data for this analysis.


We modeled CPUE (vermilion per angler trip) using a Bayesian negative binomial regression with subregion (defined as CRFS districts, see Table \@ref(tab:number-trips)) as a qualitative covariate and 
pooling data across years 2016-2019. Including the subregion covariate reduced AIC 
by 2270 points relative to the null (intercept-only) model. CPUE in the Wine District subregion was lower than the other subregions in the model 
(Table \@ref(tab:rel-cpue)). When CPUE is multiplied by the percentage of habitat area north of \CapeM latitude, the expected percentage of the stock that occurs north of Cape Mendocino is 4.4% (Table \@ref(tab:rel-cpue)).



<!--Tables-->


\begin{table}

\caption{(\#tab:rec-split)California recreational total mortality (1000s of fish) for vermilion by CRFS district, 2016-2019. The Wine district occurs mainly north of Cap Mendocino. Source:RecFIN}
\centering
\begin{tabular}[t]{rrrrr>{\raggedright\arraybackslash}p{2.4cm}}
\toprule
Year & CENTRAL & BAY & WINE & REDWOOD & Percent mortality in WINE District\\
\midrule
\cellcolor{gray!6}{2016} & \cellcolor{gray!6}{63.382} & \cellcolor{gray!6}{15.480} & \cellcolor{gray!6}{3.888} & \cellcolor{gray!6}{2.099} & \cellcolor{gray!6}{4.58\%}\\
2017 & 79.042 & 20.795 & 4.891 & 2.858 & 4.55\%\\
\cellcolor{gray!6}{2018} & \cellcolor{gray!6}{89.937} & \cellcolor{gray!6}{17.996} & \cellcolor{gray!6}{4.192} & \cellcolor{gray!6}{3.214} & \cellcolor{gray!6}{3.63\%}\\
2019 & 96.274 & 29.016 & 8.616 & 3.363 & 6.28\%\\
\bottomrule
\end{tabular}
\end{table}

\begin{table}

\caption{(\#tab:com-split)Commercial landings (mt) of vermilion in California port complexes located north (CRS+ERK) and south (MRO-BRG) of Cape Mendocino, 2016-2019. Source: CALCOM.}
\centering
\begin{tabular}[t]{lrr>{\raggedright\arraybackslash}p{2.4cm}}
\toprule
Year & CRS+ERK & MRO-BRG & Percent landings in CRS+ERK\\
\midrule
\cellcolor{gray!6}{2016} & \cellcolor{gray!6}{0.888} & \cellcolor{gray!6}{12.477} & \cellcolor{gray!6}{1.33\%}\\
2017 & 1.550 & 12.738 & 2.32\%\\
\cellcolor{gray!6}{2018} & \cellcolor{gray!6}{2.010} & \cellcolor{gray!6}{17.650} & \cellcolor{gray!6}{3.00\%}\\
2019 & 3.052 & 16.579 & 4.56\%\\
\bottomrule
\end{tabular}
\end{table}

\begin{table}

\caption{(\#tab:number-trips)Number of trips sampled in the PR1 mode by year and CRFS District.}
\centering
\begin{tabular}[t]{rrrrr}
\toprule
YEAR & Central & Bay & Redwood & Wine\\
\midrule
\cellcolor{gray!6}{2016} & \cellcolor{gray!6}{2175} & \cellcolor{gray!6}{795} & \cellcolor{gray!6}{279} & \cellcolor{gray!6}{1108}\\
2017 & 1782 & 800 & 392 & 1148\\
\cellcolor{gray!6}{2018} & \cellcolor{gray!6}{1783} & \cellcolor{gray!6}{677} & \cellcolor{gray!6}{345} & \cellcolor{gray!6}{1149}\\
2019 & 1724 & 681 & 204 & 1151\\
\bottomrule
\end{tabular}
\end{table}

\begin{table}

\caption{(\#tab:rel-cpue)Estimated CPUE, percent habitat area, and relative abundance by CRFS District.}
\centering
\begin{tabular}[t]{lrlrl}
\toprule
CRFS District & CPUE & Area & CPUExAREA & Relative Abundance\\
\midrule
\cellcolor{gray!6}{Central} & \cellcolor{gray!6}{0.833} & \cellcolor{gray!6}{37.58\%} & \cellcolor{gray!6}{0.313} & \cellcolor{gray!6}{61.61\%}\\
Bay & 0.448 & 28.76\% & 0.129 & 25.39\%\\
\cellcolor{gray!6}{Redwood} & \cellcolor{gray!6}{0.286} & \cellcolor{gray!6}{15.26\%} & \cellcolor{gray!6}{0.044} & \cellcolor{gray!6}{8.66\%}\\
Wine & 0.122 & 18.40\% & 0.022 & 4.33\%\\
\bottomrule
\end{tabular}
\end{table}
<!--figures-->
