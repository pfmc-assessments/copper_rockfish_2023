---
geometry: margin=1in
month: "April"
year: "2023"
preamble: |
output:
  sa4ss::techreport_pdf:
    default
  bookdown::pdf_document2:
    keep_tex: true
lang: en
papersize: letter
---



<!--chapter:end:00a.Rmd-->

---
author:
  - name: Melissa H. Monk
    code: 1
    first: M
    middle: H
    family: Monk
  - name: Chantel R. Wetzel
    code: 2
    first: C
    middle: R
    family: Wetzel
  - name: Julia Coates
    code: 3
    first: J
    middle: ''
    family: Coates
author_list: Monk, M.H., C.R. Wetzel, J. Coates
affiliation:
  - code: 1
    address: Southwest Fisheries Science Center, U.S. Department of Commerce, National
      Oceanic and Atmospheric Administration, National Marine Fisheries Service, 110
      McAllister Way, Santa Cruz, California 95060
  - code: 2
    address: Northwest Fisheries Science Center, U.S. Department of Commerce, National
      Oceanic and Atmospheric Administration, National Marine Fisheries Service, 2725
      Montlake Boulevard East, Seattle, Washington 98112
  - code: 3
    address: .na.character
address:
  - ^1^Southwest Fisheries Science Center, U.S. Department of Commerce, National Oceanic
    and Atmospheric Administration, National Marine Fisheries Service, 110 McAllister
    Way, Santa Cruz, California 95060
  - ^2^Northwest Fisheries Science Center, U.S. Department of Commerce, National Oceanic
    and Atmospheric Administration, National Marine Fisheries Service, 2725 Montlake
    Boulevard East, Seattle, Washington 98112
  - ^3^NA
---

<!--chapter:end:00authors.Rmd-->

---
bibliography:
 - sa4ss.bib
 - copper.bib
 - wcassess.bib
---

<!--chapter:end:00bibliography.Rmd-->



# Executive summary{-}

## Stock{-}

This assessment reports the status of copper rockfish (_Sebastes caurinus_) off the California North of Pt. Conception U.S. West coast using data through 2022.

## Catches{-}

Replace text with
trends and current levels.
Include Table for last 10 years.
Include Figure with long-term estimates.

I am testing references of Table \ref{tab:removalsES} and \ref{tab:north-removalsES}

\input{tex_tables/a_Catches_ES.tex}


\input{tex_tables/north_a_Catches_ES.tex}


![Landings by fleet used in the base model where catches in metric tons by fleet are stacked.\label{fig:es-catch}](S:/copper_rockfish_2023/models/nca/0.1_init_model/plots/catch2 landings stacked.png){width=100% height=100% alt="."}

## Data and assessment{-}

This assessment uses the stock assessment framework
Stock Synthesis 

```
[1] "3.30.21.00"
```
(SS3).

Replace text with
date of last assessment,
type of assessment model,
data available,
new information, and
information lacking.

## Stock biomass and dynamics{-}

Replace text with
trends and current levels relative to virgin or historic levels and
description of uncertainty.
Include Table for last 10 years.
Include Figure with long-term estimates.

\input{tex_tables/b_SSB_ES.tex}


![Estimated time series of spawning output (circles and line: median; light broken lines: 95 percent intervals) for the base model.\label{fig:es-sb}](S:/copper_rockfish_2023/models/nca/0.1_init_model/plots/ts7_Spawning_output_with_95_asymptotic_intervals_intervals.png){width=100% height=100% alt="."}


![Estimated time series of fraction of unfished spawning output (circles and line: median; light broken lines: 95 percent intervals) for the base model.\label{fig:es-depl}](S:/copper_rockfish_2023/models/nca/0.1_init_model/plots/ts9_Relative_spawning_output_intervals.png){width=100% height=100% alt="."}

\clearpage

## Recruitment{-}

Replace text with
trends and current levels relative to virgin or historic levels and
description of uncertainty.
Include Table for last 10 years.
Include Figure with long-term estimates.

\input{tex_tables/c_Recr_ES.tex}


![Estimated time series of age-0 recruits (1000s) for the base model with 95 percent intervals.\label{fig:es-recruits}](S:/copper_rockfish_2023/models/nca/0.1_init_model/plots/ts11_Age-0_recruits_(1000s)_with_95_asymptotic_intervals.png){width=100% height=100% alt="."}



\clearpage

## Exploitation status{-}

Replace text with
total catch divided by exploitable biomass or SPR harvest rate.
Include Table for last 10 years.
Include Figure with trend in f relative to target vs. trend in biomass relative to the target.

\input{tex_tables/d_SPR_ES.tex}


![Estimated 1 - relative spawning ratio (SPR) by year for the base model. The management target is plotted as a red horizontal line and values above this reflect harvest in excess of the proxy harvest rate.\label{fig:es-1-spr}](S:/copper_rockfish_2023/models/nca/0.1_init_model/plots/SPR2_minusSPRseries.png){width=100% height=100% alt="."}

## Ecosystem considerations{-}


## Reference points{-}

Replace text with
management targets and definition of overfishing, including the harvest rate that brings the stock to equilibrium at $B_{40\%}$, i.e., the $B_{MSY}$ proxy and the equilibrium stock size that results from fishing at the default harvest rate, i.e., the $F_{MSY}$ proxy.
Include Table of estimated reference points for ssb, SPR, exploitation rate, and yield based on SSB proxy for MSY, SPR proxy for MSY, and estimated MSY values.



![Phase plot of estimated 1-SPR versus fraction unfished for the base model.\label{fig:es-phase}](S:/copper_rockfish_2023/models/nca/0.1_init_model/plots/SPR4_phase.png){width=100% height=100% alt="."}


![Equilibrium yield curve for the base case model. Values are based on the 2020
fishery selectivities and with steepness fixed at 0.80.\label{fig:es-yield}](S:/copper_rockfish_2023/models/nca/0.1_init_model/plots/yield2_yield_curve_with_refpoints.png){width=100% height=100% alt="."}


## Management performance{-}

Include Table of most recent 10 years of
catches in comparison with OFL, ABC, HG, and OY/ACL values,
overfishing levels,
actual catch and discard.
Include OFL (encountered), OFL (retained), and OFL (dead) if different due to discard and discard mortality.


\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{c>{\centering\arraybackslash}p{2cm}>{\centering\arraybackslash}p{2cm}>{\centering\arraybackslash}p{2cm}}
\caption{(\#tab:es-ca-management)The portion of the Overfishing Limit (OFL) and Annual Catch Limit (ACL) and estimated catch in California waters.}\\
\toprule
Year & OFL (mt) & ACL (mt) & Catch (mt)\\
\midrule
\endfirsthead
\caption[]{(\#tab:es-ca-management)The portion of the Overfishing Limit (OFL) and Annual Catch Limit (ACL) and estimated catch in California waters. \textit{(continued)}}\\
\toprule
Year & OFL (mt) & ACL (mt) & Catch (mt)\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2012 & 163.15 & 136.17 & 85.95\\
2013 & 148.00 & 123.42 & 105.18\\
2014 & 148.00 & 123.42 & 98.65\\
2015 & 303.75 & 277.32 & 147.64\\
2016 & 286.88 & 261.95 & 165.27\\
2017 & 313.70 & 286.38 & 225.48\\
2018 & 319.60 & 291.85 & 203.69\\
2019 & 325.08 & 296.83 & 182.59\\
2020 & 330.35 & 301.60 & 242.73\\
2021 & 249.85 & 206.43 & 164.20\\
2022 & 249.48 & 204.02 & 66.67\\*
\end{longtable}
\endgroup{}
\endgroup{}


## Unresolved problems and major uncertainties{-}



shared text

## Decision table and projections{-}

Replace text with
projected yields (OFL, ABC, and ACL), spawning biomass, and stock depletion levels for each year.
OFL calculations should be based on the assumption that future catches equal ABCs and not OFLs.


\begingroup\fontsize{10}{12}\selectfont

\begin{landscape}\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{c>{\centering\arraybackslash}p{1.38cm}>{\centering\arraybackslash}p{1.38cm}>{\centering\arraybackslash}p{1.38cm}>{\centering\arraybackslash}p{1.38cm}>{\centering\arraybackslash}p{1.38cm}>{\centering\arraybackslash}p{1.38cm}>{\centering\arraybackslash}p{1.38cm}}
\caption{(\#tab:es-ca-proj)The estimated spawning output in number of million eggs across California and fraction unfished by year.}\\
\toprule
Year & Adopted OFL (mt) & Adopted ABC (mt) & Assumed Catch (mt) & OFL (mt) & ABC (mt) & Spawning Biomass & Fraction Unfished\\
\midrule
\endfirsthead
\caption[]{(\#tab:es-ca-proj)The estimated spawning output in number of million eggs across California and fraction unfished by year. \textit{(continued)}}\\
\toprule
Year & Adopted OFL (mt) & Adopted ABC (mt) & Assumed Catch (mt) & OFL (mt) & ABC (mt) & Spawning Biomass & Fraction Unfished\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2023 & 116.4 & 91.53 & 70 & - & - & 331.05 & 0.481\\
2024 & 121.32 & 94.69 & 70 & - & - & 331.91 & 0.482\\
2025 & - & - & - & 182.58 & 169.79 & 331.76 & 0.482\\
2026 & - & - & - & 182.54 & 169.45 & 330.85 & 0.481\\
2027 & - & - & - & 182.16 & 169.11 & 329.74 & 0.479\\
2028 & - & - & - & 181.54 & 168.81 & 328.80 & 0.478\\
2029 & - & - & - & 180.82 & 168.58 & 328.09 & 0.477\\
2030 & - & - & - & 180.13 & 168.42 & 327.58 & 0.476\\
2031 & - & - & - & 179.53 & 168.35 & 327.20 & 0.475\\
2032 & - & - & - & 179.02 & 168.33 & 326.90 & 0.475\\
2033 & - & - & - & 178.61 & 168.37 & 326.66 & 0.474\\
2034 & - & - & - & 178.28 & 168.44 & 326.45 & 0.474\\*
\end{longtable}
\endgroup{}
\end{landscape}
\endgroup{}



## Scientific uncertainty{-}


The model estimated uncertainty around the 2023 spawning output was $\sigma$ = 0.22 and the uncertainty around the OFL was $\sigma$ = 0.22. This is likely an underestimate of overall uncertainty because of the necessity to fix several population dynamic parameters (e.g., steepness, recruitment variance, female natural mortality) and no explicit incorporation of model structural uncertainty (although see the decision table for alternative states of nature).

## Research and data needs{-}


shared text

<!--chapter:end:01executive.Rmd-->

# Introduction
## Basic Information and Life History
This assessment reports the status of copper rockfish (*Sebastes caurinus*) off the California coast, north of Point Conception, using data through 2022.


 Copper rockfish have historically been a part of both commercial and recreational 
fisheries throughout its range. Copper rockfish are a demersal, relatively nearshore 
species within the subgenus *Pteropodus.* Copper rockfish range from xxx to xx at depth of   xxx [@love_rockfishes_2002].  The core range is 
comparatively large, from northern Baja Mexico to the Gulf of Alaska, with copper rockfish 
also found in Puget Sound. Copper rockfish are commonly found in waters less than 100 meters in depth 
inhabiting nearshore kelp forests and complex low-relief rocky habitat [@love_probably_1996].
Adult copper rockfish have high site fidelity and do not make long-range movements. 
An acoustic telemetry study displaced copper rockfish 4km from their capture location to an artificial 
reef and within 10 days, half of the copper rockfish returned to the original capture location 
[@reynolds_application_2010]. 

long the posterior two-thirds of the lateral line. The copper rockfish has high variation 
in coloration throughout its range, taking on coloration from dark brown, olive, 
orage-red and pink, with patches of yellow and pink [@miller_guide_1972]. In general 
the copper rockfish rockfish towards the northern part of the range are often darker in 
color than fish encountered in southern California. The distinct change in coloration 
resulted in copper rockfish described as two separate species, copper rockfish
(*S. caurinus*) and whitebelly rockfish (*S. vexillaris*). 

The *Sebastes* genus are viviparous with internal fertilization, many exhibit 
dimorphic growth with females larger at size-at-age than males, and a number 
of species have reproductive strategies that vary with latitude. There 
are very few fecundity samples from copper rockfish available from available from California, 
although copper rockfish are assumed to produce a single brood annually. In southern California, 
peak parturition occurs xxxx central California peak parturition occurs 
in February and March.   

The pelagic larvae are encountered in the CalCOFI surveys, but neither larval 
nor young-of-the-year (YOY) can be identified copper rockfish visually [@thompson_larval_2017]. The
size at birth ranges from 5-6 mm and the larvae remain pelagic until approximately 22-23 mm 
standard length at which time they recruit to the kelp forest canopy [@anderson_identification_1983].

Juvenile Copper rockfish are indistinguishable from kelp (*S. atrovirens*), black-and-yellow 
(*S. chrysomelas*), and gopher rockfish (*S. carnatus*), all of which recruit to the 
kelp forest canopy in the spring months. Copper rockfish is the first of the species group to 
recruit to the kelp forest from April to May and can be distinguished from the other 
species once it reaches a size around 50 mm standard length [@anderson_identification_1983]. 
Baetscher genetically identified YOY rockfish from surveys in Carmel and Monterey Bays 
in California and provided the authors with the length and genotyped species idenifications 
from her study [@baetscher_dispersal_2019]. The average length of copper rockfish in July was 3-4 cm 
total length \ref{fig:copper-smurf-length}. Anderson observed benthic copper rockfish 
nocturnally active over sandy bottom outside the kelp forest [@anderson_identification_1983].

Copper rockfish are a relatively long-lived rockfish, estimated to live at least 50 years
[@love_milton_probably_1996]. Copper rockfish was determined to have the highest 
vulnerability (V = 2.27) of any West Coast groundfish stock evaluated in a 
productivity susceptibility analysis [@cope_approach_2011]. This analysis
calculated species-specific vulnerability scores based on two dimensions: 
productivity characterized by the life history and susceptibility that
characterized how the stock could be impacted by fisheries and other activities. 
 
 As adults, there is little evidence of movement, with  Hanan and CCFRP citations

copper rockfish are opportunistic carnivores and commonly consume crustaceans, mollusks, 
and fish whole [@lea_biological_1999; @bizzarro_diet_2017]. Prince -@prince_food_1972 
observed a shift in a diet dominated by arthropods in age 0 and 1 fish, and a shift to a more 
diverse diet including molluscs and fish as they aged. the study also noted that 
juvenile copper rockfish were predated on by harbor seals and lingcod.
 
There is currently no evidence of significant stock structure from genetic 
studies of copper rockfish across the west coast. -@buonaccorsi_population_2002 looked 
at genetic variation across six micosatellite 
DNA loci from samples ranging from British Columbia to southern California. 
Significant population subdivision was detected between th Puget Sound and coastal 
samples and support the model of isolation-by-distance for copper rockfish.
@sivasundar_life_2010 conducted a genetic study to determine the potential for 
biogeographic boundaries to prohibit gene flow for 15 *Sebastes* species.
The study's sample sizes of 
copper rockfish with samples form Oregon, Monterey Bay and Santa Barbara. @sivasundar_life_2010
used mtDNA and could differentiate samples from Santa Barbara from those collected 
in Oregon and Monterey Bay, but the Monterey Bay and Oregon samples could not be distinguished. 
Micosatellite data did not reveal any genetic differentiation among the sampels from 
the three locations for copper rockfish and suggests low genetic differentiation coastwide. 

The most recent genetic analysis of copper rockfish to date was conducted by 
 @johansson_influence_2008. The study included 
749 samples from along the west coast ranging from Neah Bay, Washington to 
San Diego, California with the majority of sampling locations clustered north 
of Cape Mendocino in northern California.  The study included 185 samples collected 
within California. Eleven microsatellite 
DNA loci were analyzed. The study found significant evidence to support isolation 
by distance at the coast wide scale. Weak, but significant, genetic structure was 
identified from samples collected 
along the Oregon coast suggesting that habitat barriers may limit larval dispersal. 

## Ecosystem Considerations
Replace text.

## Historical and Current Fishery Information

Off the coast of California, north of Point Conception, copper rockfish is caught in both commercial and recreational fisheries. Recreational removals have been the largest source of fishing mortality, comprising nearly 85 percent of total removals of copper rockfish across all years (Table XX and Figure XX). The landings from the commercial fishery have been minimal by year, expect for a brief period between the mid-1980s and early-2000s. 


The recreational fishery in the early part of the 20th century was focused on nearshore waters near ports, with expanded activity further from port and into deeper depths over time [@miller_spatially_2014]. Prior to the groundfish fishery being declared a federal disaster in 2000, and the subsequent rebuilding period, there were no time or area closures for groundfish. Access to deeper depths during this period spread effort over a larger area and filled bag limits with a greater diversity of species from both the shelf and nearshore. This resulted in lower catch of nearshore rockfish relative to the period after 2000 when 20 to 60 fm depth restrictions ranging from 20 fm in the Northern Management Area to 60 fm in the Southern Management Area were put in place in various management area delineations along the state. This shifting effort onto the nearshore, concomitantly increased catch rates for nearshore rockfish including copper rockfish in the remaining open depths, though season lengths were greatly curtailed.   

Following  all previously overfished groundfish species, other than yelloweye rockfish, being declared rebuilt by 2019, deeper depth restrictions were offered in the Southern Management area allowing resumed access to shelf rockfish in less than 75 fm and are currently 100 fm as of 2021. The increased access to deeper depths south of Point Conception with the rebuilding of cowcod is expected to reduce the effort in nearshore waters where copper rockfish is most prevalent.  To the north of Point Conception where yelloweye rockfish are prevalent, depth constraints persist and effort remains focused on the nearshore in 30 to 50 fm depending on the management area.  As yelloweye rockfish continues to rebuild, incremental increases in access to deeper depths are expected, which will likely further reduce the effort in nearshore waters where copper rockfish is most prevalent.

Prior to development of the live fish market in the 1980s, there was very little commercial catch of copper rockfish, with dead copper rockfish fetching a low ex-vessel price per pound. Copper rockfish were targeted along with other rockfish to some degree in the nearshore or caught as incidental catch by vessels targeting other more valuable stocks such as lingcod.  Most fish were caught using hook and line gear, though some were caught using traps, gill nets and, rarely, trawl gear. Trawling was prohibited within three miles of shore in 1953 and gill netting within three miles of shore was prohibited in 1994, preventing access to a high proportion of the species habitat with these gear types. Copper rockfish were caught along with other rockfish to some degree in the nearshore or caught as bycatch by vessels targeting other more valuable stocks such as lingcod.

In the late 1980s and early 1990s a market for fish landed live arose out of Los Angeles and the Bay area, driven by demand from Asian restaurants and markets. The growth of the live fish market was driven by consumers willing to pay a higher price for live fish, ideally plate-sized (12 - 14 inches or 30.5 - 35.6 cm). Live fish landed for the restaurant market are lumped into two categories, small (1 - 3 lbs.) or large (3 - 6 lbs.), with small, plate-sized, fish fetching higher prices at market ranging between $5 -7 per fish (Bill James, personal communication). Copper rockfish is one of the many rockfish species that is included in the commercial live fish fishery. The proportion of copper rockfish being landed live vs. dead since 2000 by California commercial fleets ranges between 50 to greater than 70 percent in the southern and northern areas, respectively. 
 
With the development and expansion of the nearshore live fish fishery during the 1980s and 1990s, new entrants in this open access fishery were drawn by premium ex-vessel price per pound for live fish, resulting in over-capitalization of the fishery. Since 2002, the California Department of Fish and Wildlife (CDFW) has managed 19 nearshore species in accordance with Nearshore Fisheries Management Plan [@wilson-vandenberg_implementing_2014]. In 2003, the CDFW implemented a Nearshore Restricted Access Permit system, including the requirement of a Deeper Nearshore Fishery Species Permit to retain copper rockfish, with the overall goal of reducing the number of participants to a more sustainable level, with permit issuance based on historical landings history by the retrospective qualifying date.  The result was a reduction in permits issued from 1,127 in 1999 to 505 in 2003, greatly reducing catch levels. In addition, reduced trip limits, season closures in March and April and depth restrictions were implemented to address bycatch of overfished species and associated constraints from their low catch limits.   
 
 

Copper rockfish residing between Point Conception and the California/Oregon border are assessed here as a single, separate stock (Figure \ref{fig:map}). This designation was made based on oceanographic, geographic, and fishery conditions. The copper rockfish population in California waters was split at Point Conception due to water circulation patterns that create a natural barrier between nearshore rockfish populations to the north and south. The northern border for this assessment was defined as the California/Oregon border due to substantial differences in historical and current exploitation levels. Additionally, the fairly sedentary nature of adult copper rockfish, likely limits flow of fish between northern California and areas to the north. 

## Summary of Management History and Performance
Replace text.

## Foreign Fisheries
Replace text.

<!--chapter:end:11introduction.Rmd-->

# Data

Data comprise the foundational components of stock assessment models.
The decision to include or exclude particular data sources in an assessment model depends on many factors.
These factors often include, but are not limited to,
the way in which data were collected (e.g., measurement method and consistency);
the spatial and temporal coverage of the data;
the quantity of data available per desired sampling unit;
the representativeness of the data to inform the modeled processes of importance;
timing of when the data were provided;
limitations imposed by the Terms of Reference; and
the presence of an avenue for the inclusion of the data in the assessment model.
Attributes associated with a data source can change through time,
as can the applicability of the data source when different modeling approaches are explored (e.g., stock structure or time-varying processes).
Therefore, the specific data sources included or excluded from this assessment should not necessarily constrain the selection of data sources applicable to future stock assessments for copper rockfish.
Even if a data source is not directly used in the stock assessment they can provide valuable insights into biology, fishery behavior, or localized dynamics.

Data from a wide range of programs were available for possible inclusion in the current assessment model.
Descriptions of each data source included in the model (Figure \@ref(fig:data-plot)) and sources that were explored but not included in the base model are provided below.
Data that were excluded from the base model were explicitly explored during the development of this stock assessment or have not changed since their past exploration in a previous copper rockfish stock assessment.
In some cases, the inclusion of excluded data sources were explored through sensitivity analyses (see Section \@ref(assessment-model)).

<!--chapter:end:20data.Rmd-->

# Assessment Model


<!--chapter:end:30model.Rmd-->

