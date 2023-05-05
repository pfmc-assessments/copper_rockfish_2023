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
bibilography: [pfmcassess.bib, wcassess.bib]
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

This assessment reports the status of copper rockfish (*Sebastes caurinus*) off the California coast in U.S. waters, using data through 2022. The copper rockfish stock was assessed using two sub-area models that captured distinct inter-stock dynamics split north and south of Point Conception. The estimated dynamics for each assessed sub-area is described here along with the combined stock status for the California stock. This assessment does not account for populations located in Mexico waters or other areas off the U.S. coast and assumes that these southern and northern populations do not contribute to the population being assessed here. 


## Catches{-}

Replace text with
trends and current levels.
Include Table for last 10 years.
Include Figure with long-term estimates.


\input{tex_tables/south_a_Catches_ES.tex}

\input{tex_tables/north_a_Catches_ES.tex}



![Landings by fleet used in the base model for the area south of Point Conception where catches in metric tons by fleet are stacked.\label{fig:es-south-catch}](S:/copper_rockfish_2023/models/sca/5.5_est_m/plots/catch2 landings stacked.png){width=100% height=100% alt="."}




![Landings by fleet used in the base model for the area north of Point Conception where catches in metric tons by fleet are stacked.\label{fig:es-north-catch}](S:/copper_rockfish_2023/models/nca/8.5_update_deb_index/plots/catch2 landings stacked.png){width=100% height=100% alt="."}


## Data and assessment{-}

This assessment uses the stock assessment framework
Stock Synthesis version 3.30.21 (SS3).

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

\input{tex_tables/south_b_SSB_ES.tex}

\input{tex_tables/north_b_SSB_ES.tex}

\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{c>{\centering\arraybackslash}p{2cm}>{\centering\arraybackslash}p{2cm}}
\caption{(\#tab:ca-status)The estimated spawning ouput in number of million eggs across California and fraction unfished by year.}\\
\toprule
Year & Spawning Output & Fraction Unfished\\
\midrule
\endfirsthead
\caption[]{(\#tab:ca-status)The estimated spawning ouput in number of million eggs across California and fraction unfished by year. \textit{(continued)}}\\
\toprule
Year & Spawning Output & Fraction Unfished\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2013 & 227.72 & 0.331\\
2014 & 243.57 & 0.354\\
2015 & 264.67 & 0.384\\
2016 & 283.21 & 0.411\\
2017 & 299.59 & 0.435\\
2018 & 308.56 & 0.448\\
2019 & 316.83 & 0.460\\
2020 & 322.83 & 0.469\\
2021 & 319.07 & 0.463\\
2022 & 320.42 & 0.465\\
2023 & 331.05 & 0.481\\*
\end{longtable}
\endgroup{}
\endgroup{}



![Estimated time series of spawning output (circles and line: median; light broken lines: 95 percent intervals) for the model areas south and north of Point Conception.\label{fig:es-sb}](C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023/documents/shared_figures/compare2_spawnbio_uncertainty.png){width=100% height=100% alt="."}



![Estimated time series of fraction of unfished spawning output (circles and line: median; light broken lines: 95 percent intervals) for the model areas south and north of Point Conception.\label{fig:es-depl}](C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023/documents/shared_figures/compare4_Bratio_uncertainty.png){width=100% height=100% alt="."}


\clearpage

## Recruitment{-}

Replace text with
trends and current levels relative to virgin or historic levels and
description of uncertainty.
Include Table for last 10 years.
Include Figure with long-term estimates.

\input{tex_tables/south_c_Recr_ES.tex}

\input{tex_tables/north_c_Recr_ES.tex}


![Estimated time series of age-0 recruits (1000s) for the model areas south and north of Point Conception with 95 percent intervals.\label{fig:es-recruits}](C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023/documents/shared_figures/compare10_recruits_uncertainty.png){width=100% height=100% alt="."}



![Estimated time series of recruitment deviations for the model areas south and north of Point Conception.\label{fig:es-rec-devs}](C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023/documents/shared_figures/compare12_recdevs_uncertainty.png){width=100% height=100% alt="."}

\clearpage

## Exploitation status{-}

Replace text with
total catch divided by exploitable biomass or SPR harvest rate.
Include Table for last 10 years.
Include Figure with trend in f relative to target vs. trend in biomass relative to the target.

\input{tex_tables/south_d_SPR_ES.tex}

\input{tex_tables/north_d_SPR_ES.tex}


![Estimated 1 - relative spawning ratio (SPR) by year for the model areas south and north of Point Conception. The management target is plotted as a red horizontal line and values above this reflect harvest in excess of the proxy harvest rate.\label{fig:es-1-spr}](C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023/documents/shared_figures/compare6_SPRratio_uncertainty.png){width=100% height=100% alt="."}

## Ecosystem considerations{-}


shared text

## Reference points{-}

Replace text with
management targets and definition of overfishing, including the harvest rate that brings the stock to equilibrium at $B_{40\%}$, i.e., the $B_{MSY}$ proxy and the equilibrium stock size that results from fishing at the default harvest rate, i.e., the $F_{MSY}$ proxy.
Include Table of estimated reference points for ssb, SPR, exploitation rate, and yield based on SSB proxy for MSY, SPR proxy for MSY, and estimated MSY values.

\input{tex_tables/south_e_ReferencePoints_ES.tex}

\input{tex_tables/north_e_ReferencePoints_ES.tex}



![Phase plot of estimated 1-SPR versus fraction unfished for the model areas south and north of Point Conception.\label{fig:es-phase}](C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023/documents/shared_figures/compare15_phase_plot.png){width=100% height=100% alt="."}



![Equilibrium yield curve for the base case model for model south of Point Conception. Values are based on the 2022 fishery selectivities and with steepness fixed at 0.72.\label{fig:south-es-yield}](S:/copper_rockfish_2023/models/sca/5.5_est_m/plots/yield2_yield_curve_with_refpoints.png){width=100% height=100% alt="."}




![Equilibrium yield curve for the base case model for model north of Point Conception. Values are based on the 2022 fishery selectivities and with steepness fixed at 0.72.\label{fig:north-es-yield}](S:/copper_rockfish_2023/models/nca/8.5_update_deb_index/plots/yield2_yield_curve_with_refpoints.png){width=100% height=100% alt="."}



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
2023 & 116.4 & 91.53 & 91.5 & - & - & 331.05 & 0.481\\
2024 & 121.32 & 94.69 & 94.7 & - & - & 331.91 & 0.482\\
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
This assessment report describes the sub-area population of copper rockfish (*Sebastes caurinus*) off the California coast south of Point Conception in U.S. waters, using data through 2022. The sub-area population north of Point Conception in California waters was also evaluated and is described in a separate assessment report. The copper rockfish status for the California stock of is determined by the combined estimates of spawning output from both sub-areas and is detailed in the [management](#management) section. This assessment does not account for populations located in Mexico waters or other areas off the U.S. coast and assumes that these southern and northern populations do not contribute to the population being assessed here. 


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
orange-red and pink, with patches of yellow and pink [@miller_guide_1972]. In general 
the copper rockfish towards the northern part of the range are often darker in 
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
[@love_probably_1996]. Copper rockfish was determined to have the highest 
vulnerability (V = 2.27) of any West Coast groundfish stock evaluated in a 
productivity susceptibility analysis [@cope_approach_2011]. This analysis
calculated species-specific vulnerability scores based on two dimensions: 
productivity characterized by the life history and susceptibility that
characterized how the stock could be impacted by fisheries and other activities. 
 
 As adults, there is little evidence of movement, with  Hanan and CCFRP citations

Copper rockfish are opportunistic carnivores and commonly consume crustaceans, mollusks, 
and fish whole [@lea_biological_1999; @bizzarro_diet_2017]. -@prince_food_1972 
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



This stock assessment does not explicitly incorporate trophic interactions, habitat factors
(other than as they inform relative abundance indices) or environmental factors into the assessment model, but a brief description of likely or potential ecosystem considerations are
provided below.

As with most other rockfish and groundfish in the California Current, recruitment, or
cohort (year-class) strength appears to be highly variable for the copper rockfish complex, with only a modest apparent relationship to estimated levels of spawning output. Oceanographic and ecosystem factors are widely recognized to be key drivers of recruitment variability for most species of groundfish, as well as most elements of California Current food webs. Empirical estimates of recruitment from pelagic juvenile rockfish surveys have been used to inform incoming year class strength for some of these stocks, however copper rockfish are infrequently encountered in these surveys. Between 1998 and 2013 the California Cooperative Oceanic Fisheries Investigation (CalCOFI) survey observed had 34 positive observations copper rockfish out of nearly 300,000 total juvenile *Sebastes* encountered in juvenile surveys. 


## Historical and Current Fishery Information

Off the coast of California south of Point Conception copper rockfish is caught in both commercial and recreational fisheries. Recreational removals have been the largest source of fishing mortality of copper rockfish across all years (Table \ref{tab:allcatches} and Figure \ref{fig:catch}). The recreational fishery is comprised of individual recreational fishers (Private/Rental, PR) and charter recreational private vessels (CPFV) which take groups of individuals out for day fishing trips. Across both types of recreational fishing the majority of effort occurs around rocky reefs that can be accessed via a day-trips. 


The recreational fishery in the early part of the 20th century was focused on nearshore waters near ports, with expanded activity further from port and into deeper depths over time [@miller_spatially_2014]. Prior to the groundfish fishery being declared a federal disaster in 2000, and the subsequent rebuilding period, there were no time or area closures for groundfish. Access to deeper depths during this period spread effort over a larger area and filled bag limits with a greater diversity of species from both the shelf and nearshore. This resulted in lower catch of nearshore rockfish relative to the period after 2000 when 20 to 60 fm depth restrictions ranging from 20 fm in the Northern Management Area to 60 fm in the Southern Management Area were put in place in various management area delineations along the state. This shifting effort onto the nearshore, concomitantly increased catch rates for nearshore rockfish including copper rockfish in the remaining open depths, though season lengths were greatly curtailed.   

Following  all previously overfished groundfish species, other than yelloweye rockfish, being declared rebuilt by 2019, deeper depth restrictions were offered in the Southern Management area allowing resumed access to shelf rockfish in less than 75 fm and are currently 100 fm as of 2021. The increased access to deeper depths south of Point Conception with the rebuilding of cowcod is expected to reduce the effort in nearshore waters where copper rockfish is most prevalent.  To the north of Point Conception where yelloweye rockfish are prevalent, depth constraints persist and effort remains focused on the nearshore in 30 to 50 fm depending on the management area.  As yelloweye rockfish continues to rebuild, incremental increases in access to deeper depths are expected, which will likely further reduce the effort in nearshore waters where copper rockfish is most prevalent.

Prior to development of the live fish market in the 1980s, there was very little commercial catch of copper rockfish, with dead copper rockfish fetching a low ex-vessel price per pound. Copper rockfish were targeted along with other rockfish to some degree in the nearshore or caught as incidental catch by vessels targeting other more valuable stocks such as lingcod.  Most fish were caught using hook and line gear, though some were caught using traps, gill nets and, rarely, trawl gear. Trawling was prohibited within three miles of shore in 1953 and gill netting within three miles of shore was prohibited in 1994, preventing access to a high proportion of the species habitat with these gear types. Copper rockfish were caught along with other rockfish to some degree in the nearshore or caught as bycatch by vessels targeting other more valuable stocks such as lingcod.

In the late 1980s and early 1990s a market for fish landed live arose out of Los Angeles and the Bay area, driven by demand from Asian restaurants and markets. The growth of the live fish market was driven by consumers willing to pay a higher price for live fish, ideally plate-sized (12 - 14 inches or 30.5 - 35.6 cm). Live fish landed for the restaurant market are lumped into two categories, small (1 - 3 lbs.) or large (3 - 6 lbs.), with small, plate-sized, fish fetching higher prices at market ranging between $5 -7 per fish (Bill James, personal communication). Copper rockfish is one of the many rockfish species that is included in the commercial live fish fishery. The proportion of copper rockfish being landed live vs. dead since 2000 by California commercial fleets ranges between 50 to greater than 70 percent in the southern and northern areas, respectively. 
 
With the development and expansion of the nearshore live fish fishery during the 1980s and 1990s, new entrants in this open access fishery were drawn by premium ex-vessel price per pound for live fish, resulting in over-capitalization of the fishery. Since 2002, the California Department of Fish and Wildlife (CDFW) has managed 19 nearshore species in accordance with Nearshore Fisheries Management Plan [@wilson-vandenberg_implementing_2014]. In 2003, the CDFW implemented a Nearshore Restricted Access Permit system, including the requirement of a Deeper Nearshore Fishery Species Permit to retain copper rockfish, with the overall goal of reducing the number of participants to a more sustainable level, with permit issuance based on historical landings history by the retrospective qualifying date.  The result was a reduction in permits issued from 1,127 in 1999 to 505 in 2003, greatly reducing catch levels. In addition, reduced trip limits, season closures in March and April and depth restrictions were implemented to address bycatch of overfished species and associated constraints from their low catch limits.   
 
 

Copper rockfish residing between Point Conception and the California/Oregon border are assessed here as a single, separate stock (Figure \ref{fig:ca-map}). This designation was made based on oceanographic, geographic, and fishery conditions. The copper rockfish population in California waters was split at Point Conception due to water circulation patterns that create a natural barrier between nearshore rockfish populations to the north and south. The northern border for this assessment was defined as the California/Oregon border due to substantial differences in historical and current exploitation levels. Additionally, the fairly sedentary nature of adult copper rockfish, likely limits flow of fish between northern California and areas to the north. 

## Summary of Management History and Performance
Replace text.

## Foreign Fisheries
Replace text.

<!--chapter:end:11introduction.Rmd-->

# Appendix F. CCFRP Index of Abundance {#ccfrp-index}



**California Collaborative Fisheries Research Program Index**

The California Collaborative Fisheries Research Program, [CCFRP](https://www.mlml.calstate.edu/ccfrp/), 
is a fishery-independent 
hook-and-line survey designed to monitor nearshore fish populations at a series of sampling 
locations both inside and adjacent to MPAs 
[@starr_variation_2015a; @wendt_collaborative_2009].  The CCFRP survey began in 
2007 along the central coast of California and was designed in collaboration 
with academics, NMFS scientists and fishermen. 
From 2007-2016 the CCFRP project was focused on the central California coast,
and has monitored four MPAs consistently. In 2017, the CCFRP expanded coastwide within California.  
The index of abundance was developed from the four MPAs sampled consistently 
(A&ntilde;o Nuevo and Point Lobos 
by Moss Landing Marine Labs; Point Buchon and Piedras Blancas by Cal Poly).

The survey design for CCFRP consists 500 x 500 m cells both within and 
adjacent to each MPA. On any given survey day site cells are randomly 
selected within a stratum (MPA and/or reference cells).  CPFVs are chartered 
for the survey and the fishing captain is allowed to search within the cell for 
a fishing location.  During a sampling event, each cell is fished for a total of 
30-45 minutes by volunteer anglers. Each fish encountered is recorded, measured, 
and can be linked back to a particular angler, and released (or descended to depth). 
 CCFRP samples shallower depths to avoid barotrauma-induced mortality.  
 Starting in 2017, a subset of fish have been retained to collect otoliths and fin 
clips that provide needed biological information for nearshore species. For the 
index of abundance, CPUE was modeled at the level of the drift, similar to the 
fishery-dependent onboard observer survey described above.


*CCFRP Index: Data Preparation, Filtering, and Sample Sizes*

The CCFRP data are quality controlled at the time they are key punched and little 
filtering was needed for the index. 
Cells not consistently sampled over time were excluded as well as cells that never 
encountered copper rockfish.
The full dataset for northern California contained 8,770 drifts, 23% of which encountered 
copper rockfish.  After applying filters to remove drfits from sites that were not consistently sampled,
marked for exclusion in the data, or did not fish a minimum of xxx, 7,078 drifts remained for 
for index standardization, with 1,757 drifts encountering copper rockfish.
  

*CCFRP Index: Model Selection, Fits, and Diagnostics*

The CCFRP index includes all of the 
MPAs currently sampled from 2017-2020 and the core central California sampling sites 
from 2007-2016. Trends among all of the MPAs sampled increased along the entire coast 
from 2017-2020. The final index (Table \@ref(tab:tab-index-ccfrp)) 
represents a similar trend to the arithmetic mean of the annual CPUE (Figure \@ref(fig:fig-cpue-ccfrp)).

We modeled retained catch per angler hour (CPUE; number of fish per angler hour) 
using MLE fr. Indices with a year and area (location along the coast) interaction were not 
considered in model selection; trends in the average CPUE by region were similar 
in the filtered data set (Figure \@ref(fig:fig-areacpue-ccfrp)). Plots of the arithmetic 
mean by inside (MPA) and outside (REF) MPAs over time is in Figure \@ref(fig:fig-sitecpue-ccfrp) 
and shows the distinct trends of increasing average CPUE over time.


A negative binomial model was fit to the drift-level data (catch with a log offset for angler 
hours). Because the average observed CPUE inside MPAs and in the reference sites exhibited 
differing trends, we explored a YEAR:SITE interaction, which was selected as the best 
fit model by AIC Table \@ref(tab:tab-model-select-ccfrp)), The final model included
yrea, mpa/reference categorization, depth, depth squared, and a year:mpa/reference interaction. 
The model was fit using the sdmTMB R package (version xxx1). 


Based on work completed at the SWFSC, we estimate that the percent of rocky reef habitat from Point Conception to the California border within California state waters is 892 $km^2$, of which approximately 23% is in MPAs that prohibit the harvest of groundfish (pers comm. Rebecca Miller, UCSC). There is recreational fishing outside of state waters, but habitat maps are not available at the same 2-m resolution and do not allow for direct comparisons. To estimate the area 
of rocky substrate south of Point conception, we separted the southern California Bight into four areas, 1) CRFS District 
1 along the mainland coast, 2) CRFS District 2 along the mainland coast, 3) state waters encompassing the southern 
Channel Islands, and 4) state waters encompassing the northern Channel Islands.  We calculated the total area in each of the 
four regions, as well as the total area with available interpretted substrate.  By also calculating the total area open and closed to fishing, i.e., MPAs and CCAs, we expanded the known fraction of rocky substrate to the areas within state 
waters where no substrated interpretted maps exist.  This resulted in an estimate of 27% of the available rocky substrate 
within closed areas to fishing in southern California state waters. 

The final index was weighted, giving 20% of the model weight to MPAs and 80% of model 
weight to the "open" areas within the state.



\begingroup\fontsize{7}{9}\selectfont

\begin{landscape}\begingroup\fontsize{7}{9}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{2cm}>{\raggedright\arraybackslash}p{2cm}>{\raggedright\arraybackslash}p{2cm}}
\caption{(\#tab:ccfrp-data-filter)Data filtering for the CCFRP survey.}\\
\toprule
Filter & Description & Samples & Positive\_Samples\\
\midrule
\endfirsthead
\caption[]{(\#tab:ccfrp-data-filter)Data filtering for the CCFRP survey. \textit{(continued)}}\\
\toprule
Filter & Description & Samples & Positive\_Samples\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
All data &  & 8770 & 1979\\
Sampling frequency & Remove locations and cells not well 
                                          sampled and drifts marked for exclusion & 7850 & 1773\\
Location & Remove grid cells that never observed
                                           the target species & 7205 & 1773\\
Time fished & Remove drifts less than two minutes 
                                          and cells fished less than 15 minutes
                                          during a sampling event & 7078 & 1757\\*
\end{longtable}
\endgroup{}
\end{landscape}
\endgroup{}

\newpage

\begingroup\fontsize{7}{9}\selectfont

\begin{landscape}\begingroup\fontsize{7}{9}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}>{\raggedright\arraybackslash}p{1cm}}
\caption{(\#tab:ccfrp-model-selection)Model selection for the CCFRP survey.}\\
\toprule
Depth & Depth.Squared & Mpaorref & Region & Year & Interaction & Effort.Offset & Df & Log.Likelihood & AICc & Delta\\
\midrule
\endfirsthead
\caption[]{(\#tab:ccfrp-model-selection)Model selection for the CCFRP survey. \textit{(continued)}}\\
\toprule
Depth & Depth.Squared & Mpaorref & Region & Year & Interaction & Effort.Offset & Df & Log.Likelihood & AICc & Delta\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
0.402 & -0.008 & Included & Included & Included & Included & Included & 36 & -5319.3 & 10710.9 & 0.0\\
0.393 & -0.008 & Included & Excluded & Included & Included & Included & 35 & -5321.0 & 10712.3 & 1.4\\
0.406 & -0.008 & Included & Included & Included & Excluded & Included & 21 & -5351.1 & 10744.4 & 33.5\\
0.397 & -0.008 & Included & Excluded & Included & Excluded & Included & 20 & -5353.0 & 10746.1 & 35.2\\
0.145 & Excluded & Included & Excluded & Included & Included & Included & 34 & -5350.2 & 10768.8 & 57.9\\
0.144 & Excluded & Included & Included & Included & Included & Included & 35 & -5350.1 & 10770.5 & 59.6\\
0.143 & Excluded & Included & Excluded & Included & Excluded & Included & 19 & -5383.4 & 10804.9 & 94.0\\
0.143 & Excluded & Included & Included & Included & Excluded & Included & 20 & -5383.2 & 10806.5 & 95.6\\
0.464 & -0.010 & Excluded & Included & Included & Excluded & Included & 20 & -5508.1 & 11056.3 & 345.4\\
0.454 & -0.010 & Excluded & Excluded & Included & Excluded & Included & 19 & -5510.5 & 11059.2 & 348.3\\
0.144 & Excluded & Excluded & Excluded & Included & Excluded & Included & 18 & -5554.0 & 11144.1 & 433.2\\
0.144 & Excluded & Excluded & Included & Included & Excluded & Included & 19 & -5553.8 & 11145.6 & 434.7\\
Excluded & Excluded & Included & Excluded & Included & Included & Included & 33 & -5632.6 & 11331.5 & 620.6\\
Excluded & Excluded & Included & Included & Included & Included & Included & 34 & -5632.2 & 11332.7 & 621.8\\
Excluded & Excluded & Included & Excluded & Included & Excluded & Included & 18 & -5661.2 & 11358.4 & 647.5\\
Excluded & Excluded & Included & Included & Included & Excluded & Included & 19 & -5660.7 & 11359.5 & 648.6\\
Excluded & Excluded & Excluded & Excluded & Included & Excluded & Included & 17 & -5815.9 & 11665.8 & 954.9\\
Excluded & Excluded & Excluded & Included & Included & Excluded & Included & 18 & -5815.3 & 11666.8 & 955.9\\*
\end{longtable}
\endgroup{}
\end{landscape}
\endgroup{}

\newpage

\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{c>{\centering\arraybackslash}p{2cm}>{\centering\arraybackslash}p{2cm}}
\caption{(\#tab:ccfrp-index)Estimated relative index of abundance for the CCFRP survey.}\\
\toprule
Year & Estimate & logSE\\
\midrule
\endfirsthead
\caption[]{(\#tab:ccfrp-index)Estimated relative index of abundance for the CCFRP survey. \textit{(continued)}}\\
\toprule
Year & Estimate & logSE\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2007 & 0.0582160 & 0.1394863\\
2008 & 0.0275242 & 0.1493542\\
2009 & 0.0599728 & 0.1562757\\
2010 & 0.0329613 & 0.1665564\\
2011 & 0.0302584 & 0.1638784\\
2012 & 0.0359084 & 0.1446754\\
2013 & 0.0237656 & 0.1726645\\
2014 & 0.0495890 & 0.1397864\\
2015 & 0.0371527 & 0.2124289\\
2016 & 0.0962345 & 0.1096466\\
2017 & 0.0920281 & 0.1075274\\
2018 & 0.1107285 & 0.0950086\\
2019 & 0.1284849 & 0.0884973\\
2020 & 0.1693210 & 0.0947559\\
2021 & 0.1546231 & 0.0894429\\
2022 & 0.1363272 & 0.0914945\\*
\end{longtable}
\endgroup{}
\endgroup{}

\newpage


![QQ-plot for the CCFRP survey.\label{fig:ccfrp-qq}](S:/copper_rockfish_2023/data/survey_indices/ccfrp/north/area_weighted/qq.png){width=100% height=100% alt="."}

\newpage 


![Average CPUE by site with trends prior to standardization in the MPA and REF areas.\label{fig:ccfrp-avg-cpue}](S:/copper_rockfish_2023/data/survey_indices/ccfrp/north/mpa_site_cpue.png){width=100% height=100% alt="."}

\newpage


![The weighted relative index of abundance.\label{fig:ccfrp-index}](S:/copper_rockfish_2023/data/survey_indices/ccfrp/north/area_weighted/Index.png){width=100% height=100% alt="."}



<!--chapter:end:65appendix_ccfrp.Rmd-->

