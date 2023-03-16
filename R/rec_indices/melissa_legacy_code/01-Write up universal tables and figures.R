library(ggplot2)

#-------------------------------------------------------------------------------
# define survey label
surveys <- as.data.frame(c(
  "MRFSS_dockside", "WCGBTS", "CRFS_PR_dockside", "CCFRP",
  "DebWV_onboard", "NWFSC_HL", "CA_CPFV_onboard"
))
colnames(surveys) <- "index.subfolder"
surveys$survey.labels <- c(
  "mrfss", "wcgbts", "crfspr", "ccfrp",
  "debwv", "nwfschl", "cpfvonboard"
)

if(params$species.name == "vermilion") {spp1 = "vermilion rockfish"}


#-------------------------------------------------------------------------------
# survey and species specific legend and table levels of regions
# change levels of the subregion for tables and figures

# CPFV onboard
# tab-region  fig-region-cpue
if (params$survey.name == "CA CPFV onboard") {
  legend.labels.region <- if (params$species.name == "lingcod") {
    dat$SubRegion <- dat$DISTRICT
    dat$SubRegion <- as.factor(dat$SubRegion)
  } else if (params$Model.number == 1 && params$species.name == "vermilion") {
    c(
      "CA/OR border to Santa Cruz (V1)",
      "Moss Landing to Big Sur (V2)",
      "San Luis Obsipso to Morro Bay (V3)",
      "South Morro Bay to Point Conception (V4)",
      "Offshore (V5)"
    )
  } else if (params$Model.number == 2 && params$species.name == "vermilion") {
    c("Los Angeles", "Orange", "San Diego", "Santa Barbara", "Ventura")
  } else if (params$Model.number == 2 && params$species.name == "lingcod") {
    c("Southern CA (District 2)", "Central CA (District 3)", "Northern CA (District 4)")
  }
  dat$SubRegion <- as.factor(dat$SubRegion)
  levels(dat$SubRegion) <- legend.labels.region
}

# CCFRP
if (params$survey.name == "CCFRP") {
  legend.labels.ccfrp <- if (params$species.name == "vermilion") {
    c(
      "South Cape Mendocino",
      "Ten Mile",
      "Stewarts Point",
      "Bodega Head",
      "Ano Nuevo",
      "Point Lobos",
      "Piedras Blancas",
      "Point Buchon"
    )
  } else if (params$species.name == "lingcod") {
    c(
      "South Cape Mendocino",
      "Ten Mile",
      "Stewarts Point",
      "Bodega Head",
      "Ano Nuevo",
      "Point Lobos",
      "Piedras Blancas",
      "Point Buchon",
      "Anacapa Island",
      "Carrington Point",
      "South La Jolla"
    )
  }
  dat$SubRegion <- as.factor(dat$AREA)
  levels(dat$SubRegion) <- legend.labels.ccfrp
}

# MRFSS dockside
# for models south of Pt Conception - use county as subregion
if (params$survey.name == "MRFSS dockside") {
  if (params$Model.number == 2) {
    dat$SubRegion <- dat$CNTY
  }
}


# CRFS PR dockside
if (params$survey.name == "CRFS PR dockside") {
  if (Model.number == 1 && params$species.name == "vermilion") {
    dat$SubRegion <- dat$DISTRICT
  } else if (Model.number == 2 && params$species.name == "vermilion") {
    dat$SubRegion <- dat$CNTY
  } else if (Model.number == 2 && params$species.name == "lingcod") {
    dat$SubRegion <- dat$DISTRICT
  } else if (Model.number == 1 && params$species.name == "lingcod") {
    dat$SubRegion <- dat$CNTY
  }
  dat$SubRegion <- as.factor(dat$SubRegion)
}


# WCGBTS uses latitude bins as regions
if (params$survey.name == "WCGBTS") {
  dat$SubRegion <- as.factor(dat$LAT_bin)
}


################################ TABLES ########################################


#-------------------------------------------------------------------------------
# tab-data-filter
# Create the data filter table
if (params$survey.name %in% c(
  "CA CPFV onboard", "CRFS PR dockside", "CCFRP",
  "DebWV onboard"
)) {
  data_filters <- data_filters %>%
    filter(!is.na(Filter)) %>%
    mutate(percent.pos = scales::percent(Positive_Samples / Samples, accuracy = 1))


  table.data.filter <- kable(data_filters,
    booktabs = TRUE,
    align = "llccc",
    caption = paste0(
      "Data filtering steps for the", params$survey.name,
      " survey index for ", spp1, " in the ",
      model.region, ". The last row in the table represents the number of trips used 
      to develop the index."
    ),
    label = paste0("tab-data-filter-", surveys$survey.labels[which(surveys$index.subfolder == params$index.subfolder)]),
    col.names = c(
      "Filter", "Desciption", "Trip", "Positive Trips",
      "Percent drifts retained"
    ),
    row.names = F
  ) %>%
    kable_styling(latex_options = "striped") %>%
    column_spec(1, width = "8em") %>%
    column_spec(4:5, width = "8em") %>%
    column_spec(2, width = "15em")
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# tab-depth
# samples by depth bin
if (params$survey.name %in% c(
  "WCGBTS", "CCFRP", "DebWV onboard",
  "NWFSC HL", "CA CPFV onboard"
)) {
  sample.sizes.depth <- dat %>%
    dplyr::group_by(DEPTH_bin) %>%
    dplyr::summarise(
      Positive.samples = sum(Targetbin),
      Samples = dplyr::n()
    ) %>%
    mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1))
  colnames(sample.sizes.depth) <- c("Year", "Positive Samples", "Samples", "Percent Positive")
  # make table
  table.depth <- kableExtra::kbl(sample.sizes.depth,
    booktabs = TRUE,
    caption = paste(
      "Positive samples of", spp1, "in the",
      model.region, "by depth (fm)."
    ),
    label = paste0("tab-depth-", surveys$survey.labels[which(surveys$index.subfolder == params$index.subfolder)])
  ) %>%
    kable_styling(latex_options = "striped", full_width = FALSE)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# tab-year
# samples by year
sample.sizes.year <- dat %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(
    Positive.samples = sum(Targetbin),
    Samples = dplyr::n()
  ) %>%
  mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1))
colnames(sample.sizes.year) <- c("Year", "Positive Samples", "Samples", "Percent Positive")
# make table
table.year <- kableExtra::kbl(sample.sizes.year,
  booktabs = TRUE,
  caption = paste(
    "Samples of", spp1, "in the",
    model.region, "by year."
  ),
  label = paste0("tab-year-", surveys$survey.labels[which(surveys$index.subfolder == params$index.subfolder)])
) %>%
  kable_styling(latex_options = "striped", full_width = FALSE)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# tab-region
# table of sample sizes by region
sample.sizes.region <- dat %>%
  dplyr::group_by(SubRegion) %>%
  dplyr::summarise(
    Positive.samples = sum(Targetbin),
    Samples = dplyr::n()
  ) %>%
  mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1))

# make table
table.subregion <- kable(sample.sizes.region,
  booktabs = TRUE,
  caption = paste(
    "Samples of", spp1, "in the",
    model.region, "by subregion used in the index."),
    col.names = c("Subregion", "Positive Samples", "Samples", "Percent Positive"),
  label = paste0("tab-region-", 
                 surveys$survey.labels[which(surveys$index.subfolder == params$index.subfolder)])
) %>%
  kable_styling(latex_options = "striped", full_width = FALSE)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# tab-region for districts
# if(!(params$survey.name  %in% c("MRFSS dockside")))
# {
# sample.sizes.district <- dat %>%
#   dplyr::group_by(DISTRICT) %>%
#   dplyr::summarise(Positive.samples = sum(Targetbin),
#                    Samples = dplyr::n()) %>%
#   mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1))
# colnames(sample.sizes.district) <- c("Year", "Samples", "Positive Samples", "Percent Positive")
#
# #make table
# table.district <- kableExtra::kbl(samples_size_1 ,
#                 booktabs = TRUE,
#                 caption = paste("Positive samples of vermilion in",
#                                 Model_region[Model.number], "by district.")) %>%
#   kable_styling(latex_options = "striped", full_width = FALSE)
# }
#-------------------------------------------------------------------------------

# tab-area
# table of area by year for models that use AREA as region
#
# sample.sizes.area<- dat %>%
#   dplyr::group_by(AREA) %>%
#   dplyr::summarise(Positive.samples = sum(Targetbin),
#                    Samples = dplyr::n()) %>%
#   mutate(percent.pos = scales::percent(Positive.samples/Samples, accuracy = 1))
# colnames(sample.sizes.area) = c("Area", "Drifts", "Postive Drifs", "Percent Positive")
# #make table
# table.area <- kableExtra::kbl(sample.sizes.area,
#                 booktabs = TRUE,
#                 caption = paste("Positive samples of", spp1, "in",
#                                 Model_region[Model.number], "by areas.")) %>%
#   kable_styling(latex_options = "striped", full_width = FALSE)


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
if (run_delta_glm == TRUE) {
  # tab-model-select
  Models$Model <- as.data.frame(c(
    as.character(b1)[3],
    as.character(b2)[3],
    as.character(b3)[3],
    as.character(b4)[3],
    as.character(b5)[3],
    as.character(b6)[3],
    as.character(b7)[3],
    as.character(b8)[3]
  ))

  Model_selection <- as.data.frame(cbind(
    Models$Model,
    round(binAIC$AIC, 4),
    round(posAIC$AIC, 4)
  ))
  colnames(Model_selection) <- c("Model", "bin", "pos")
  Model_selection$Model <- gsub("\\_", " ", Model_selection$Model)

  delta <- function(x) (round((abs(x - min(x))), 2))
  Model_selection <- Model_selection %>%
    mutate_at(vars("bin", "pos"), delta) %>%
    unique()

  table.model.select <- kableExtra::kbl(Model_selection,
    booktabs = TRUE,
    caption = paste0(
      "Model selection for the ", params$survey.name,
      " survey index for ", spp1, " in the ",
      model.region, "."
    ),
    label = paste0("tab-model-select-", surveys$survey.labels[which(surveys$index.subfolder == params$index.subfolder)]),
    col.names = c(
      "Model",
      "Binomial $\\Delta$AIC",
      paste0(pos.mod.dist, " $\\Delta$AIC")
    ),
    escape = F,
    row.names = F
  ) %>%
    kable_styling(latex_options = "striped", full_width = FALSE)
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
if (run_negative_binomial == TRUE) {
  # tab-model-select
  nbModels$Model <- as.data.frame(c(
    as.character(nb1)[3],
    as.character(nb2)[3],
    as.character(nb3)[3],
    as.character(nb4)[3],
    as.character(nb5)[3],
    as.character(nb6)[3],
    as.character(nb7)[3],
    as.character(nb8)[3],
    as.character(nb9)[3]
  ))

  Model_selection <- as.data.frame(cbind(
    nbModels$Model,
    round(nbinAIC$AIC, 4)
  ))
  colnames(Model_selection) <- c("Model", "AIC")
  Model_selection$Model <- gsub("\\_", " ", Model_selection$Model)

  delta <- function(x) (round((abs(x - min(x))), 2))
  Model_selection <- Model_selection %>%
    mutate_at(vars("AIC"), delta) %>%
    unique()

  table.model.select <- kableExtra::kbl(Model_selection,
    booktabs = TRUE,
    caption = paste0(
      "Model selection for the ", params$survey.name,
      " survey index for ", spp1, " in the ",
      model.region, "."
    ),
    label = paste0("tab-model-select-", surveys$survey.labels[which(surveys$index.subfolder == params$index.subfolder)]),
    col.names = c(
      "Model",
      "$\\Delta$AIC"),
    escape = F,
    row.names = F
  ) %>%
    kable_styling(latex_options = "striped", full_width = FALSE)
}
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
if(params$survey.name=="CCFRP"){ outdf$Index <- outdf$Mean}
# tab-index
# index table
index.tab <-  outdf %>%
    dplyr::select(Year, Index, logSD, `2.5%`, `97.5%`) %>%
    mutate(across(c(2:5), ~ round(.,2))) %>%
    rename(
      logSE = logSD,
      `lower HPD` = `2.5%`,
      `upper HPD` = `97.5%`)
  
table.index <- kableExtra::kbl(index.tab,
  booktabs = TRUE,
  caption = paste0(
    "Standardized index for the ", params$survey.name,
    " survey index with log-scale standard errors and 95\\% highest
       posterior density (HPD) intervals for ", species.name, " in the ",
    model.region,"."
  ),
  label = paste0("tab-index-", surveys$survey.labels[which(surveys$index.subfolder == params$index.subfolder)]),
  row.names = F
) %>%
  kableExtra::kable_styling(latex_options = "striped")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------




############################## FIGURES #########################################
#-------------------------------------------------------------------------------


if (run_delta_glm == TRUE) {
  #-------------------------------------------------------------------------------
  # fig-dist-fits
  # qq plots and residuals
  pos.qq <- ggplot(logn.full, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
    geom_point(aes(color = "lognormal"), show.legend = TRUE) +
    geom_point(data = gamma.full, aes(qqnorm(.stdresid)[[1]], .stdresid,
      color = "gamma"
    ), show.legend = TRUE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = "Theoretical Quantiles", y = "Standardized Residuals",
      color = "Legend"
    ) +
    theme_bw() +
    scale_colour_manual(
      name = "",
      guide = "legend",
      values = c(
        "lognormal" = "darkcyan",
        "gamma" = "darkorange2"
      ),
      labels = c(
        "lognormal",
        "gamma"
      )
    )

  mod.full <- if (pos.mod.dist == "Lognormal") {
    logn.full
  } else {
    gamma.full
  }
  pos.resid <- ggplot(mod.full, aes(.fitted, .resid)) +
    geom_point() +
    stat_smooth(method = "loess") +
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
    labs(
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_bw()

  # sjPlot::plot_grid(list(pos.qq, pos.resid), tags = TRUE, margin = c(.1, .1, .1, .1))
  #-------------------------------------------------------------------------------
}

#-------------------------------------------------------------------------------
# figure.subregion.CPUE
# fig-region-cpue
# Look at raw cpue by subregion
figure.subregion.CPUE <- dat %>%
  mutate_at(vars(YEAR), varhandle::unfactor) %>%
  group_by(SubRegion, YEAR) %>%
  summarise(avg_cpue = mean(CPUE)) %>%
  tidyr::complete(YEAR = min(yrvec):max(yrvec)) %>%
  ggplot(aes(YEAR, avg_cpue, colour = SubRegion)) +
  geom_line(lwd = 1.1) +
  geom_point(size = 3) +
  theme_bw() +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Arithmetic mean of CPUE") +
  ggplot2::scale_colour_brewer(palette = "Paired") +
  ggplot2::theme(legend.title = element_blank()) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(colour = guide_legend(nrow = 2)) +
  ggplot2::theme(text = element_text(size = 10)) +
  ggplot2::theme(plot.margin = margin(.5, .5, .5, .5, "in"))
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
if (params$survey.name == "CCFRP") {
  site.legend.labels <- c("Inside MPA", "Outside MPA")
  # Look at raw cpue by region
  figure.SITE.rawCPUE <- dat %>%
    mutate_at(vars(YEAR), varhandle::unfactor) %>%
    group_by(SITE, YEAR) %>%
    summarise(avg_cpue = mean(CPUE)) %>%
    tidyr::complete(YEAR = min(yrvec):max(yrvec)) %>%
    ggplot(aes(YEAR, avg_cpue, colour = SITE)) +
    geom_line(lwd = 1.1) +
    geom_point(size = 3) +
    theme_bw() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Arithmetic mean of CPUE") +
    ggplot2::scale_colour_brewer(labels = site.legend.labels, palette = "Dark2") +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(colour = guide_legend(nrow = 2)) +
    ggplot2::theme(text = element_text(size = 10)) +
    ggplot2::theme(plot.margin = margin(.5, .5, .5, .5, "in"))
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# fig-cpue
# final cpue
figure.CPUE <- outdf %>%
  tidyr::complete(Year = min(yrvec):max(yrvec), fill = list(stdzd.raw.cpue = NA)) %>%
  dplyr::select(Year, stdzd.Index, stdzd.raw.cpue) %>%
  rename(`Standarized Index` = stdzd.Index, `Arithmetic mean of CPUE` = stdzd.raw.cpue) %>%
  tidyr::gather(index, cpue, -Year) %>%
  ggplot(aes(Year, cpue, colour = index)) +
  geom_line(lwd = 1.1) +
  geom_point(size = 3) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Standardized CPUE") +
  ggplot2::scale_colour_brewer(palette = "Dark2") +
  ggplot2::theme(legend.title = element_blank()) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(text = element_text(size = 10)) +
  ggplot2::theme(plot.margin = margin(.5, .5, .5, .5, "in"))

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# For CCFRP change MPA and REF weighting
if (params$species.name == "vermilion" & params$survey.name == "CCFRP") {
  Area.weighted.options <- Area.index.standardized %>%
    relocate("Unweighted", .before = `MPA 10%`) %>%
    tidyr::pivot_longer(!YEAR, names_to = "index", values_to = "cpue") %>%
    ggplot(aes(YEAR, cpue, colour = index)) +
    geom_line(lwd = 1) +
    geom_point(size = 2) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      x = "Year",
      y = "Standardized CPUE",
      colour = "Percent of index\nweight given to MPAs"
    ) +
    ggplot2::scale_colour_brewer(palette = "Dark2") +
    ggplot2::guides(colour = guide_legend(reverse = TRUE)) +
    ggplot2::theme(text = element_text(size = 10)) +
    ggplot2::theme(plot.margin = margin(.5, .5, .5, .5, "in"))
}




#-------------------------------------------------------------------------------
if (run_delta_glm == TRUE) {
  # fig-Dbin-marginal
  bin.components <- all.vars(get(paste0(
    "b",
    which.min(binAIC$AIC[2:length(binAIC$AIC)]) + 1
  )))
  Dbin.ggpredict.list <- vector(mode = "list", length(bin.components) - 1)
  figure.Dbin.list <- Dbin.ggpredict.list
  for (i in 1:length(bin.components[-c(1)])) {
    Dbin.ggpredict <- ggeffects::ggpredict(Dbin,
      bin.components[i + 1],
      ppd = FALSE
    )
    Dbin.ggpredict.list[[i]] <- Dbin.ggpredict
    figure.Dbin.list[[i]] <- plot(Dbin.ggpredict,
      show.title = FALSE
    ) +
      theme_bw() +
      theme(text = element_text(size = 8)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

  #-------------------------------------------------------------------------------
  # fig-Dpos-marginal
  # positive model marginal components
  Dpos <- if (pos.mod.dist == "Lognormal") {
    Dlno
  } else {
    Dgam
  }
  Pos.formula <- ifelse(pos.mod.dist == "Lognormal", "f", "g")
  pos.components <- all.vars(get(paste0(
    Pos.formula,
    which.min(posAIC$AIC[2:length(posAIC$AIC)]) + 1
  )))
  Dpos.ggpredict.list <- vector(mode = "list", length(pos.components) - 1)
  figure.Dpos.list <- Dpos.ggpredict.list

  for (j in 1:length(pos.components[-c(1)])) {
    Dpos.ggpredict <- ggeffects::ggpredict(Dpos, pos.components[j + 1], ppd = FALSE)
    Dpos.ggpredict.list[[j]] <- Dpos.ggpredict
    figure.Dpos.list[[j]] <- plot(Dpos.ggpredict,
      rawdata = FALSE,
      show.title = FALSE,
      show.legend = FALSE
    ) +
      theme_bw() +
      theme(text = element_text(size = 8)) +
      theme(axis.title.y = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Marginal effects and cpue - need to install package sjPlot
if (run_negative_binomial == TRUE) {
  Nbin.formula <- "nb"
  nbin.components <- all.vars(get(paste0(
    Nbin.formula,
    which.min(nbinAIC$AIC[2:length(nbinAIC$AIC)]) + 1
  )))
  nbin.components <- nbin.components[-which(nbin.components %in% c("Target", "logEffort"))]
  Dnbin.ggpredict.list <- vector(mode = "list", length(nbin.components))
  figure.Dnbin.list <- Dnbin.ggpredict.list

  for (j in 1:length(nbin.components)) {
    Dnbin.ggpredict <- ggeffects::ggpredict(Dnbin, nbin.components[j], ppd = FALSE)
    Dnbin.ggpredict.list[[j]] <- Dnbin.ggpredict
    figure.Dnbin.list[[j]] <- plot(Dnbin.ggpredict,
      rawdata = FALSE,
      show.title = FALSE,
      show.legend = FALSE
    ) +
      theme_bw() +
      theme(text = element_text(size = 8)) +
      theme(axis.title.y = element_blank())
  }
  # add interaction
  figure.Dnbin.list[[j + 1]] <- plot(
    ggeffects::ggpredict(Dnbin, c("YEAR", "SITE"), ppd = FALSE),
    rawdata = FALSE,
    show.title = FALSE,
    show.legend = FALSE
  ) +
    theme_bw() +
    theme(text = element_text(size = 8)) +
    theme(axis.title.y = element_blank())
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
if (params$survey.name == "WCGBTS") {
  plot_map <- function(data) {
    states_map <- ggplot2::map_data("state", regions = "california")
    ggplot2::ggplot(states_map, ggplot2::aes(x = long, y = lat)) +
      ggplot2::geom_polygon(ggplot2::aes(group = group), fill = NA, col = "black") +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::geom_point(
        data = data %>% dplyr::filter(NUMBER_FISH > 0),
        alpha = 0.5, cex = 2.5,
        ggplot2::aes(
          y = BEST_LAT_DD,
          x = LONGITUDE_DD,
          col = TOTAL_CATCH_WT_KG
        )
      ) +
      ggplot2::xlab("Longitude (decimal degrees)") +
      ggplot2::ylab("Latitude (decimal degrees)") +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "weight (kg)")) +
      ggplot2::geom_hline(yintercept = 34.5)
  }
  plot_map(ca_data)
}

#-------------------------------------------------------------------------------
