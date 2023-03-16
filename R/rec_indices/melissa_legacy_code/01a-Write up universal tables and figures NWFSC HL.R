
#-------------------------------------------------------------------------------
# define survey label


#-------------------------------------------------------------------------------
################################ TABLES ########################################


#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# tab-depth
# samples by depth bin
dat <- dat %>%
  mutate(Targetbin = as.numeric(vermtot > 0))
  
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
      "Positive samples of", params$species.name, "in the",
      model.region, "by depth (fm)."
    ),
    label = paste0("tab-depth-nwfschl")
  ) %>%
    kable_styling(latex_options = "striped", full_width = FALSE)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# tab-year
# samples by year
sample.sizes.year <- dat %>%
  dplyr::group_by(year) %>%
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
    "Samples of", params$species.name, "in the",
    model.region, "by year."
  ),
  label = paste0("tab-year-nwfschl")
) %>%
  kable_styling(latex_options = "striped", full_width = FALSE)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# tab-depth by area
# samples by year
depth.by.site <- dat %>%
  dplyr::group_by(area_name, DEPTH_bin) %>%
  dplyr::summarise(
    Positive.samples = sum(Targetbin),
    Samples = dplyr::n()
  ) %>%
mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1)) %>%
  dplyr::select(area_name, DEPTH_bin, percent.pos) %>%
  tidyr::pivot_wider(names_from = DEPTH_bin, values_from = percent.pos) %>%
  relocate("(0,50]", .before = "(75,100]") %>%
  relocate("(50,75]", .before = "(75,100]") %>%
  rename("Area name" = area_name)

table.depth.by.site <- kableExtra::kbl(depth.by.site,
                                booktabs = TRUE,
                                caption = paste(
                                  "Samples of", params$species.name, "in the
                                  NWFSC hook-and-line survey by area and 
                                  depth bins (ft)."
                                ),
                                label = paste0("tab-depthsite-nwfschl")
  ) %>%
  kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE)



#-------------------------------------------------------------------------------
# tab-region
# # table of sample sizes by region
# sample.sizes.region <- dat %>%
#   dplyr::group_by(SubRegion) %>%
#   dplyr::summarise(
#     Positive.samples = sum(Targetbin),
#     Samples = dplyr::n()
#   ) %>%
#   mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1))
# colnames(sample.sizes.region) <- c("Year", "Samples", "Positive Samples", "Percent Positive")
# 
# # make table
# table.subregion <- kable(sample.sizes.region,
#   booktabs = TRUE,
#   caption = paste(
#     "Samples of", params$species.name, "in the",
#     model.region, "by subregion used in the index."
#   ),
#   label = paste0("tab-region-", surveys$survey.labels[which(surveys$index.subfolder == params$index.subfolder)])
# ) %>%
#   kable_styling(latex_options = "striped", full_width = FALSE)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# tab-index
# index table
table.index <- kable(
  outdf %>%
    dplyr::select(Year, Index, logSD, `2.5%`, `97.5%`) %>%
    mutate_at(vars(-Year), round, 4) %>%
    rename(
      Mean = Index, logSE = logSD,
      `lower HPD` = `2.5%`,
      `upper HPD` = `97.5%`
    ),
  booktabs = TRUE,
  caption = paste0(
    "Standardized index for the NWFSC Hook-and-Line Survey index with log-scale standard errors and 95\\% highest
       posterior density (HPD) intervals for ", params$species.name, " in the ",
    model.region, "."
  ),
  label = paste0("tab-index-nwfschl"),
  row.names = F
) %>%
  kable_styling(latex_options = "striped")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------




############################## FIGURES #########################################
#-------------------------------------------------------------------------------



# #-------------------------------------------------------------------------------
# # fig-dist-fits
# # qq plots and residuals
# pos.qq <- ggplot(logn.full, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
#   geom_point(aes(color = "lognormal"), show.legend = TRUE) +
#   geom_point(data = gamma.full, aes(qqnorm(.stdresid)[[1]], .stdresid,
#     color = "gamma"
#   ), show.legend = TRUE) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   labs(
#     x = "Theoretical Quantiles", y = "Standardized Residuals",
#     color = "Legend"
#   ) +
#   theme_bw() +
#   scale_colour_manual(
#     name = "",
#     guide = "legend",
#     values = c(
#       "lognormal" = "darkcyan",
#       "gamma" = "darkorange2"
#     ),
#     labels = c(
#       "lognormal",
#       "gamma"
#     )
#   )
# 
# mod.full <- if (pos.mod.dist == "Lognormal") {
#   logn.full
# } else {
#   gamma.full
# }
# pos.resid <- ggplot(mod.full, aes(.fitted, .resid)) +
#   geom_point() +
#   stat_smooth(method = "loess") +
#   geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
#   labs(
#     x = "Fitted values",
#     y = "Residuals"
#   ) +
#   theme_bw()
# 
# # sjPlot::plot_grid(list(pos.qq, pos.resid), tags = TRUE, margin = c(.1, .1, .1, .1))
# #-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# figure.subregion.CPUE
# fig-region-cpue
# Look at raw cpue by subregion
# figure.subregion.CPUE <- dat %>%
#   mutate_at(vars(YEAR), varhandle::unfactor) %>%
#   group_by(SubRegion, YEAR) %>%
#   summarise(avg_cpue = mean(CPUE)) %>%
#   tidyr::complete(YEAR = min(yrvec):max(yrvec)) %>%
#   ggplot(aes(YEAR, avg_cpue, colour = SubRegion)) +
#   geom_line(lwd = 1.1) +
#   geom_point(size = 3) +
#   theme_bw() +
#   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
#   ggplot2::xlab("Year") +
#   ggplot2::ylab("Arithmetic mean of CPUE") +
#   ggplot2::scale_colour_brewer(palette = "Paired") +
#   ggplot2::theme(legend.title = element_blank()) +
#   ggplot2::theme(legend.position = "bottom") +
#   ggplot2::guides(colour = guide_legend(nrow = 2)) +
#   ggplot2::theme(text = element_text(size = 10)) +
#   ggplot2::theme(plot.margin = margin(.5, .5, .5, .5, "in"))
#-------------------------------------------------------------------------------

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
#

pos.components <- all.vars(logitnorm$formula) 
pos.components <- pos.components[c(1:6)]

ggpredict.list <- vector(mode = "list", length(pos.components) - 3)
figure.ggpredict.list <- ggpredict.list

for (j in 1:length(pos.components[-c(1,2)])) {
  Dist.ggpredict <- ggeffects::ggpredict(logitnorm, pos.components[j + 2], ppd = FALSE)
  ggpredict.list[[j]] <- Dist.ggpredict
  figure.ggpredict.list[[j]] <- plot(Dist.ggpredict,
    rawdata = FALSE,
    show.title = FALSE,
    show.legend = FALSE
  ) +
    theme_bw() +
    theme(text = element_text(size = 8)) +
    theme(axis.title.y = element_blank()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
