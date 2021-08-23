library("dplyr")
library("ggplot2")
library("ggrepel")

fig_path <- here::here("figure")
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

adm <- readRDS(file.path("output", "admissions", "r_inc.rds"))
cas <- readRDS(file.path("output", "cases", "r_inc.rds"))

com <- adm %>%
  left_join(cas, by = c("nation", "region", "nhse_region",
                        "pop", "date", "strat", "type"),
            suffix = c(".adm", ".cas")) %>%
  mutate(label = if_else(!is.na(label.adm) | !is.na(label.cas),
         region, NA_character_))

xmax <- max(com$median_R.cas, na.rm = TRUE) +
  IQR(com$median_R.cas, na.rm = TRUE)
xmin <- min(com$median_R.cas, na.rm = TRUE) -
  IQR(com$median_R.cas, na.rm = TRUE)
ymax <- max(com$median_R.adm, na.rm = TRUE) +
  IQR(com$median_R.adm, na.rm = TRUE)
ymin <- min(com$median_R.adm, na.rm = TRUE) -
  IQR(com$median_R.adm, na.rm = TRUE)

p <- ggplot(com, aes(x = median_R.cas, y = median_R.adm,
                     color = nhse_region)) +
  geom_point() +
  xlab("Cases") +
  ylab("Admissions") +
  geom_text_repel(aes(label = label), show.legend = FALSE) +
  theme_bw() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer("Region", palette = "Paired") +
  coord_cartesian(xlim = c(xmin, xmax),
                  ylim = c(ymin, ymax))

ggsave(file.path(fig_path, "cas_adm.png"), p, width = 11, height = 6)
