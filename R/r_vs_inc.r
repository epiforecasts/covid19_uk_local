# set up options
library("optparse")
option_list <- list(
    make_option(c("-s", "--source"), type = "character", action = "store_true",
        default = "cases",
        help = "Specify the data source to use [default %default]")
    )
args <- parse_args(OptionParser(option_list = option_list))

library("readxl")
library("janitor")
library("dplyr")
library("tidyr")
library("vroom")
library("ggplot2")
library("ggrepel")
library("ggExtra")

# make output directory
fig_path <- here::here("figure", args$source)
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)


## get UTLA/NHSER mapping
utla_nhser <- readRDS(here::here("data", "utla_nhser.rds")) %>%
  rename(region = utla_name)

pop_l <- read_excel(here::here("data", "uk_pop.xls"),
                       sheet = "MYE2 - Persons", skip = 4) %>%
  janitor::clean_names() %>%
  filter(!is.na(name)) %>%
  select(-all_ages) %>%
  mutate(nation = substr(code, 1, 1),
         nation = recode_factor(nation,
                                E = "England",
                                N = "Northern Ireland",
                                S = "Scotland",
                                W = "Wales",
                                .default = NA_character_)) %>%
  filter(!is.na(nation)) %>%
  pivot_longer(starts_with("x"), names_to = "age", values_to = "pop") %>%
  group_by(nation, region = name) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  pivot_wider(names_from = "region", values_from = "pop") %>%
  mutate(`Cornwall and Isles of Scilly` = Cornwall + `Isles of Scilly`,
         `Hackney and City of London` = Hackney + `City of London`) %>%
  pivot_longer(c(-nation), names_to = "region", values_to = "pop") %>%
  filter(!is.na(pop))

rt <- vroom(paste0("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom-local/", args$source, "/summary/rt.csv")) %>%
  rename_at(vars(c(-region, -date, -strat, -type)), ~ paste0(., "_R"))

rep <- vroom(paste0("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/united-kingdom-local/", args$source, "/summary/cases_by_report.csv")) %>%
  rename_at(vars(c(-region, -date, -strat, -type)), ~ paste0(., "_rep"))

compare <- pop_l %>%
  inner_join(rt, by = c("region")) %>%
  inner_join(rep, by = c("region", "date", "strat", "type")) %>%
  mutate(median_abs = median_rep) %>%
  mutate_at(vars(ends_with("_rep")), ~ . / pop * 700000) %>%
  mutate(label = if_else((median_R > 2) |
                         (median_R > 1 & median_rep > 300) |
                         (median_R < 1 & median_rep > 200) |
                         (median_R > 1.5 & median_rep > 200),
                        region, NA_character_)) %>%
  left_join(utla_nhser, by = "region") %>%
  mutate(nhse_region = if_else(nation == "England", nhse_region,
                               as.character(nation)),
         nhse_region = if_else(grepl("^Cornwall", region), "South West",
                               nhse_region),
         nhse_region = if_else(grepl("^Hackney", region), "London",
                               nhse_region))

compare_latest <- compare %>%
  filter(type != "forecast") %>%
  filter(date == max(date)) %>%
  mutate(date = "last")

labelled_regions <- compare_latest %>%
  filter(!is.na(label)) %>%
  .$label

past_weeks <- 1
week_label_short <- paste0(past_weeks, "w")
week_label <-
  paste0(past_weeks, " week", if_else(past_weeks > 1, "s", ""), " ago")
compare_previous <- rt %>%
  filter(type != "forecast") %>%
  mutate(max_date = max(date)) %>%
  filter(as.integer(max_date - date) %in% c(0L, past_weeks * 7L)) %>%
  mutate(select = if_else(date == max(date), "last", week_label_short)) %>%
  select(-max_date) %>%
  select(region, date, select, R = median_R) %>%
  inner_join(rep, by = c("region", "date")) %>%
  select(region, select, R, rep = median_rep) %>%
  inner_join(pop_l, by = c("region")) %>%
  mutate(rep = rep / pop * 700000) %>%
  pivot_longer(c(R, rep)) %>%
  unite("name", c(name, select)) %>%
  pivot_wider() %>%
  left_join(utla_nhser, by = "region") %>%
  mutate(nhse_region = if_else(is.na(nhse_region), "Other", nhse_region)) %>%
  pivot_longer(c(starts_with("R_"), starts_with("rep_"))) %>%
  separate(name, c("variable", "date"), remove = FALSE)

p <- ggplot(compare_previous %>% select(-name) %>%
            pivot_wider(names_from = "variable"),
            aes(colour = date, fill = date)) +
  geom_segment(data = compare_previous %>% select(-variable, -date) %>%
                 pivot_wider(names_from = "name") %>%
                 mutate(date = week_label_short),
               aes_string(x = paste("R", week_label_short, sep = "_"),
                          y = paste("rep", week_label_short, sep = "_"),
                          xend = 'R_last', yend = "rep_last"),
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_errorbarh(data = compare_latest,
                 alpha = 0.3, aes(xmin = lower_20_R, xmax = upper_20_R,
                                  y = median_rep)) +
  geom_errorbarh(data = compare_latest,
                 alpha = 0.15, aes(xmin = lower_50_R, xmax = upper_50_R,
                                   y = median_rep)) +
  geom_text_repel(data = compare_latest, aes(x = median_R, y = median_rep,
                                      label = label), show.legend = FALSE) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(aes(y = rep, x = R)) +
  ylab("7-day incidence / 100k") +
  xlab("R") +
  theme_bw() +
  scale_colour_manual("", values = c("black", "red"),
                      breaks = c("last", week_label_short),
                      labels = c("latest", week_label)) +
  scale_fill_manual("", values = c("black", "red"),
                    breaks = c("last", week_label_short),
                    labels = c("latest", week_label)) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_path, paste0("r_vs_inc_", week_label_short, ".png")),
       suppressWarnings(
         ggMarginal(p,
                    groupFill = TRUE, groupColour = TRUE, type = "histogram",
                    xparams = list(position = "identity"),
                    yparams = list(position = "identity")
                    )
       ),
       width = 11, height = 7
       )

p <- ggplot(compare_latest,
       aes(y = median_rep, x = median_R, colour = nhse_region)) +
  geom_point() +
  geom_errorbarh(alpha = 0.3, aes(xmin = lower_20_R, xmax = upper_20_R)) +
  geom_errorbar(alpha = 0.3, aes(ymin = lower_20_rep, ymax = upper_20_rep)) +
  geom_errorbarh(alpha = 0.15, aes(xmin = lower_50_R, xmax = upper_50_R)) +
  geom_errorbar(alpha = 0.15, aes(ymin = lower_50_rep, ymax = upper_50_rep)) +
  geom_text_repel(aes(label = label), show.legend = FALSE) +
  ylab("7-day incidence / 100k") +
  xlab("R") +
  theme_bw() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_brewer("Region", palette = "Paired")

ggsave(file.path(fig_path, "r_vs_inc_region.png"), p, width = 11, height = 6)

weeks_hist <- 5

recent <- compare %>%
  filter(type != "forecast") %>%
  filter(as.integer(max(date) - date) %% 7 == 0,
         date > max(date) - 7 * 5) %>%
  mutate(label = if_else(date == max(date) & !is.na(label), region, NA_character_))

p <- ggplot(recent, aes(x = date, y = mean_R,
                        colour = nhse_region,
                        group = region)) +
  geom_point() +
  geom_line(colour = "black", alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  scale_colour_brewer("", palette = "Paired") +
  theme_bw() +
  xlab("") +
  expand_limits(x = max(recent$date + 7)) +
  scale_y_continuous("R") +
  theme(legend.position = "bottom") +
  geom_text_repel(aes(label = label), show.legend = FALSE)

ggsave(file.path(fig_path,  "recent_r.png"), p, width = 11, height = 6)
