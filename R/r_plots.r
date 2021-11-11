# set up options
library("optparse")
option_list <- list(
    make_option(c("-s", "--source"), type = "character", action = "store_true",
        default = "cases",
        help = "Specify the data source to use [default %default]")
    )
args <- parse_args(OptionParser(option_list = option_list))

das <- c("Northern Ireland", "Scotland", "Wales")

# load packages
library("here")
library("dplyr")
library("tidyr")
library("ggplot2")
library("janitor")
library("vroom")
library("lubridate")
library("covidregionaldata")
library("ggrepel")
library("readxl")
library("stringr")

# make output directory
fig_path <- here::here("figure", args$source)
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

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

rt_url <-
  paste0("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/",
         "master/subnational/united-kingdom-local/", args$source, "/summary/rt.csv")
rt <- vroom::vroom(rt_url)

utla_nhser <- readRDS(here::here("data", "utla_nhser.rds")) %>%
mutate(nhse_region = factor(nhse_region,
                            c(setdiff(unique(nhse_region), das), das)))

rt_by_utla <- rt %>%
  rename(utla_name = region) %>%
  filter(type != "forecast") %>%
  left_join(utla_nhser, by = "utla_name") %>%
  left_join(pop_l %>% rename(utla_name = "region"), by = "utla_name") %>%
  mutate(nhse_region =
           if_else(is.na(nhse_region), as.character(nation), as.character(nhse_region)),
         nhse_region =
           if_else(grepl("^Cornwall", utla_name), "South West", nhse_region),
         nhse_region =
           if_else(grepl("^Hackney", utla_name), "London", nhse_region),
         nhse_region = factor(nhse_region, levels(utla_nhser$nhse_region)))

utla_sorted <- rt_by_utla %>%
  filter(date == max(date)) %>%
  arrange(desc(mean)) %>%
  mutate(id = seq_len(n()))

p <- ggplot(utla_sorted,
            aes(x = id, y = mean,
                colour = nhse_region,
                label = utla_name)) +
  geom_linerange(aes(ymin = lower_20, ymax = upper_20), size = 2) +
  geom_linerange(aes(ymin = lower_50, ymax = upper_50), alpha = 0.35, size = 2) +
  geom_linerange(aes(ymin = lower_90, ymax = upper_90), alpha = 0.15, size = 2) +
  scale_colour_brewer("", palette = "Paired", drop = FALSE) +
  xlab("") +
  ylab("Reproduction number estimate") +
  theme_classic() +
  labs(size = str_to_sentence(args$source)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(file.path(fig_path, "R_ranking.svg"), p, height = 4, width = 12)

prop_gt <- rt_by_utla %>%
  filter(date > max(date) - 90) %>%
  group_by(date) %>%
  filter(!duplicated(utla_name)) %>%
  ungroup() %>%
  mutate(gt_1 = as.integer(median > 1))

prop_gt_reg <- prop_gt %>%
  group_by(date) %>%
  summarise(gt_1 = mean(gt_1), 
            type = names(which.max(table(type))),
            .groups = "drop")

plot_gt <- function(data) {
  p <- ggplot(data, aes(x = date, y = gt_1, alpha = type,
                        fill = nhse_region)) +
    geom_col() +
    xlab("") +
    theme_bw() +
    ylab("Proportion of UTLAs with P(R > 1) > 0.5") +
    geom_hline(yintercept = 1) +
    scale_fill_brewer("", palette = "Paired", drop = FALSE) +
    scale_alpha_manual("", values = c(1, 0.35)) +
    theme(legend.position = "bottom")
  return(p)
}

p <- plot_gt(prop_gt)

ggsave(file.path(fig_path, "latest_prop_gt1.svg"), p, height = 6.5, width = 11)

prop_gt_da_region <- prop_gt %>%
  group_by(date, nhse_region) %>%
  summarise(gt_1 = mean(gt_1), 
            type = names(which.max(table(type))),
            .groups = "drop")

p <- plot_gt(prop_gt_da_region) + 
  facet_wrap(~ nhse_region)

ggsave(file.path(fig_path, "latest_prop_gt1_da_region.svg"), p,
                  height = 8, width = 8)
