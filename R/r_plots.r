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

# make output directory
fig_path <- here::here("figure")
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
         "master/subnational/united-kingdom-local/cases/summary/rt.csv")
rt <- vroom::vroom(rt_url)

utla_nhser <- readRDS(here::here("data", "utla_nhser.rds"))

rt_by_utla <- rt %>%
  rename(utla_name = region) %>%
  filter(type != "forecast") %>%
  left_join(utla_nhser, by = "utla_name") %>%
  left_join(pop_l %>% rename(utla_name = "region"), by = "utla_name") %>%
  mutate(nhse_region =
           if_else(is.na(nhse_region), as.character(nation), nhse_region),
         nhse_region =
           if_else(grepl("^Cornwall", utla_name), "South West", nhse_region),
         nhse_region =
           if_else(grepl("^Hackney", utla_name), "London", nhse_region))

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
  labs(size = paste("Cases")) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(here::here("figure", "R_ranking.png"), p, height = 4, width = 12)

prop_gt <- rt_by_utla %>%
  filter(date > max(date) - 90) %>%
  group_by(date) %>%
  filter(!duplicated(utla_name)) %>%
  ungroup() %>%
  mutate(gt_1 = median > 1)

prop_gt_nat <- prop_gt %>%
  group_by(date) %>%
  summarise(gt_1 = mean(gt_1), 
            type = names(which.max(table(type))),
            .groups = "drop")

plot_gt <- function(data) {
  p <- ggplot(data, aes(x = date, y = gt_1, alpha = type)) +
    geom_col() +
    xlab("") +
    theme_bw() +
    ylab("Proportion of UTLAs with P(R > 1) > 0.5") +
    geom_hline(yintercept = 1) +
    scale_alpha_manual("", values = c(1, 0.35)) +
    theme(legend.position = "bottom")
  return(p)
}

p <- plot_gt(prop_gt_nat)

ggsave(here::here("figure", "latest_prop_gt1.png"), p, height = 4, width = 8)

prop_gt_da_region <- prop_gt %>%
  group_by(date, nhse_region) %>%
  summarise(gt_1 = mean(gt_1), 
            type = names(which.max(table(type))),
            .groups = "drop")

p <- plot_gt(prop_gt_da_region) + 
  facet_wrap(~ nhse_region)

ggsave(here::here("figure", "latest_prop_gt1_da_region.png"), p,
                  height = 8, width = 8)
