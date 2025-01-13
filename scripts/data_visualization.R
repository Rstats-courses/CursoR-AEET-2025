## -----------------------------------------------------
library(here)
library(tidyverse)
library(tidylog)


## ------------------------------------------------------------------
dt <- read_csv(here("data/clean_data.csv"))


## ------------------------------------------------------------------


ggplot(data = dt)


## ------------------------------------------------------------------


ggplot(data = dt,
       aes(x = year, y = fruits))


## ------------------------------------------------------------------


ggplot(data = dt,
       aes(x = year, y = fruits)) +
  geom_point()


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits)) +
  geom_point(color = "coral")


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits)) +
  geom_point(color = "coral",
             shape = "triangle",
             size = 4,
             alpha = 0.5)


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = pollinator_code)) +
  geom_point(shape = "triangle",
             size = 4,
             alpha = 0.5)


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = year)) +
  geom_point()


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = year)) +
  geom_point() +
  scale_x_continuous(n.breaks = 3)


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = year)) +
  geom_point() +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(
          breaks = c(100000, 200000),
          labels = c("100mil",
                     "200mil"))


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = year)) +
  geom_point() +
  scale_y_reverse()


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = year,
           size = year)) +
  geom_point() +
  scale_size_binned(range = c(0.1, 8),
                    n.breaks = 10)


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = fleshy_fruit)) +
  geom_point() +
  scale_color_manual(
      values = c("darkolivegreen3",
                 "coral2"))


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = year)) +
  geom_point() +
  scale_color_gradient(
    low = "yellow", high = "orange")


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = year, y = fruits,
           color = year)) +
  geom_point() +
  scale_color_viridis_c()


## ------------------------------------------------------------------


dt |>
  group_by(year, site) |>
  summarise(mean_fruits = mean(fruits,
                    na.rm = TRUE)) |>
  ggplot(aes(x = year,
             y = mean_fruits)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, scales = "free")


## ------------------------------------------------------------------


dt |>
  group_by(year, site, pollinator_code) |>
  summarise(mean_fruits = mean(fruits,
                    na.rm = TRUE)) |>
  ggplot(aes(x = year,
             y = mean_fruits)) +
  geom_point() +
  geom_line() +
  facet_grid(site~pollinator_code,
             scales = "free")


## ------------------------------------------------------------------
#| echo: false
sort(ls("package:ggplot2", pattern = "^geom_"))


## ------------------------------------------------------------------
#| message: true
dt_diam <- dt |> filter(!is.na(stem_cm))


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = stem_cm, y = fruits)) +
  geom_point() +
  geom_hline(yintercept = 700,
             size = 2) +
  geom_vline(xintercept = c(50, 120),
          color = c("red", "blue"))


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = stem_cm, y = fruits)) +
  geom_point() +
  geom_smooth()


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = stem_cm, y = fruits)) +
  geom_point() +
  geom_smooth(method = "lm")


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = stem_cm, y = fruits,
           color = shade_tolerance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = stem_cm)) +
  geom_histogram()


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = stem_cm)) +
  geom_histogram(bins = 10)


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = stem_cm)) +
  geom_density()


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = stem_cm)) +
  geom_density(bw = 1)


## ------------------------------------------------------------------


ggplot(dt_diam,
         aes(x = species_name)) +
  geom_bar(stat = "count")


## ------------------------------------------------------------------


dt_diam |>
  group_by(species_name) |>
  summarise(mean_stem = mean(stem_cm)) |>
  ggplot(aes(x = species_name,
             y = mean_stem)) +
  geom_bar(stat = "identity")


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = species_name,
           y = stem_cm)) +
  geom_bar(stat = "summary",
           fun = "mean")


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = species_name,
           y = stem_cm)) +
  geom_boxplot()


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = species_name,
           y = stem_cm)) +
  geom_violin() +
  geom_jitter(alpha = 0.5)


## ------------------------------------------------------------------


ggplot(dt_diam,
 aes(x = reorder(species_name, stem_cm),
     y = stem_cm)) +
  geom_violin() +
  geom_jitter(alpha = 0.5)


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = reorder(species_name, stem_cm),
           y = stem_cm)) +
  geom_violin() +
  geom_jitter(alpha = 0.5) +
  theme_linedraw()


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = reorder(species_name, stem_cm),
           y = stem_cm)) +
  geom_violin() +
  geom_jitter(alpha = 0.5) +
  theme_linedraw() +
  theme(axis.text.x = element_text(
          angle = 90,
          hjust = 1, vjust = 0.5,
          face = "italic"))


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = reorder(species_name, stem_cm),
           y = stem_cm)) +
  geom_violin() +
  geom_jitter(alpha = 0.5) +
  theme_linedraw() +
  theme(axis.text.x = element_text(
          angle = 90,
          hjust = 1, vjust = 0.5,
          face = "italic"),
        axis.text.y = element_text(
          size = 15, face = "bold"))


## ------------------------------------------------------------------


ggplot(dt_diam,
       aes(x = reorder(species_name, stem_cm),
           y = stem_cm)) +
  geom_violin() +
  geom_jitter(alpha = 0.5) +
  theme_linedraw() +
  theme(axis.text.x = element_text(
          angle = 90,
          hjust = 1, vjust = 0.5,
          face = "italic"),
        axis.text.y = element_text(
          size = 15, face = "bold")) +
  labs(x = NULL,
       y = "Stem diameter (cm)",
       title = "Diameter of Pinaceae species in USA National Parks")


## ----eval=F--------------------------------------------------------
## theme_set(theme_minimal())


## ----eval = F------------------------------------------------------
## library(hrbrthemes)
## library(ggthemes)
## library(ggpomological)
## library(tvthemes)
## library(ggtech)
## library(ggthemr)
## library(ggsci)


## ------------------------------------------------------------------
#| output-location: slide
library(patchwork)

p1 <- ggplot(dt_diam,
       aes(x = stem_cm,
           fill = genus)) +
  geom_density(alpha = 0.3)

p2 <- ggplot(dt,
       aes(x = stem_cm,
           fill = shade_tolerance)) +
  geom_density(alpha = 0.3)

p1 + p2 +
  plot_annotation(
  title = "Diameter distribution",
  tag_levels = 'A')


## ------------------------------------------------------------------
#| eval: false
## ggsave(p1, width = 20, units = "cm",
##   filename = here("img/figure_1.pdf"))


## ----eval=F--------------------------------------------------------
## GGally::ggpairs(dataset)


## ------------------------------------------------------------------


ggplot(dt,
       aes(x = growth_form,
           y = fruits)) +
  geom_violin() +
  labs(x = "Tipo de crecimiento",
       y = "Fruits per m2 (log scale)") +
  scale_y_log10()


## ------------------------------------------------------------------


dt |> filter(genus == "Quercus") |>
  ggplot(aes(x = year,
             y = fruits,
             color = species_name)) +
  geom_point(alpha = 0.4)  +
  geom_smooth() +
  scale_color_viridis_d() +
  labs(x = "Year",
       y = "Fruits per m2",
       color = "Quercus sp.")


## ------------------------------------------------------------------
dt |> filter(genus == "Quercus") |>
  ggplot(aes(x = year,
             y = fruits,
             color = species_name)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth() +
  scale_color_viridis_d() +
  facet_wrap(~species_name, scales = "free", nrow = 1) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1),
        strip.text = element_text(face = "italic")) +
  labs(x = "Year",
       y = "Fruits per m2")



## ------------------------------------------------------------------


dt |>
  #remove outlier year
  filter(!(site == "BNZ" & year == 1958)) |>
  group_by(site, year) |>
  summarise(mean_fruits = mean(fruits, na.rm = TRUE),
            se_fruits = sd(fruits, na.rm = TRUE)/sqrt(n())) |>
  ggplot(aes(x = year, y = mean_fruits)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_fruits - se_fruits,
                    ymax = mean_fruits + se_fruits)) +
  geom_line() +
  facet_wrap(~site, scales = "free") +
  theme_bw()


## ------------------------------------------------------------------


dt |>
  #remove outlier year
  filter(!(site == "BNZ" & year == 1958)) |>
  ggplot(aes(x = year, y = fruits)) +
  geom_point(stat = "summary", fun = "mean") +
  geom_line(stat = "summary", fun = "mean") +
  stat_summary(
    fun.data = "mean_se",
    geom = "errorbar",
    width = 0.2) +
  facet_wrap(~site, scales = "free") +
  theme_bw()

