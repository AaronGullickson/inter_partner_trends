# Create Figures for Poster

# Setup -------------------------------------------------------------------

# load packages and functions
library(here)
source(here("utils","check_packages.R"))
source(here("utils","functions.R"))

# load data
load(here("data", "data_constructed", "exogamy_trends.RData"))
load(here("data", "data_constructed", "model_state_bs.RData"))
load(here("data", "data_constructed", "model_basic_bs.RData"))
load(here("data", "data_constructed", "models_subgroups.RData"))
load(here("data", "data_constructed", "models_counterfactual.RData"))
load(here("data", "data_constructed", "model_region.RData"))
load(here("data", "data_constructed", "models_state_gen_bs.RData"))
load(here("data", "data_constructed", "model_recent_mar.RData"))
load(here("data", "data_constructed", "models_cohab.RData"))
load(here("data", "data_constructed", "model_recent_mar.RData"))
load(here("data", "data_constructed", "joint_age_dist_newlyweds.RData"))
load(here("data", "data_constructed", "age_weight_example.RData"))


# Set color palettes ------------------------------------------------------

palette <- c(
  # Single race combinations
  "White/Black"                   = "#FF3B00",  # rich orange
  "White/AIAN"                    = "#00785E",  # teal green
  "White/API"                     = "#00A6D6",  # muted magenta
  "White/Hispanic"                = "#661100",  # deep rust
  "Black/AIAN"                    = "#E6D945",  # warm brown
  "Black/API"                     = "#74AA99",  # muted cyan
  "Black/Hispanic"                = "#0B3D91",  # deep burgundy
  "AIAN/API"                      = "#F05A1A",  # desaturated turquoise
  "AIAN/Hispanic"                 = "#C0408C",  # dusty rose
  "API/Hispanic"                  = "#FFC107",  # yellow-gold
  
  # Single race to multiracial combinations
  "White/White-Black"             = "#3E8F0D",  # vivid spring green
  "Black/White-Black"             = "#471624",  # deep maroon-brown
  "White/White-AIAN"              = "#361892",  # royal indigo
  "AIAN/White-AIAN"               = "#C9A227",  # bright teal-blue
  "White/White-API"               = "#CC6666",  # slate blue
  "API/White-API"                 = "#6D7003",  # olive drab
  "Black/Black-API"               = "#1117AA",  # bold cobalt blue
  "API/Black-API"                 = "#6D4113",  # dark umber
  "Black/Black-AIAN"              = "#7171BB",  # muted periwinkle
  "AIAN/Black-AIAN"               = "#422D13",  # earthy espresso
  
  # Hispanic subgroups with White and Black
  "White/White Hispanic"           = "#552288",  # soft purple
  "White/Black Hispanic"           = "#E1A400",  # rose plum
  "White/Other Hispanic"           = "#287B33",  # leafy green
  "Black/White Hispanic"           = "#33A02C",  # olive yellow
  "Black/Black Hispanic"           = "#9C2C5C",  # reddish brick
  "Black/Other Hispanic"           = "#2C37DA",  # vivid sapphire
  
  # Asian subgroups with White
  "White/EastAsian"               = "#3366AA",  # steely blue
  "White/E&SE Asian"              = "#3311EE",  # steel sky blue
  "White/SEAsian"                 = "#118877",  # sea green
  "White/South Asian"             = "#009E73",  # muted violet-magenta
  
  # Indigenous subgroups with White
  "White/AIAN"                    = "#CC4477",  # muted raspberry rose
  "White/Pac. Islander"           = "#FF8C00",  # orange red
  
  # Intra-Hispanic
  "WhiteHispanic/BlackHispanic"   = "#BB8866",  # muted tan
  "WhiteHispanic/OtherHispanic"   = "#885577",  # dusty mauve
  "BlackHispanic/OtherHispanic"   = "#AA8844",  # muted amber
  
  # Intra-Asian
  "EastAsian/SEAsian"             = "#336633",  # forest moss
  "EastAsian/SouthAsian"          = "#993399",  # grape violet
  "SEAsian/SouthAsian"            = "#CC6666",  # muted rose
  
  # Intra-Indigenous
  "AIAN/PI"                       = "#447777"   # muted teal gray
)


# Set theme ---------------------------------------------------------------

theme_poster <- function(base_size = 24) {
  theme_bw(base_size = base_size) +
    theme(
      #panel.border     = element_blank(),
      plot.background = element_rect(fill = "#F4F4F2", color = NA),
      panel.background = element_rect(fill = "#F4F4F2", color = NA),
      strip.background = element_rect(fill = "#D9D9D6"),
      legend.background = element_rect(fill = "#F4F4F2", color = NA),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}


# Overall Exogamy Rates ---------------------------------------------------

ggplot(exogamy_trend, aes(x = year, y = exogamy, group = 1))+
  geom_ribbon(aes(ymin = exogamy-1.96*exogamy_se,
                  ymax = exogamy+1.96*exogamy_se),
              alpha = 0.3, color = NA)+
  geom_line()+
  geom_point()+
  scale_y_continuous(labels = scales::percent)+
  labs(y = "percent racially exogamous")+
  theme_poster(20)

ggsave(
  here("_products", "presentation","overall_exogamy.pdf"),
  height = 5.2,
  width = 9.94,
  units = "in",
  device = cairo_pdf
)

# Exogamy Rates by Race ---------------------------------------------------

exogamy_trend_race |>
  filter(!str_detect(race, "-")) |>
  ggplot(aes(x = year, y = exogamy, color = spouse, group = spouse))+
  geom_ribbon(aes(ymin = exogamy-1.83*exogamy_se,
                  ymax = exogamy+1.83*exogamy_se, 
                  fill = spouse),
              alpha = 0.3, color = NA)+
  geom_line(alpha = 0.7)+
  geom_point(size = 1, alpha = 0.7)+
  facet_wrap(~race, ncol = 2)+
  labs(y = "percent racially exogamous")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("red", "navy", "gold"))+
  theme_poster(18)+
  theme(legend.position = "bottom") 

ggsave(
  here("_products", "presentation","exogamy_race.pdf"),
  height = 9.34,
  width = 10.1,
  units = "in",
  device = cairo_pdf
)


# Overall Trends ----------------------------------------------------------

model_single_state_bs <- model_state_bs |>
  filter(!str_detect(term, "-"))

lims <- exp(c(min(model_single_state_bs$conf.low), 
              max(model_single_state_bs$conf.high)))

# we need to facet instead of subcap because word doesn't like it. Also it will
# be a pain for journal editorial teams (sigh)
temp <- model_single_state_bs |>
  mutate(focal1 = str_extract(term, "^[^/]+"),
         focal2 = str_extract(term, "(?<=/).*")) |>
  pivot_longer(cols = starts_with("focal"), values_to = "focal") |>
  mutate(focal = factor(
    focal, 
    levels = c("White", "Black", "AIAN", "API", "Hispanic"),
    labels = paste0("Involving ", 
                    c("White", "Black", "AIAN", "API", "Hispanic"),
                    " spouse")
  ))

ggplot(temp, aes(x = year, y = exp(estimate), color = term))+
  #geom_vline(xintercept = 2000, linetype = 2)+
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = term),
              alpha = 0.3, color = NA)+
  geom_line()+
  geom_point(size = 0.5)+
  geom_text_repel(data = temp |> filter(year == max(year)),
                  aes(label = term),
                  hjust = 0,
                  nudge_x = 0.5,
                  direction = "y", 
                  segment.color = "grey80", 
                  size = 6,
                  force = 1)+
  facet_wrap(~focal, ncol = 2)+
  labs(y = "odds ratio", color = "pairing", fill = "pairing")+
  scale_y_log10(limits = lims, labels = scales::comma)+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), 
                     limits = c(1960, 2040))+
  scale_color_manual(values = palette)+
  scale_fill_manual(values = palette)+
  theme_poster(30)+
  theme(legend.position = "none")

ggsave(
  here("_products", "presentation","overall_trends.pdf"),
  height = 14.37,
  width = 17.74,
  units = "in",
  device = cairo_pdf
)



# Multiracial Trends ------------------------------------------------------

temp <- model_state_bs |>
  mutate(group = case_when(
    term == "White/Black" | str_detect(term, "White-Black") ~ "White-Black",
    term == "White/AIAN" | str_detect(term, "White-AIAN") ~ "White-AIAN",
    term == "White/API" | str_detect(term, "White-API") ~ "White-API",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(group)) |>
  mutate(group = factor(group, 
                        levels = c("White-Black", "White-AIAN", "White-API"))) |>
  filter(year >= 1990)

ggplot(temp, aes(x = year, y = exp(estimate), color = term))+
  #geom_vline(xintercept = 2000, linetype = 2)+
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = term),
              alpha = 0.3, color = NA)+
  geom_line()+
  geom_point(size = 0.5)+
  geom_text_repel(data = temp |> filter(year == max(year)),
                  aes(label = term),
                  hjust = 0,
                  nudge_x = 0.5,
                  direction = "y", 
                  segment.color = "grey80", 
                  size = 6,
                  force = 3)+
  facet_wrap(~group, ncol = 2)+
  labs(y = "odds ratio", color = "pairing", fill = "pairing")+
  scale_y_log10(labels = scales::comma)+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020), 
                     limits = c(1990, 2040))+
  scale_color_manual(values = palette)+
  scale_fill_manual(values = palette)+
  theme_poster()+
  theme(legend.position = "none")

ggsave(
  here("_products", "presentation","multiracial_trends.pdf"),
  height = 8.5,
  width = 12.78,
  units = "in",
  device = cairo_pdf
)



 # Multiracial Counterfactual ----------------------------------------------

model_state_bs |>
  mutate(counterfactual = "Actual") |>
  bind_rows(model_cf_white_state_bs) |>
  bind_rows(model_cf_minority_state_bs) |>
  bind_rows(model_cf_best_state_bs) |>
  filter(counterfactual == "Actual" | year >= 1990) |>
  filter(term == "White/Black" | term == "White/API" | term == "White/AIAN") |>
  mutate(counterfactual = factor(counterfactual, 
                                 levels = c("Actual", "White", "Minority", 
                                            "Best"))) |>
  ggplot(aes(x = year, y = exp(estimate), color = counterfactual))+
  #geom_vline(xintercept = 2000, linetype = 2)+
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), 
                  fill = counterfactual),
              alpha = 0.3, color = NA)+
  geom_line()+
  geom_point(size = 0.5)+
  facet_wrap(~term, ncol = 2)+
  scale_y_log10(labels = scales::comma)+
  labs(y = "odds ratio", 
       color = "counterfactual\nsingle race",
       fill = "counterfactual\nsingle race")+
  scale_color_manual(values = c("navy", "gold", "red", "#004C2E"))+
  scale_fill_manual(values = c("navy", "gold", "red", "#004C2E"))+
  theme_poster(base_size = 14)+
  theme(legend.position = "bottom")

ggsave(
  here("_products", "presentation","multiracial_cf.pdf"),
  height = 4.94,
  width = 6.74,
  units = "in",
  device = cairo_pdf
)


# Hispanic Subgroup Trends ------------------------------------------------

lims <- exp(c(min(model_hispanic_sub_state_bs$conf.low), 
              max(model_hispanic_sub_state_bs$conf.high)))

temp <- model_hispanic_sub_state_bs |>
  filter(term == "White/Black") |>
  mutate(group = "Black Spouse")

temp <- model_hispanic_sub_state_bs |>
  filter(!str_detect(term, "Hispanic/")) |>
  mutate(group = if_else(str_detect(term, "^Black"), "Black Spouse", "White Spouse"),
         term = str_replace(term, "Hispanic", " Hispanic")) |>
  bind_rows(temp) |>
  mutate(group = factor(group, levels = c("White Spouse", "Black Spouse")),
         term = factor(term, 
                       levels = c("White/White Hispanic",
                                  "White/Other Hispanic",
                                  "White/Black Hispanic",
                                  "Black/Black Hispanic",
                                  "Black/Other Hispanic",
                                  "Black/White Hispanic",
                                  "White/Black"))) |>
  filter(year >= 1980)

ggplot(temp, aes(x = year, y = exp(estimate), color = term))+
  #geom_vline(xintercept = 2000, linetype = 2)+
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = term),
              alpha = 0.3, color = NA)+
  geom_line()+
  geom_point(size = 0.5)+
  geom_text_repel(data = temp |> filter(year == max(year)),
                  aes(label = term),
                  hjust = 0,
                  nudge_x = 0.5,
                  direction = "y", 
                  segment.color = "grey80", 
                  size = 5,
                  force = 3)+
  facet_wrap(~group)+
  labs(y = "odds ratio", color = "pairing", fill = "pairing")+
  scale_y_log10(limits = lims, labels = scales::comma)+
  scale_x_continuous(breaks = c(1980, 2000, 2020), 
                     limits = c(1980, 2040))+
  scale_color_manual(values = palette)+
  scale_fill_manual(values = palette)+
  theme_poster()+
  theme(legend.position = "none")

ggsave(
  here("_products", "presentation","hispanic_trends.pdf"),
  height = 7.33,
  width = 14.13,
  units = "in",
  device = cairo_pdf
)


# Asian Ethnonational Trends ----------------------------------------------

temp <- model_asian_sub_simple_state_bs |>
  filter(str_detect(term, "^White/")) |>
  mutate(term = str_replace(term, "/EastAsian", "/E&SE Asian"),
         term = str_replace(term, "/PI", "/Pac. Islander"),
         term = str_replace(term, "/SouthAsian", "/South Asian"))

ggplot(temp, aes(x = year, y = exp(estimate), color = term))+
  #geom_vline(xintercept = 2000, linetype = 2)+
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = term),
              alpha = 0.3, color = NA)+
  geom_line()+
  geom_point(size = 0.5)+
  geom_text_repel(data = temp |> filter(year == max(year)),
                  aes(label = term),
                  hjust = 0,
                  nudge_x = 0.5,
                  direction = "y", 
                  segment.color = "grey80", 
                  size = 6,
                  force = 3)+
  scale_y_log10(labels = scales::comma)+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), 
                     limits = c(1960, 2035))+
  labs(y = "odds ratio", color = "pairing", fill = "pairing")+
  scale_color_manual(values = palette)+
  scale_fill_manual(values = palette)+
  theme_poster()+
  theme(legend.position = "none")

ggsave(
  here("_products", "presentation","asian_trends.pdf"),
  height = 7.33,
  width = 14.13,
  units = "in",
  device = cairo_pdf
)


# Cohabiting --------------------------------------------------------------



model_state_bs <- model_state_bs |>
  mutate(union_type = "marriage")

#model_state_combine_bs <- model_state_combine_bs |>
#  mutate(union_type = "both")

model_state_cohab_bs |>
  mutate(union_type = "cohabitation") |>
  #bind_rows(model_state_combine_bs) |>
  bind_rows(model_state_bs) |>
  filter(!str_detect(term, "-")) |>
  mutate(union_type = factor(union_type, 
                             levels = c("marriage", "cohabitation", "both"))) |>
  ggplot(aes(x = year, y = exp(estimate), color = union_type))+
  #geom_vline(xintercept = 2000, linetype = 2)+
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = union_type),
              alpha = 0.3, color = NA)+
  geom_line()+
  geom_point(size = 0.5)+
  labs(y = "odds ratio", color = "union type", fill = "union type")+
  facet_wrap(~term)+
  scale_y_log10(labels = scales::comma)+
  scale_color_manual(values = c("red", "navy", "darkgreen"))+
  scale_fill_manual(values = c("red", "navy", "darkgreen"))+
  theme_poster(base_size = 14)+
  theme(panel.spacing.x = unit(0.75, "lines"),
        legend.position = "bottom") 

ggsave(
  here("_products", "presentation","cohabiting_trends.pdf"),
  height = 4.94,
  width = 6.74,
  units = "in",
  device = cairo_pdf
)


# recent marriages comparison ---------------------------------------------

# add in missing values to break line
model_recent_mar <- expand_grid(term = unique(model_recent_mar$term), 
                                year = c(1990, 2000:2007),) |>
  bind_rows(model_recent_mar) |>
  mutate(identifier = "<= 1 year")

model_state_bs |>
  filter(!str_detect(term, "-")) |>
  mutate(identifier = "age weighted") |>
  bind_rows(model_recent_mar) |>
  ggplot(aes(x = year, y = exp(estimate), group = identifier, color = identifier))+
  geom_ribbon(aes(ymin = exp(conf.low), ymax = exp(conf.high), fill = identifier),
              alpha = 0.3, color = NA)+
  geom_point(size = 1)+
  geom_line()+
  facet_wrap(~term)+
  labs(y = "odds ratio", color = "restriction", fill = "restriction")+
  scale_color_manual(values = c("red", "navy", "darkgreen"))+
  scale_fill_manual(values = c("red", "navy", "darkgreen"))+
  scale_y_log10(labels = scales::comma)+
  theme_poster(base_size = 14)+
  theme(legend.position = "bottom")

ggsave(
  here("_products", "presentation","recent_mar.pdf"),
  height = 4.67,
  width = 8.81,
  units = "in",
  device = cairo_pdf
)


# Age weighting example ---------------------------------------------------

age_weight2008 |>
  ggplot(aes(x = age_husband, y = after_stat(density)))+
  geom_histogram(alpha = 0.7, fill = "red", binwidth = 1)+
  geom_histogram(alpha = 0.7, fill = "navy", binwidth = 1, 
                 aes(weight = weight_age))+
  geom_histogram(data = filter(age_weight2008, dur_mar <= 1),
                 fill = NA, color = "black",  binwidth = 1, linetype = 2)+
  labs(x = "husband's age")+
  theme_poster(base_size = 14)

ggsave(
  here("_products", "presentation","age_weight_example.pdf"),
  height = 4.67,
  width = 8.81,
  units = "in",
  device = cairo_pdf
)

qr_code("https://courageous-paprenjak-a54198.netlify.app/") |>
  generate_svg(filename = here("_products", "presentation", "qrcode.svg"),
               background = "transparent")
