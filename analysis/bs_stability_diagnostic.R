# This script will assess how much the variance of bootstrapped inference 
# parameters change with increased numbers of replicates


# load packages ----------------------------------------------------------

library(here)
source(here("utils","check_packages.R"))
source(here("utils","functions.R"))

# load data --------------------------------------------------------------

load(here("data", "data_constructed", "individ_data.RData"))

# relevel year and factor up anything that might go into models
census <- census |>
  mutate(year = factor(year),
         year = relevel(year, "2000"),
         region = factor(region),
         division = factor(division),
         state = factor(state))

# run models -------------------------------------------------------------

# run model on a more sensitive outcome as these will be the cases that 
# require the most replicates - Black/Asian intermarriage in 1960 is a good
# candidate

replicates <- c(10, 20, 50, 100, 200)
test <- map(replicates, function(r) {
  map(seq_len(20), function(i) {
    census |>
      filter(gen_husband == "2nd+" & gen_wife == "2nd+") |>
      filter(year == "1960") |>
      bootstrap_model(c("Black", "Asian"),
                      n_replicates = r,
                      show_progress = TRUE) |>
      select(std.error, conf.low, conf.high) |>
      mutate(B = r)
  })
}) |> bind_rows()

test |>
  mutate(conf.int = conf.high - conf.low) |>
  group_by(B) |>
  summarize(sd_se = sd(std.error),
            sd_high = sd(conf.high),
            sd_low = sd(conf.low),
            sd_int = sd(conf.int))

test |>
  ggplot(aes(x = factor(B), y = std.error))+
  geom_boxplot()+
  geom_violin(color = NA, fill = "blue", alpha = 0.2)+
  geom_jitter()

test |>
  ggplot(aes(x = factor(B), y = conf.high))+
  geom_boxplot()+
  geom_violin(color = NA, fill = "blue", alpha = 0.2)+
  geom_jitter()

test |>
  ggplot(aes(x = factor(B), y = conf.low))+
  geom_boxplot()+
  geom_violin(color = NA, fill = "blue", alpha = 0.2)+
  geom_jitter()

test |>
  mutate(conf.int = conf.high - conf.low) |>
  ggplot(aes(x = factor(B), y = conf.int))+
  geom_boxplot()+
  geom_violin(color = NA, fill = "blue", alpha = 0.2)+
  geom_jitter()
  
