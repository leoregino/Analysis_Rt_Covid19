
library(dplyr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(EpiEstim)
library(grid)
library(gridExtra)

### 1. Load file from local ###
file_name <- "C:/Users/Leonardo REGINO/Documents/DATAMINH/Covid19/Dashboards/Data/Data_Covid19_Colombia.csv"
DataCovid19 <-  read.csv(file = file_name, sep = ",", header = TRUE )

DataCovid19$fis <- as.character.Date(DataCovid19$fis)

### 2. Parameters to filter ###
depto_input <- "Bogot� D.C."
city_input <- "Bogot� D.C."

data_filter <- DataCovid19 %>%
  filter(departamento == depto_input) %>% 
  filter(ciudad_de_ubicaci_n == city_input)

min_date <- min(data_filter$fis)
max_date <- max(data_filter$fis)

### If specific date interval is needed: ###
# min_date <- "2020-03-09T00:00:00.000"
# max_date <- "2020-09-15T00:00:00.000"


### 3. Filter Data ###
data_filter <- data_filter %>%
  filter(fis > min_date & fis < max_date )

### 4. Aggregate data ###
data_filter <- cbind(data_filter, data.frame(nb= rep(1,nrow(data_filter)) ) )

data_agg <- data_filter %>% 
  group_by(fis) %>% 
  summarise(nb = sum(nb))

### 4.1 Neglect first dates (Dates not continuous) ###
data_agg <- as.data.frame(data_agg[seq(4, nrow(data_agg)),] )

data_agg <- data_agg %>% rename( dates = fis, I = nb)
data_agg$dates <- as.Date(data_agg$dates)


### 5. Calculate R_t ###

rest <- estimate_R(
  data_agg,
  method = "uncertain_si",
  config = make_config(
    list(
      mean_si = 2.6, 
      std_mean_si = 1,
      min_mean_si = 1, 
      max_mean_si = 4.2,
      std_si = 1.5, 
      std_std_si = 0.5,
      min_std_si = 0.5, 
      max_std_si = 2.5,
      n1 = 100, n2 = 100
    )
  )
)


rest_param <- estimate_R(
  data_agg,
  method = "parametric_si",
  config = make_config(
    list( mean_si = 4.8 , std_si = 2.6
    )
  )
)



## TODO ##
## Implementar con distribuciones Gamma y Log Normal ##

n_days <- nrow(data_agg)
mean_c19_si <- 3.95
sd_c19_si <- 1.51

rest_gamma <- estimate_R(
  data_agg,
  method = "non_parametric_si",
  config = make_config(
    list( si_distr = discr_si(seq(0, n_days), mean_c19_si , sd_c19_si)
    )
  )
)


### 6. Plot R_t ###


p_inc_gamma <- plot(rest_gamma, "incid", legend = FALSE)
p_Rt_gamma <- plot(rest_gamma, "R", legend = FALSE)
gridExtra::grid.arrange(p_inc_gamma, p_Rt_gamma, ncol = 1 , top = textGrob("GAMMA") )


p_Rt <- plot(rest, "R", legend = FALSE)
p_inc <- plot(rest, "incid", legend = FALSE)

gridExtra::grid.arrange(p_inc, p_Rt, ncol = 1 , top = textGrob("Mean and Std Fitted"))

p_Rt_parm <- plot(rest_param, "R", legend = FALSE)
p_inc_parm <- plot(rest_param, "incid", legend = FALSE)

gridExtra::grid.arrange(p_inc_parm, p_Rt_parm, ncol = 1 , top = textGrob("Mean=4.8, STD=2.6") )

### 7. Mirror plot [New Cases] vs [Daily Deaths] ###

# 7.1 New Cases #
new_agg <- data_filter %>%
  group_by(fis) %>%
  summarise(nb = sum(nb))

# 7.2 Daily Deaths #
data_dead <- data_filter %>%
    filter(atenci_n == "Fallecido")

### 7.1 Aggregate data ###
dead_agg <- data_dead %>%
  group_by(fis) %>%
  summarise(nb = sum(nb))


new_agg$type <- "1. New Cases"
dead_agg$type <- "2. Deaths"
dead_agg$nb <- -dead_agg$nb

mirror_plot_data <- rbind(new_agg, dead_agg)

mirror_plot_data$fis <-  as.Date(mirror_plot_data$fis)


### Plot ### 
q <- ggplot(mirror_plot_data, aes(x=fis, y= nb, fill=type)) + 
  geom_bar(stat = "identity", position = "identity") + 
  scale_fill_manual( values =  c("dodgerblue", "coral2") ) +
  ggtitle(paste0("New cases vs Daily Deaths for " , "Colombia") ) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_grid(type ~ . , scales = "free_y")

q

###


