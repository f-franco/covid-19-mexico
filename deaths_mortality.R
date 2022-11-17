library(dplyr)
library(zoo)
library(ggplot2)
library(viridis)

dir <- "C:/Users/santa/Desktop/COVID"
setwd(dir)

datos <- "201108COVID19MEXICO.csv"
fecha_rec <- as.Date(paste("2020", substr(datos, 3, 4), substr(datos, 5, 6), sep = "-"))
covid19 <- read.csv(datos, stringsAsFactors = FALSE)

covid19$FECHA <- as.Date(covid19$FECHA_INGRESO)
covid19 <- filter(covid19, FECHA >= "2020-02-28", FECHA <= (fecha_rec - 14))

plot_data <- covid19 %>%
  group_by(FECHA) %>%
  summarise(CASOS_N = sum(RESULTADO_LAB == 1), FALL_N = sum(FECHA_DEF != "9999-99-99"), HOSP_N = sum(TIPO_PACIENTE == 2)) %>%
  mutate(RM_CASOS = rollmean(CASOS_N, k = 7, fill = NA), 
         RM_FALL = rollmean(FALL_N, k = 7, fill = NA),
         RM_HOSP = rollmean(HOSP_N, k = 7, fill = NA),
         TDL = RM_FALL/RM_CASOS)

plot_data <- filter(plot_data, TDL <= (quantile(plot_data$TDL, na.rm = TRUE)[4]+(IQR(plot_data$TDL, na.rm = TRUE)*2.5)))
  
ggplot(plot_data, aes(x = FECHA)) +
  geom_bar(aes(y = RM_FALL*2.5, fill = TDL), stat = "identity", width = 1) +
  geom_line(aes(y = RM_HOSP), size = 2, alpha = 0.5, lineend = "round") +
  scale_x_date(date_breaks = "1 month", limits = c(as.Date("2020-03-01"), NA)) +
  scale_fill_viridis_c(begin = 0, direction = -1, option = "A") +
  scale_y_continuous(sec.axis = sec_axis(~./2.5))