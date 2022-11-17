 library(dplyr)
library(zoo)
library(ggplot2)
library(viridis)

dir <- "C:/Users/santa/Desktop/COVID"
setwd(dir)

datos <- "201101COVID19MEXICO.csv"
fecha_rec <- as.Date(paste("2020", substr(datos, 3, 4), substr(datos, 5, 6), sep = "-"))
covid19 <- read.csv(datos, stringsAsFactors = FALSE)

covid19$FECHA <- as.Date(covid19$FECHA_INGRESO)
covid19 <- filter(covid19, FECHA >= "2020-02-28", FECHA <= (fecha_rec - 14))

# # Weekly plot
# covid19$SEMANA <- week(covid19$FECHA) - 9
# plot_data <- covid19 %>%
#   group_by(SEMANA) %>%
#   summarise(CASOS_N = sum(RESULTADO == 1),
#             PRUEBAS = sum(RESULTADO == 1 | 2),
#             TPR = (CASOS_N/PRUEBAS) * 100)
# 
# ggplot(plot_data, aes(x = SEMANA)) +
#   geom_bar(aes(y = PRUEBAS/2, fill = TPR), stat = "identity", width = 1) +
#   geom_line(aes(y = CASOS_N), size = 2, alpha = 0.5) +
#   scale_fill_viridis_c(option = "C") +
#   scale_y_continuous(
#     sec.axis = sec_axis(~.*2)
#   )
# 
# 7-day rolling average plot

plot_data <- covid19 %>%
  group_by(FECHA) %>%
  summarise(CASOS_N = sum(RESULTADO_LAB == 1),
            CASOS_S = sum(RESULTADO_LAB == 3),
            PRUEBAS = sum(RESULTADO_LAB == 1 | 2),
            TPR = (CASOS_N/PRUEBAS) * 100) %>%
  mutate(RM_PRUEBAS = rollmean(PRUEBAS, k = 7, fill = NA),
         RM_CASOS = rollmean(CASOS_N, k = 7, fill = NA),
         RM_CASOS_S = rollmean(CASOS_S, k = 7, fill = NA),
         RM_TPR = rollmean(TPR, k = 7, fill = NA))

ggplot(plot_data, aes(x = FECHA)) +
  geom_bar(aes(y = RM_PRUEBAS/2, fill = RM_TPR), stat = "identity", width = 1) +
  geom_line(aes(y = RM_CASOS), size = 2, alpha = 0.5, lineend = "round") +
  scale_x_date(date_breaks = "1 month") +
  scale_fill_viridis_c(option = "C") +
  scale_y_continuous(
    sec.axis = sec_axis(~.*2)
  )
