library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(skimr)
library(here)
library(janitor)
library(lubridate)
setwd("/home/fabian/R/proyectos/analisis-bellabeat/datasets")
act_diaria <- read.csv("dailyActivity_merged.csv")
calorias <- read.csv("hourlyCalories_merged.csv")
intensidad <- read.csv("hourlyIntensities_merged.csv")
tiempo_sueno <- read.csv ("sleepDay_merged.csv")
track_peso <- read.csv("weightLogInfo_merged.csv")

glimpse(act_diaria)
Sub_act_diaria <- act_diaria %>%
  mutate(day = mdy(ActivityDate)) %>%
  arrange(-Id,ActivityDate) %>%
  filter(TotalSteps != 0) %>%
  select(-LoggedActivitiesDistance)
head(Sub_act_diaria,50)
glimpse(calorias)
sub_calorias <- calorias %>%
  mutate(ActivityHour = mdy_hms(ActivityHour, tz = "UTC")) %>%
  mutate(hour = hour(ActivityHour)) %>%
  group_by(Id) %>%
  mutate(total_calories_per_month = sum(Calories), avg_month = mean(Calories), max_cal_burn = max(Calories), min_cal_burn = min(Calories))
head(sub_calorias)
sub_intensidad <- intensidad %>%
  mutate(ActivityHour = mdy_hms(ActivityHour, tz = "UTC"))
head(sub_intensidad)
sub_tiempo_sueno <- tiempo_sueno %>%
  mutate(day = mdy_hms(SleepDay, tz = "UTC")) %>%
  arrange(Id) %>%
  mutate(Difsueno_min = TotalTimeInBed - TotalMinutesAsleep, TotalMinutesAsleepHr = TotalMinutesAsleep/60, TotalTimeInBedHr = TotalTimeInBed/60)
head(sub_tiempo_sueno,150)

sub_track_peso <- track_peso %>%
  mutate(IsManualReport = as.logical(toupper(IsManualReport))) %>%
  mutate(Date = mdy_hms(Date, tz = "UTC")) %>%
  arrange(Id) %>%
  select(-Fat,-LogId)
head(sub_track_peso)
#calorias
calorias_limp <- sub_calorias %>%
  rename_with(tolower) %>%
  clean_names() %>%
  rename(calories_per_hour = calories) %>%
  mutate (cal_per_day = total_calories_per_month/31)
head(calorias_limp)

#Intensidad
intensidad_limp <- sub_intensidad %>%
  rename_with(tolower) %>%
  clean_names() 

#Sueno
tiempo_sueno_limp <- sub_tiempo_sueno  %>%
  rename_with(tolower) %>%
  clean_names()
#peso
track_peso_limp <- sub_track_peso %>%
  rename_with(tolower) %>%
  clean_names()
#actividad diaria
act_diaria_limp %>%
  select(-id,-activitydate, -trackerdistance,-distanciatotal,-comparar,-error_absoluto) %>%
  summary()
#calorias
calorias_limp %>%
  summary()
sub_calorias_horas <- calorias %>%
  mutate(ActivityHour = mdy_hms(ActivityHour, tz = "UTC")) %>%
  mutate(hour = hour(ActivityHour)) 

resumen <- sub_calorias_horas %>%
  group_by(hour) %>%
  summarise(promedio_calorias = mean(Calories, na.rm = TRUE))
ggplot(data = resumen, aes(x = hour, y = promedio_calorias)) + 
  geom_col(stat = "identity", fill = 'darkblue') + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Promedio de Calorías Quemadas por Hora",
       x = "Hora del Día",
       y = "Calorías Quemadas")

intensidad_horas <- intensidad_limp %>%
  mutate(hour = hour(activityhour))

resumen_int <- intensidad_horas %>%
  group_by(hour)
head(resumen_int,150)

ggplot(data = resumen_int, aes(x = hour, y = averageintensity)) + 
  geom_col(stat = "identity", fill = 'darkblue') + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Promedio de intensidad por Hora",
       x = "Hora del Día",
       y = "Promedio de intensidad")

track_peso_limp %>%
  select(-id,-date,-ismanualreport ) %>%
  summary()

graf <- track_peso_limp %>%
  select(-date,-ismanualreport) %>%
  group_by(bmi)

ggplot(graf, mapping = aes(x = bmi)) +
  geom_histogram( fill = "blue", color = "black") +
  labs(title = "Distribución del IMC de los usuarios",
       x = "IMC",
       y = "Frecuencia") +
  theme_minimal()
tiempo_sueno_limp %>%
  select(-id,-sleepday)
summary(tiempo_sueno_limp)
ggplot(data = tiempo_sueno_limp, mapping = aes( x = totaltimeinbedhr)) + geom_bar(color = "blue" , size = 1) 
merged_data <- merge(tiempo_sueno_limp, act_diaria_limp, by=c('id', 'day'))
head(merged_data)
ggplot(data=merged_data, aes(x=totalminutesasleep, y=sedentaryminutes)) + 
  geom_point(color='darkblue') + geom_smooth() +
  labs(title="Minutos durmiendo vs. Minutos sedentarios")
ggplot(data= act_diaria_limp, aes(x=totalsteps, y=calories)) + 
  geom_point() + geom_smooth() + labs(title="Pasos totales vs. calorias")
