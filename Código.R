library(ggplot2)
library(dplyr)
library(palmerpenguins)
library(ggthemes)
library(cowplot)
library(tidyr)
library(purrr)
library(ggExtra)
library(ggridges)
original <- read.csv("data/complete_renewable_energy_dataset.csv")
data <- original|> mutate(Country = case_when(
  Country == "Brazil"~"Brasil", Country == "Canada" ~ "Canadá", 
  Country == "France"~"Francia", Country == "Germany"~"Alemania", 
  Country == "Japan"~"Japón", Country == "Russia"~"Rusia", 
  Country == "USA"~"E.E.U.U.", Country == "Australia"~"Australia",
  Country == "China"~"China", Country == "India"~"India",
  TRUE~"Other")) %>% mutate(Energy.Type = case_when(
    Energy.Type == "Solar"~"Solar", Energy.Type == "Biomass" ~ "Biomasa", 
    Energy.Type == "Wind"~"Eólica", Energy.Type == "Hydro"~"Hidráulica", 
    Energy.Type == "Geothermal"~"Geotérmica", TRUE~"Other")) %>% 
  group_by(Country, Year, Energy.Type) |> 
  summarise(Total.Production = sum(Production..GWh.), Electricity.Prices = mean(Electricity.Prices), R.D.Expenditure = sum(R.D.Expenditure), CO2.Emissions = sum(CO2.Emissions), Investments..USD. = sum(Investments..USD.), Public.Awareness = mean(Public.Awareness), Exportaciones = sum(Energy.Exports), Importaciones = sum(Energy.Imports), Renewable.Energy.Jobs = sum(Renewable.Energy.Jobs), Renewable.Energy.Patents = sum(Renewable.Energy.Patents), Renewable.Energy.Education.Programs = max(Renewable.Energy.Education.Programs), Government.Policies= max(Government.Policies)) %>% 
  pivot_longer(cols = c("Importaciones", "Exportaciones"),
               names_to = "Energy.Flow.Type",
               values_to = "Energy.Flow")

# Emisiones  de CO2

#Ridges
df1 <- data.frame(Production.Range. = cut(data$Total.Production, breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000,  max(data$Total.Production)),  labels = c("(0,50]", "(50, 100]", "(100,150]", "(150,200]", "(200, 250]", "(250,300]", "(300,350]", "(350,400]", "(400,450]", "más de 450")))
df1 <- cbind(data, df1)
ggplot(df1, aes(x = CO2.Emissions, y = Production.Range., fill = Production.Range.)) +
  geom_density_ridges2(quantile_lines = TRUE, quantiles = 2, color = "white", alpha = 0.75) +
  scale_fill_brewer(type = "div", palette = 8)+
  labs(
       x = "Emisiones de CO2",
       y = "Producción Total (miles de GWh)",
       caption= "Información de https://www.kaggle.com/datasets/anishvijay/global-renewable-energy-and-indicators-dataset")+
  theme_ridges()+
  theme(legend.position = "none")
ggsave("producción_emisiones.jpg", width = 8)
#Eólica
ggplot(data, aes(x = Total.Production, y = CO2.Emissions, color = Energy.Type)) +
  geom_point()+
  scale_color_brewer(type = "qual", palette = 2)  +
  scale_x_continuous(limits = c(0, max(data$Total.Production)), breaks = c(0, 150000, 300000, 450000))+
  facet_wrap(~Energy.Type, ncol = 2)+
  labs(
    
    x = "Producción total (miles de GWh)",
    y = "Emisiones de CO2",
    caption= "Información de https://www.kaggle.com/datasets/anishvijay/global-renewable-energy-and-indicators-dataset")+
  theme_cowplot()+
  theme(legend.position = "none")
ggsave("producción_emisiones_tipo_energía.jpg", width = 8,  height = 7)


# Precios

#Producción
ggplot(data, aes(x = Total.Production, y = Electricity.Prices))+
  geom_point()+
  geom_hline(yintercept=0.29, linetype="dashed", color = "red")+
  labs(
       x = "Producción",
       y = "Precios",
       caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy- \n and-indicators-dataset")+
  theme_cowplot()+
  theme(legend.position = "none")
ggsave("precios_producción.jpg", width = 8, height = 5)

#Políticas de gobierno
data %>% 
  ggplot(aes(x = Electricity.Prices))+
  geom_density() +
  facet_wrap(~Government.Policies)+
  labs(
       x = "Precios",
       y = "Densidad",
       caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy- \n and-indicators-dataset")+
  theme_cowplot()+
  theme( 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none")
ggsave("precios_políticas.jpg", width = 8)

#Social

#Conciencia pública y programas educativos
data %>% ggplot(aes(Public.Awareness, fill = as.logical(Renewable.Energy.Education.Programs)))+
  geom_density()+
  facet_wrap(~Renewable.Energy.Education.Programs, ncol = 2)+
  scale_fill_manual(name="", 
                    labels = c("Con", "Sin"), 
                    values = c("TRUE"="#b8d8be", "FALSE"="#ae5a41"))+
  labs(
       x = "Conciencia pública",
       y = "Densidad",
       caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy- \n and-indicators-dataset")+
  theme_cowplot()+
  theme( 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    legend.position = "none")
ggsave("social_conciencia.jpg", width = 8)

#Trabajos y Programas
ggplot(data, aes(x = Total.Production, y = Renewable.Energy.Jobs))+
  geom_point(aes(color = as.logical(Renewable.Energy.Education.Programs)))+
  scale_color_manual(name="", 
                     labels = c("Con", "Sin"), 
                     values = c("TRUE"="#b8d8be", "FALSE"="#ae5a41"))+
  scale_x_continuous(breaks = c(0,  150000, 300000, 450000))+
  labs(
       x = "Producción",
       y = "Trabajos",
       caption= "Información de \n https://www.kaggle.com/datasets/anishvijay/global-renewable-energy- \n and-indicators-dataset")+
  facet_wrap(~Renewable.Energy.Education.Programs)+
  theme_cowplot()+
  theme(legend.position = "none")
ggsave("social_trabajos.jpg", width = 8)
