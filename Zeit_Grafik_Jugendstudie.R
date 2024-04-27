library(readxl)
library(dplyr)
library(ggplot2)


# Load the data
excel_path <- "data.xlsx"
data <- read_excel(excel_path)

# View the first few rows of the data
str(data)
head(data)


# Ich kenne die Daten nicht und habe keinen Zugang. 
## Ich weiß: 2042 Respondents sind in den Daten. Die verteile ich nahezu identisch über drei Jahre:
total_befrage <- c("2022" = 680, "2023" = 681, "2024" = 681)  
### Annahme kann falsch sein. Sofern ja, bitte korrigierne. 



# Nun kann ich ungefähr von Prozent zu total kommen. 
## ABER: Klar geht sich das nicht aus, es wird Kommastellen geben. Ich runde also. 
data <- data %>%
  mutate(
    N = round(prozent / 100 * total_befrage[as.character(jahr)])
  )

# Wie verteilt sich das nun? 
head(data)

# hier kommt der Graf wie im Artikel: 
fig1 <- ggplot(data, aes(x=jahr, y=prozent, group=partei, color=partei)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x="Jahr", y="Prozent", title="Parteien über die Jahre") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank() # Remove x-axis major gridlines if desired
  ) +
  scale_x_continuous(breaks = unique(data$jahr)) +  # Set breaks at unique years
  scale_y_continuous(limits = c(0, NA)) +  # Start y-axis at 0
  scale_color_manual(values = c("Grüne" = "green", "Weiß nicht" = "grey50", "Union" = "black", 
                                "keine Stimme" = "blue", "FDP" = "yellow", "Linke" = "purple", 
                                "SPD" = "red", "AfD" = "lightblue", "BSW" = "pink", "Sonstige" = "cyan"))

ggsave("fig1_original.pdf", plot = fig1, device = "pdf")


# Ok, wir können 95% CIs rechnen: 
## wir wissen die Prozent 
data <- data %>% 
  mutate(
    prozent_decimal = prozent / 100,  
    SE = sqrt(prozent_decimal * (1 - prozent_decimal) / N),
    CI_lower = prozent - (1.96 * SE * 100),  
    CI_upper = prozent + (1.96 * SE * 100)   
  )

# Ok, nun geben wir das in die Grafik ein: 
fig2 <- ggplot(data, aes(x=jahr, y=prozent, group=partei, color=partei)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper), width=0.2) +
  theme_minimal() +
  labs(x="Jahr", y="Prozent", title="Umfrageergebnisse über die Jahre") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  scale_x_continuous(breaks = unique(data$jahr), expand = c(0, 0)) +  # Set breaks at unique years, remove expansion
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Start y-axis at 0, remove expansion
scale_color_manual(values = c("Grüne" = "green", "Weiß nicht" = "grey50", "Union" = "black", 
                              "keine Stimme" = "blue", "FDP" = "yellow", "Linke" = "purple", 
                              "SPD" = "red", "AfD" = "lightblue", "BSW" = "pink", "Sonstige" = "cyan"))

ggsave("fig2.pdf", plot = fig2, device = "pdf")

## überlappt sehr viel 

# nur AfD und Grüne: 
# Filter the data for only "AfD" and "Grüne"
data_filtered <- data %>% filter(partei %in% c("AfD", "Grüne"))

# Create the line plot for only "AfD" and "Grüne"
fig3 <- ggplot(data_filtered, aes(x=jahr, y=prozent, group=partei, color=partei)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper), width=0.2) +
  theme_minimal() +
  labs(x="Jahr", y="Prozent", title="Umfrageergebnisse über die Jahre - AfD und Grüne") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  scale_x_continuous(breaks = unique(data_filtered$jahr), expand = c(0, 0)) +  # Set breaks at unique years, remove expansion
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +  # Start y-axis at 0, remove expansion
  scale_color_manual(values = c("AfD" = "lightblue", "Grüne" = "green"))  # Define colors for "AfD" and "Grüne"

ggsave("fig3.pdf", plot = fig3, device = "pdf")


