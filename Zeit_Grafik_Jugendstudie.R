library(readxl)
library(dplyr)
library(ggplot2)


# Daten basieren auf der Grafik im Artikel. 
## Ich habe keinen Zugang zu den Daten, ich habe die Prozent aus der Grafik übernommen. 
excel_path <- "data.xlsx"
data <- read_excel(excel_path)

# Summe Prozent pro Jahr sollte 100 sein: 
data <- data %>%
  group_by(jahr) %>%
  mutate(total_prozent = sum(prozent)) %>%
  ungroup() 

## Ist sie aber nicht. Das muss falsch sein.

# Ich kenne die Daten nicht und habe keinen Zugang. 
## Ich weiß: 2042 Respondents sind in den Daten. Die verteile ich nahezu identisch über drei Jahre:
total_befrage <- c("2022" = 2042, "2023" = 2042, "2024" = 2042)  
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
  scale_color_manual(values = c("Grüne" = "green", "Weiß nicht" = "grey50", "CDU/CSU" = "black", 
                                "keine Stimme" = "red", "FDP" = "yellow", "Linke" = "purple", 
                                "SPD" = "blue", "AfD" = "lightblue", "BSW" = "pink", "Sonstige" = "cyan"))

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
scale_color_manual(values = c("Grüne" = "green", "Weiß nicht" = "grey50", "CDU/CSU" = "black", 
                              "keine Stimme" = "red", "FDP" = "yellow", "Linke" = "purple", 
                              "SPD" = "blue", "AfD" = "lightblue", "BSW" = "pink", "Sonstige" = "cyan"))

ggsave("fig2.pdf", plot = fig2, device = "pdf")

## überlappt sehr viel 

# nur AfD und Grüne: 
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


#Ok. Wir könnten mal annehmen, dass weiß nicht und keine Stimme aus einer anderen Variable kommen. 
## -> Auch dann verstehe ich nicht wieso man nicht auf 100 Prozent geht. 

data_100 <- data %>%
  filter(!partei %in% c("Weiß nicht", "keine Stimme")) %>%
  select(-total_prozent, -N, -SE, -CI_lower, -CI_upper)

# Nun nochmal alles neu: 

# Summe Prozent pro Jahr sollte 100 sein: 
data_100 <- data_100 %>%
  group_by(jahr) %>%
  mutate(total_prozent = sum(prozent)) %>%
  ungroup() 

## das könnte hinhauen! 
## ich brauche das aber im Grunde nicht neu berechnen, weil ich sowieso auf % gerechnet habe. Sollte sich nix. verändern.

# Ich kenne die Daten nicht und habe keinen Zugang. 
## Ich weiß: 2042 Respondents sind in den Daten. Die verteile ich nahezu identisch über drei Jahre:
total_befrage <- c("2022" = 2042, "2023" = 2042, "2024" = 2042)  
### Annahme kann falsch sein. Sofern ja, bitte korrigierne. 

# Nun kann ich ungefähr von Prozent zu total kommen. 
## ABER: Klar geht sich das nicht aus, es wird Kommastellen geben. Ich runde also. 
data_100 <- data_100 %>%
  mutate(
    N = round(prozent / 100 * total_befrage[as.character(jahr)])
  )

# gut aber das bedeutet nun auch das 30% des samples hier nicht drin stecken: 
data_100$sub_prop <- 0.70

# Adjusted percentages for the entire sample
data_100$korrigierte_prozent <- data_100$prozent * data_100$sub_prop

# To view the adjusted percentages
data_100$korrigierte_prozent

## rechnen wir das wiederum um. Laut update sind es in etwa 2000 pro Jahr:
total_befrage <- c("2022" = 2000, "2023" = 2000, "2024" = 2000)  

# Nun kann ich ungefähr von Prozent zu total kommen. 
## ABER: Klar geht sich das nicht aus, es wird Kommastellen geben. Ich runde also. 
data_100 <- data_100 %>%
  mutate(
    korrigierte_N = round(korrigierte_prozent / 100 * total_befrage[as.character(jahr)])
  )

# wieder CIs: 
data_100 <- data_100 %>% 
  mutate(
    korrigierte_prozent_decimal = korrigierte_prozent / 100,  
    SE = sqrt(prozent_decimal * (1 - korrigierte_prozent_decimal) / korrigierte_N),
    CI_lower = korrigierte_prozent - (1.96 * SE * 100),  
    CI_upper = korrigierte_prozent + (1.96 * SE * 100)   
  )


# hier kommt der Graf wie im Artikel: 
fig1_korrigiert <- ggplot(data_100, aes(x=jahr, y=korrigierte_prozent, group=partei, color=partei)) +
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
  scale_color_manual(values = c("Grüne" = "green", "Weiß nicht" = "grey50", "CDU/CSU" = "black", 
                                "keine Stimme" = "red", "FDP" = "yellow", "Linke" = "purple", 
                                "SPD" = "blue", "AfD" = "lightblue", "BSW" = "pink", "Sonstige" = "cyan"))

ggsave("fig1_korrigiert.pdf", plot = fig1_korrigiert, device = "pdf")

# Ok, nun geben wir das in die Grafik ein: 
fig2_korrigiert <- ggplot(data_100, aes(x=jahr, y=korrigierte_prozent, group=partei, color=partei)) +
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
  scale_color_manual(values = c("Grüne" = "green", "Weiß nicht" = "grey50", "CDU/CSU" = "black", 
                                "keine Stimme" = "red", "FDP" = "yellow", "Linke" = "purple", 
                                "SPD" = "blue", "AfD" = "lightblue", "BSW" = "pink", "Sonstige" = "cyan"))

ggsave("fig2_korrigiert.pdf", plot = fig2_korrigiert, device = "pdf")

## überlappt sehr viel 

# nur AfD und Grüne: 
data_100_filtered <- data_100 %>% filter(partei %in% c("AfD", "Grüne"))

# Create the line plot for only "AfD" and "Grüne"
fig3_korrigiert <- ggplot(data_100_filtered, aes(x=jahr, y=korrigierte_prozent, group=partei, color=partei)) +
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

ggsave("fig3_korrigiert.pdf", plot = fig3_korrigiert, device = "pdf")

