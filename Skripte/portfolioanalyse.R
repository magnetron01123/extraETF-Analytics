# Pakete laden ------------------------------------------------------------

library(tidyverse)
library(tidyquant)

library(janitor)

library(scales)
library(patchwork)


# Grundeinstellungen ------------------------------------------------------

# Pfade festlegen
pfad_datensatz <- file.path(getwd(), "Datensatz")
pfad_diagramme <- file.path(getwd(), "Diagramme")


# Import ------------------------------------------------------------------

transaktionen_rohdaten <- 
  list.files(
    pfad_datensatz,
    pattern = "(?i).*extraetf.*\\.csv$",
    full.names = TRUE,
    include.dirs = FALSE
  ) |> 
  map_dfr(~ 
    read_delim(
      .x,
      delim = ",",
      quote = "\"", 
      locale = locale(decimal_mark = ",")
    )
  )

glimpse(transaktionen_rohdaten)


# Datenbereinigung --------------------------------------------------------

transaktionen_bereinigt <-
  transaktionen_rohdaten |> 
  distinct() |> 
  clean_names() |> 
  rename(
    kosten = kommission,
    waehrung = wahrung
  ) |> 
  select(
    -portfolio_id,
    -quote_provider,
    -wechselkurs
  ) |> 
  mutate(
    datum = dmy(datum),
    anzahl = if_else(
      transaktionsart %in% c("Verkauf"), 
      -anzahl, 
      anzahl
    ),
    buchwert = -anzahl * kurs
  ) |> 
  arrange(desc(datum))

print(transaktionen_bereinigt)
View(transaktionen_bereinigt)


# Analyse -----------------------------------------------------------------

# Portfolio

aktuelles_portfolio <-
  transaktionen_bereinigt |> 
  group_by(isin) |> 
  summarise(
    name = first(name),
    wertpapiertyp = first(wertpapiertyp),
    stueck = sum(if_else(transaktionsart %in% c("Kauf", "Verkauf"), anzahl, 0)),
    kosten = sum(kosten),
    steuern = sum(steuern),
    kaufwert = -sum(buchwert)
  ) |> 
  filter(stueck > 0)

print(aktuelles_portfolio)

# Kosten

gewinne <-
  transaktionen_bereinigt |>
  group_by(isin, name) |>
  summarise(
    bestand = max(sum(if_else(transaktionsart %in% c("Kauf", "Verkauf"), anzahl, 0)), 0),
    stueck = sum(if_else(transaktionsart == "Kauf", anzahl, 0)),
    kaufwert = sum(if_else(transaktionsart == "Kauf", -buchwert, 0)),
    verkaufswert = sum(if_else(transaktionsart == "Verkauf", buchwert, 0)),
    kosten = sum(kosten),
    steuern = sum(steuern),
    .groups = "drop"
  ) |> 
  mutate(
    einkaufspreis = kaufwert / stueck,
    .after = stueck,
  ) |> 
  mutate(
    gewinn_realisiert_absolut = if_else(bestand == 0, verkaufswert - kaufwert -kosten - steuern, NA),
    gewinn_realisiert_relativ = if_else(bestand == 0, gewinn_realisiert_absolut / kaufwert, NA)
  ) |> 
  select(
    -bestand
  ) |> 
  arrange(desc(gewinn_realisiert_absolut))

print(gewinne)


# diagramme ---------------------------------------------------------------

diagramme <- list()


# Trades

diagramme$trades <-
  transaktionen_bereinigt |> 
  filter(
    transaktionsart %in% c("Kauf", "Verkauf")
  ) |> 
  mutate(
    jahr = year(datum),
    monat = month(datum),
    .after = datum
  ) |> 
  group_by(portfolioname, jahr, monat, transaktionsart) |> 
  summarise(
    buchwert = -sum(buchwert),
    trades = n(),
    .groups = "drop"
  ) |> 
  mutate(
    datum = make_date(jahr, monat),
    .before = 1
  ) |> 
  ggplot(
    aes(
      x = datum,
      y = buchwert,
      fill = transaktionsart
    )
  ) +
  geom_col() +
  scale_fill_manual(
    name = "Transaktionsart",
    values = c("Kauf" = "green", "Verkauf" = "red")
  ) +
  geom_hline(
    yintercept = 0
  ) +
  geom_smooth(
    aes(
      y = if_else(buchwert > 0, buchwert, NA)
    ),
    method = "lm",
    se = FALSE,
    color = "black",
    linetype = "dashed",
    show.legend = FALSE
  ) +
  labs(
    title = "Käufe und Verkäufe im Zeitverlauf",
    subtitle = "Linearer Trend zeigt die Entwicklung der Sparrate",
    x = "Datum",
    y = "Buchwert"
  ) +
  theme_classic() +
  scale_x_date(
    date_breaks("1 month")
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ".", 
      decimal.mark = ",",
      suffix = "€"
    )
  ) +
  facet_wrap(
    ~ portfolioname,
    scales = "free_y",
    ncol = 1
  ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = "black", 
      fill = NA, 
      linewidth = 1
    ),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title.position = "plot",
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0)
  )


# Gewinne und Verluste

diagramme$gewinne <-  
  gewinne |> 
  arrange(desc(gewinn_realisiert_absolut)) |> 
  mutate(
    name = factor(name, levels = rev(name)),
    ergebnis = case_when(
      gewinn_realisiert_absolut > 0 ~ "Gewinn",
      gewinn_realisiert_absolut < 0 ~ "Verlust",
      TRUE ~ NA
    )
  ) |> 
  drop_na() |> 
  ggplot(
    aes(
      x = name,
      y = gewinn_realisiert_absolut,
      fill = ergebnis
    )
  ) +
  geom_col() +
  scale_fill_manual(
    name = "Ergebnis",
    values = c(
      "Gewinn" = "green",
      "Verlust" = "red"
    )
  ) +
  geom_hline(yintercept = 0) +
  geom_text(
    aes(
      x = name,
      y = gewinn_realisiert_absolut + if_else(ergebnis == "Gewinn", 2.5, -2.5),
      label = percent(
        gewinn_realisiert_relativ, 
        accuracy = 1
      ),
      hjust = if_else(ergebnis == "Gewinn", 0, 1)
    ),
    size = 3
  ) +
  geom_text(
    aes(
      x = name,
      y = if_else(ergebnis == "Gewinn", -2.5, 2.5),
      label = name,
      hjust = if_else(ergebnis == "Gewinn", 1, 0)
    ),
    size = 3
  ) +
  coord_flip() +
  scale_x_discrete(expand = expansion(add = c(0.75 , 0.75))) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ".",
      decimal.mark = ",",
      suffix = "€"
    ),
    limits = c(
      -max(abs(gewinne$gewinn_realisiert_absolut), na.rm = TRUE) * 1.1,
      max(abs(gewinne$gewinn_realisiert_absolut), na.rm = TRUE) *1.1
    )
  ) +
  labs(
    title = "Realisierte Gewinne und Verluste",
    subtitle = if_else(
      sum(gewinne$gewinn_realisiert_absolut, na.rm = TRUE) > 0,
      str_glue(
        "Gesamtgewinn von ",
        "{number(sum(gewinne$gewinn_realisiert_absolut, na.rm = TRUE), 
        big.mark = '.', 
        decimal.mark = ',', 
        accuracy = 1, 
        suffix = '€'
        )}"
      ),
      str_glue(
        "Gesamtverlust von ",
        "{number(sum(gewinne$gewinn_realisiert_absolut, na.rm = TRUE), 
        big.mark = '.', 
        decimal.mark = ',', 
        accuracy = 1, 
        suffix = '€'
        )}"
      )
    ),
    y = "Gewinn oder Verlust"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title.position = "plot",
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.line.x = element_line(
      arrow = arrow(length = unit(0.5, "npc"), type = "closed", ends = "both"),
      color = "black"
    ),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )


# Infografik --------------------------------------------------------------

diagramme$infografik <-
  diagramme$trades + diagramme$gewinne +
  plot_annotation(
    title = "Portfolioanalyse",
    theme = theme(
      plot.title = element_text(hjust = 0.5)
    )
  )


# Dateien speichern -------------------------------------------------------

# Plots in einer Schleife speichern und ausführen
walk2(rev(seq_along(diagramme)), rev(diagramme), function(nummer_diagramm, plot) {
  name_diagramm <- names(diagramme)[nummer_diagramm]
  name_datei <- str_glue("{nummer_diagramm}_{name_diagramm}.png")
  ggsave(
    filename = file.path(pfad_diagramme, name_datei),
    plot = plot,
    device = "png"
  )
  print(plot)
})
  