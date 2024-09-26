# Pakete laden ------------------------------------------------------------

library(tidyverse)
library(tidyquant)

library(janitor)

# Diagramme
library(scales)
library(patchwork)


# Grundeinstellungen ------------------------------------------------------

# Pfade festlegen
pfad_datensatz <- file.path(getwd(), "Datensatz")
pfad_diagramme <- file.path(getwd(), "Diagramme")


# Import ------------------------------------------------------------------

# Transaktionen aus dem Portfolio

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

# Mapping zwischen ISIN und Symbol

mapping <-
  tibble(
    isin = c(
      "IE00B4L5Y983", 
      "IE00BF4RFH31",
      "IE00BKM4GZ66",
      "LU2572257124"
    ),
    tickersymbol = c(
      "EUNL", 
      "IUSN",
      "IS3N",
      "AHYQ"
    )
  ) |> 
  mutate(tickersymbol = paste0(tickersymbol, ".DE"))

# Marktpreise importieren

marktpreise <-
  tq_get(mapping$tickersymbol, get = "stock.prices") |> 
  left_join(
    mapping,
    join_by("symbol" == "tickersymbol")
  ) |> 
  rename(
    tickersymbol = symbol,
    datum = date,
    eroeffnungskurs = open,
    hoechstkurs = high,
    tiefstkurs = low,
    schlusskurs = close,
    handelsvolumen = volume,
    bereingter_schlusskurs = adjusted
  )

glimpse(marktpreise)

# Datenbereinigung --------------------------------------------------------

transaktionen_bereinigt <-
  transaktionen_rohdaten |> 
  distinct() |> 
  clean_names() |> 
  rename(
    kaufkurs = kurs,
    kosten = kommission,
    waehrung = wahrung
  ) |> 
  mutate(
    datum = dmy(datum),
    anzahl = if_else(
      transaktionsart == "Verkauf", 
      -anzahl, 
      anzahl
    ),
    buchwert = -anzahl * kaufkurs,
  ) |> 
  left_join(mapping) |> 
  select(
    -portfolio_id,
    -quote_provider,
    -wechselkurs
  ) |> 
  arrange(desc(datum))

print(transaktionen_bereinigt)
View(transaktionen_bereinigt)


# Analyse -----------------------------------------------------------------

# Portfolio

aktuelles_portfolio <-
  transaktionen_bereinigt |> 
  group_by(portfolioname, tickersymbol, isin, name, wertpapiertyp) |> 
  summarise(
    stueck = sum(
      if_else(
        transaktionsart %in% c("Kauf", "Verkauf", "Einbuchung"),
        anzahl, 
        0
      )
    ),
    kosten = sum(kosten),
    steuern = sum(steuern),
    kaufwert = abs(sum(buchwert)),
    .groups = "drop"
  ) |> 
  filter(stueck > 0) |> 
  left_join(filter(marktpreise, datum == max(datum))) |> 
  mutate(
    marktwert = stueck * bereingter_schlusskurs,
    gewinn = marktwert - kaufwert - steuern - kosten,
    rendite = gewinn / (kaufwert + kosten + steuern)
  ) |> 
  select(
    portfolioname,
    tickersymbol,
    isin,
    name,
    wertpapiertyp,
    marktwert,
    gewinn,
    rendite
  )

print(aktuelles_portfolio)

# Gewinne

performance <-
  transaktionen_bereinigt |>
  group_by(portfolioname, tickersymbol, isin, name, wertpapiertyp) |>
  summarise(
    bestand = max(
      sum(if_else(
        transaktionsart %in% c("Kauf", "Verkauf"), 
        anzahl, 
        0)
      ), 
    0),
    stueck = sum(
      if_else(
        transaktionsart == "Kauf", 
        anzahl, 
        0
      )
    ),
    kaufwert = sum(
      if_else(
        transaktionsart == "Kauf", 
        abs(buchwert), 
        0
      )
    ),
    verkaufswert = sum(
      if_else(
        transaktionsart == "Verkauf", 
        buchwert, 
        0
      )
    ),
    kosten = sum(kosten),
    steuern = sum(steuern),
    .groups = "drop"
  ) |> 
  left_join(filter(marktpreise, datum == max(datum))) |>
  mutate(
    realisierung = factor(
      ifelse(
        bestand == 0, 
        "realisiert", 
        "unrealisiert"
      ),
      levels = c("unrealisiert", "realisiert")
    ),
    marktwert = stueck * bereingter_schlusskurs,
    gewinn = if_else(
      bestand == 0, 
      verkaufswert - kaufwert -kosten - steuern, 
      marktwert - kaufwert - kosten - steuern
    ),
    rendite = gewinn / (kaufwert + kosten + steuern)
  ) |> 
  select(
    portfolioname,
    tickersymbol,
    isin,
    name,
    wertpapiertyp,
    gewinn,
    rendite,
    realisierung
  ) |> 
  # Bei Einbuchungen ohne Kaufwert können Inf entstehen
  filter(is.finite(rendite)) |> 
  arrange(desc(realisierung), desc(gewinn)) 

print(performance)


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
    aes(y = if_else(buchwert > 0, buchwert, NA)),  # Nur positive Werte verwenden
    method = "glm", 
    method.args = list(family = Gamma(link = "log")),  # Gamma-Regression für positive Linie
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
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    strip.text = element_text(hjust = 0.5),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = "black", 
      fill = NA, 
      linewidth = 1
    )
  )

# Gewinne und Verluste

diagramme$performance <-  
  performance |> 
  arrange(desc(realisierung), desc(rendite)) |> 
  mutate(
    name = factor(name, levels = rev(name)),
    ergebnis = case_when(
      gewinn > 0 ~ "Gewinn",
      gewinn < 0 ~ "Verlust",
      TRUE ~ NA
    )
  ) |> 
  ggplot(
    aes(
      x = name,
      y = rendite,
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
      y = rendite + if_else(ergebnis == "Gewinn", 0.01, -0.01),
      label = number(
        gewinn, 
        accuracy = 1,
        suffix = "€"
      ),
      hjust = if_else(ergebnis == "Gewinn", 0, 1)
    ),
    size = 3
  ) +
  geom_text(
    aes(
      x = name,
      y = 0 + if_else(ergebnis == "Gewinn", -0.01, 0.01),
      label = name,
      hjust = if_else(ergebnis == "Gewinn", 1, 0)
    ),
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = label_percent(
      big.mark = ".",
      decimal.mark = ","
    ),
    expand = expansion(c(0.15, 0.15))
  ) +
  labs(
    title = "Gewinne und Verluste",
    y = "Gewinn oder Verlust"
  ) +
  facet_wrap(
    ~ realisierung,
    scales = "free_y",
    labeller = as_labeller(
      c(
       "unrealisiert" = "Unrealisierte Gewinne",
       "realisiert" = "Realisierte Gewinne"
      )
    )
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    strip.background = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = "black", 
      fill = NA, 
      linewidth = 1
    )
  )


# Rebalancing -------------------------------------------------------------


# Outperformance ----------------------------------------------------------


# Infografik --------------------------------------------------------------

diagramme$infografik <-
  (diagramme$trades + plot_spacer()) / diagramme$performance +
  plot_annotation(
    title = "Portfolioanalyse",
    theme = theme(
      plot.title = element_text(hjust = 0.5)
    )
  )


# Dateien speichern -------------------------------------------------------

# Plots in einer Schleife speichern und ausführen
walk2(seq_along(diagramme), diagramme, function(nummer_diagramm, plot) {
  name_diagramm <- names(diagramme)[nummer_diagramm]
  name_datei <- str_glue("{nummer_diagramm}_{name_diagramm}.png")
  ggsave(
    filename = file.path(pfad_diagramme, name_datei),
    plot = plot,
    device = "png"
  )
  print(plot)
})
  