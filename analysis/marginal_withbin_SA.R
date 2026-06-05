library(interflex)
library(readxl)
library(dplyr)
library(ggplot2)
library(ragg)
library(patchwork)

d <- read_excel("long_df_SA.xlsx")
d <- as.data.frame(d)

d$outcome <- as.numeric(d$outcome)
d$respondent_id <- as.factor(d$respondent_id)

ATTRS <- c("region", "motivation", "education", "employer", "gender",
           "age", "occupation", "language", "politics", "appearance")

d <- d %>% mutate(across(all_of(ATTRS), as.factor))

REFS <- list(
  region = "Восточная Европа (Беларусь, Молдова, Украина)",
  motivation = "Поиск работы",
  education = "Среднее (школа)",
  employer = "Небольшая частная компания",
  gender = "Мужчина",
  occupation = "Сфера услуг (общепит)",
  language = "Говорит плохо",
  politics = "Страна помогает России обходить санкции (параллельный экспорт)",
  appearance = "В повседневной жизни носит обычную светскую одежду"
)

for (v in names(REFS)) {
  if (REFS[[v]] %in% levels(d[[v]])) d[[v]] <- relevel(d[[v]], ref = REFS[[v]])
}

d$age <- factor(d$age, levels = sort(unique(as.numeric(as.character(d$age)))))

INDEXES <- c("identity_civic_index", "identity_ethnic_index", "trust_index",
             "authoritar_index", "contact_index", "valence_index")
INDEXES <- INDEXES[INDEXES %in% names(d)]

for (v in INDEXES) {
  d[[v]] <- as.numeric(d[[v]])
  d[[paste0(v, "_c")]] <- d[[v]] - mean(d[[v]], na.rm = TRUE)
}

if ("Supporta" %in% names(d)) d$Supporta <- as.numeric(as.character(d$Supporta))

graphpath <- "Graphs/binning_hmx/"
dir.create(graphpath, recursive = TRUE, showWarnings = FALSE)

Y <- "outcome"
Ylabel <- "Эффект на вероятность выбора"

run_binning <- function(data, D, X, Z, Dlabel, Xlabel, ylim = c(-0.30, 0.30),
                        keep_axis_titles = FALSE) {
  dd <- na.omit(data[, c(Y, D, X, Z, "respondent_id")])
  x_lab <- if (keep_axis_titles) Xlabel else ""
  y_lab <- if (keep_axis_titles) Ylabel else ""
  out <- interflex(
    estimator = "binning",
    Y = Y, D = D, X = X, Z = Z, data = dd,
    Ylabel = y_lab, Xlabel = x_lab, Dlabel = Dlabel,
    main = "",
    ylim = ylim, cl = "respondent_id", vartype = "delta"
  )
  out$figure +
    labs(title = NULL, subtitle = NULL, x = x_lab, y = y_lab) +
    theme_bw(base_size = 8) +
    theme(
      plot.title = element_text(size = 7, face = "bold", hjust = 0.5,
                                lineheight = 0.85),
      plot.subtitle = element_blank(),
      axis.title.x = if (keep_axis_titles) element_text(size = 8) else element_blank(),
      axis.title.y = if (keep_axis_titles) element_text(size = 8) else element_blank(),
      axis.text = element_text(size = 5.5),
      legend.position = "none",
      plot.margin = margin(2, 2, 2, 2),
      panel.grid.minor = element_blank()
    )
}

save_png <- function(plot_object, file_name, width = 14, height = 9, dpi = 1200) {
  ggsave(filename = file.path(graphpath, paste0(file_name, ".png")),
         plot = plot_object, width = width, height = height, dpi = dpi,
         units = "in", bg = "white", device = ragg::agg_png)
}

base_region <- REFS$region
region_levels <- setdiff(levels(d$region), base_region)

for (i in seq_along(region_levels)) {
  d[[paste0("region_contrast_", i)]] <- ifelse(
    d$region == region_levels[i], 1,
    ifelse(d$region == base_region, 0, NA))
}

base_politics <- REFS$politics
politics_levels <- setdiff(levels(d$politics), base_politics)

for (i in seq_along(politics_levels)) {
  d[[paste0("politics_contrast_", i)]] <- ifelse(
    d$politics == politics_levels[i], 1,
    ifelse(d$politics == base_politics, 0, NA))
}

d$trad_clothes <- ifelse(
  d$appearance == "В повседневной жизни носит традиционную национальную одежду",
  1, 0)

short_region <- function(x) {
  x <- trimws(sub("\\s*\\(.*", "", x))
  x <- gsub("Восточная Азия", "Вост. Азия", x)
  x <- gsub("Западная Европа", "Зап. Европа", x)
  x <- gsub("Средняя Азия", "Ср. Азия", x)
  x <- gsub("Юго-Восточная Европа", "ЮВ Европа", x)
  x <- gsub("Южный Кавказ", "Юж. Кавказ", x)
  x
}

politics_short <- c(
  "Страна обычно голосует вместе с Россией в Генассамблее ООН" = "С Россией в ООН",
  "Страна обычно голосует вместе с США в Генассамблее ООН" = "С США в ООН",
  "Страна присоединилась к санкциям против России" = "Санкции против РФ"
)

pol_label <- function(x) {
  ifelse(x %in% names(politics_short),
         unname(politics_short[x]),
         trimws(sub("\\s*\\(.*", "", x)))
}

panel_theme <- theme(
  plot.title = element_text(size = 7, face = "bold", hjust = 0.5,
                            lineheight = 0.85),
  plot.subtitle = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text = element_text(size = 5.5),
  legend.position = "none",
  plot.margin = margin(2, 2, 2, 2),
  panel.grid.minor = element_blank()
)

annot_theme <- theme(
  plot.title = element_text(face = "bold", size = 11, lineheight = 0.9),
  plot.subtitle = element_text(colour = "grey30", size = 8, lineheight = 0.9)
)

# 1. Регион * этническая идентичность
plots_ethnic <- list()
for (i in seq_along(region_levels)) {
  plots_ethnic[[i]] <- run_binning(
    d, D = paste0("region_contrast_", i), X = "identity_ethnic_index_c",
    Z = c("identity_civic_index_c", "motivation", "education", "employer",
          "gender", "age", "occupation", "language", "politics", "appearance"),
    Dlabel = short_region(region_levels[i]),
    Xlabel = "Этническая идентичность",
    ylim = c(-0.30, 0.25))
}

p_identity_ethnic <- wrap_plots(plots_ethnic, ncol = 2) +
  plot_annotation(
    title = "Предельные эффекты региона при разных значениях этнической идентичности",
    subtitle = "X: этническая идентичность; Y: эффект на вероятность выбора. База: Восточная Европа, 95% ДИ",
    theme = annot_theme) & panel_theme

# 2. Регион * гражданская идентичность
plots_civic <- list()
for (i in seq_along(region_levels)) {
  plots_civic[[i]] <- run_binning(
    d, D = paste0("region_contrast_", i), X = "identity_civic_index_c",
    Z = c("identity_ethnic_index_c", "motivation", "education", "employer",
          "gender", "age", "occupation", "language", "politics", "appearance"),
    Dlabel = short_region(region_levels[i]),
    Xlabel = "Гражданская идентичность",
    ylim = c(-0.30, 0.25))
}

p_identity_civic <- wrap_plots(plots_civic, ncol = 2) +
  plot_annotation(
    title = "Предельные эффекты региона при разных значениях гражданской идентичности",
    subtitle = "X: гражданская идентичность; Y: эффект на вероятность выбора. База: Восточная Европа, 95% ДИ",
    theme = annot_theme) & panel_theme

# 3. Политика * доверие
plots_trust <- list()
for (i in seq_along(politics_levels)) {
  plots_trust[[i]] <- run_binning(
    d, D = paste0("politics_contrast_", i), X = "trust_index_c",
    Z = c("region", "motivation", "education", "employer", "gender",
          "age", "occupation", "language", "appearance"),
    Dlabel = pol_label(politics_levels[i]),
    Xlabel = "Доверие",
    ylim = c(-0.20, 0.20))
}

p_trust_c <- wrap_plots(plots_trust, ncol = 2) +
  plot_annotation(
    title = "Предельные эффекты политики страны при разных значениях доверия",
    subtitle = "X: доверие; Y: эффект на вероятность выбора. База: параллельный экспорт, 95% ДИ",
    theme = annot_theme) & panel_theme

# 4. Регион * авторитаризм
plots_author <- list()
for (i in seq_along(region_levels)) {
  plots_author[[i]] <- run_binning(
    d, D = paste0("region_contrast_", i), X = "authoritar_index_c",
    Z = c("motivation", "education", "employer", "gender", "age",
          "occupation", "language", "politics", "appearance"),
    Dlabel = short_region(region_levels[i]),
    Xlabel = "Авторитаризм",
    ylim = c(-0.30, 0.25))
}

p_author_c <- wrap_plots(plots_author, ncol = 2) +
  plot_annotation(
    title = "Предельные эффекты региона при разных значениях авторитарных установок",
    subtitle = "X: авторитаризм; Y: эффект на вероятность выбора. База: Восточная Европа, 95% ДИ",
    theme = annot_theme) & panel_theme

# 4b. Политика * авторитаризм (новая модерация)
plots_polauthor <- list()
for (i in seq_along(politics_levels)) {
  plots_polauthor[[i]] <- run_binning(
    d, D = paste0("politics_contrast_", i), X = "authoritar_index_c",
    Z = c("region", "motivation", "education", "employer", "gender",
          "age", "occupation", "language", "appearance"),
    Dlabel = pol_label(politics_levels[i]),
    Xlabel = "Авторитаризм",
    ylim = c(-0.25, 0.20))
}

p_polauthor <- wrap_plots(plots_polauthor, ncol = 2) +
  plot_annotation(
    title = "Предельные эффекты политики страны при разных значениях авторитарных установок",
    subtitle = "X: авторитаризм; Y: эффект на вероятность выбора. База: параллельный экспорт, 95% ДИ",
    theme = annot_theme) & panel_theme

# 5. Внешний вид * частота контакта
p_contact_c <- run_binning(
  d, D = "trad_clothes", X = "contact_index_c",
  Z = c("region", "motivation", "education", "employer", "gender",
        "age", "occupation", "language", "politics"),
  Dlabel = "Традиционная одежда", Xlabel = "Частота контакта",
  ylim = c(-0.18, 0.10), keep_axis_titles = TRUE) +
  ggtitle("Биннинг: одежда * частота контакта",
          subtitle = "Традиционная одежда vs светская, 95% ДИ") +
  theme(plot.title = element_text(face = "bold", size = 10.5, lineheight = 0.9),
        plot.subtitle = element_text(colour = "grey30", size = 8.5,
                                     lineheight = 0.9),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        legend.position = "none")

# 6. Внешний вид * валентность контакта
p_valence_c <- run_binning(
  d, D = "trad_clothes", X = "valence_index_c",
  Z = c("region", "motivation", "education", "employer", "gender",
        "age", "occupation", "language", "politics"),
  Dlabel = "Традиционная одежда", Xlabel = "Валентность контакта",
  ylim = c(-0.18, 0.10), keep_axis_titles = TRUE) +
  ggtitle("Биннинг: одежда * валентность контакта",
          subtitle = "Традиционная одежда vs светская, 95% ДИ") +
  theme(plot.title = element_text(face = "bold", size = 10.5, lineheight = 0.9),
        plot.subtitle = element_text(colour = "grey30", size = 8.5,
                                     lineheight = 0.9),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        legend.position = "none")

# 7. Политика * поддержка власти
plots_support <- list()
for (i in seq_along(politics_levels)) {
  plots_support[[i]] <- run_binning(
    d, D = paste0("politics_contrast_", i), X = "Supporta",
    Z = c("region", "motivation", "education", "employer", "gender",
          "age", "occupation", "language", "appearance"),
    Dlabel = pol_label(politics_levels[i]),
    Xlabel = "Поддержка власти",
    ylim = c(-0.20, 0.20))
}

p_support <- wrap_plots(plots_support, ncol = 2) +
  plot_annotation(
    title = "Предельные эффекты политики страны при разных значениях поддержки власти",
    subtitle = "X: поддержка власти; Y: эффект на вероятность выбора. База: параллельный экспорт, 95% ДИ",
    theme = annot_theme) & panel_theme

save_png(p_identity_ethnic, "01_identity_ethnic_region", width = 16, height = 13, dpi = 1200)
save_png(p_identity_civic, "02_identity_civic_region", width = 16, height = 13, dpi = 1200)
save_png(p_trust_c, "03_trust_politics", width = 14, height = 8, dpi = 1200)
save_png(p_author_c, "04_authoritarian_region", width = 16, height = 13, dpi = 1200)
save_png(p_polauthor, "04b_authoritarian_politics", width = 14, height = 8, dpi = 1200)
save_png(p_contact_c, "05_contact_appearance", width = 12, height = 7, dpi = 1200)
save_png(p_valence_c, "06_valence_appearance", width = 12, height = 7, dpi = 1200)
save_png(p_support, "07_support_politics", width = 14, height = 8, dpi = 1200)
