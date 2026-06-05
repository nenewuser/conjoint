library(readxl)
library(dplyr)
library(ggplot2)
library(estimatr)
library(broom)
library(stringr)


# загрузка данных

d <- read_excel("long_df_SA.xlsx")

d$outcome <- as.numeric(d$outcome)
d$respondent_id <- as.factor(d$respondent_id)


# основные атрибуты профилей

profile_attrs <- c(
  "region", "motivation", "education", "employer", "gender",
  "age", "occupation", "language", "politics", "appearance"
)

group_labels <- c(
  region = "Регион",
  motivation = "Мотивация",
  education = "Образование",
  employer = "Работодатель",
  gender = "Пол",
  age = "Возраст",
  occupation = "Профессия",
  language = "Язык",
  politics = "Политика страны",
  appearance = "Внешний вид"
)

d <- d %>%
  mutate(across(all_of(profile_attrs), as.factor))


# базовые категории

refs <- list(
  region = "Восточная Европа (Беларусь, Молдова, Украина)",
  motivation = "Бегство от вооружённого конфликта",
  education = "Высшее (университет)",
  employer = "Небольшая частная компания",
  gender = "Мужчина",
  occupation = "Врач",
  language = "Говорит плохо",
  politics = "Страна помогает России обходить санкции (параллельный экспорт)",
  appearance = "В повседневной жизни носит обычную светскую одежду"
)

for (v in names(refs)) {
  if (refs[[v]] %in% levels(d[[v]])) {
    d[[v]] <- relevel(d[[v]], ref = refs[[v]])
  }
}

d$age <- factor(
  d$age,
  levels = sort(unique(as.numeric(as.character(d$age))))
)


# индексы модераторов

indexes <- c(
  "identity_civic_index",
  "identity_ethnic_index",
  "trust_index",
  "authoritar_index",
  "contact_index",
  "valence_index"
)

indexes <- indexes[indexes %in% names(d)]

for (v in indexes) {
  d[[v]] <- as.numeric(d[[v]])
  d[[paste0(v, "_c")]] <- d[[v]] - mean(d[[v]], na.rm = TRUE)
}


# ковариаты респондентов

if ("Supporta" %in% names(d)) {
  d$Supporta <- as.factor(d$Supporta)
}

if ("Age_resp" %in% names(d)) {
  d$Age_resp <- as.numeric(d$Age_resp)
}

resp_covars <- c("Income", "Locality", "Female_resp", "Education_resp")
resp_covars <- resp_covars[resp_covars %in% names(d)]

for (v in resp_covars) {
  d[[v]] <- as.factor(d[[v]])
}


# папка для графиков

dir.create("Plots", showWarnings = FALSE)


# оценка AMCE

estimate_amce <- function(data, attrs = profile_attrs, formula_extra = NULL) {
  
  rhs <- paste(attrs, collapse = " + ")
  
  if (!is.null(formula_extra)) {
    rhs <- paste(rhs, "+", formula_extra)
  }
  
  m <- lm_robust(
    as.formula(paste("outcome ~", rhs)),
    data = data,
    clusters = respondent_id,
    se_type = "stata"
  )
  
  est <- tidy(m) %>%
    filter(term != "(Intercept)")
  
  rows <- list()
  
  for (a in attrs) {
    
    lvls <- levels(data[[a]])
    
    rows[[length(rows) + 1]] <- data.frame(
      attr = a,
      level = lvls[1],
      pe = 0,
      se = NA,
      lower = NA,
      upper = NA,
      stringsAsFactors = FALSE
    )
    
    for (lvl in lvls[-1]) {
      
      one <- est %>%
        filter(term == paste0(a, lvl))
      
      if (nrow(one) == 1) {
        rows[[length(rows) + 1]] <- data.frame(
          attr = a,
          level = lvl,
          pe = one$estimate,
          se = one$std.error,
          lower = one$conf.low,
          upper = one$conf.high,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  bind_rows(rows) %>%
    mutate(group = factor(group_labels[attr], levels = group_labels[attrs]))
}


# сортировка уровней внутри панелей

sort_panel_top_to_bottom <- function(df_panel) {
  
  df_panel$level <- as.character(df_panel$level)
  
  is_base <- is.na(df_panel$lower)
  has_pe <- !is.na(df_panel$pe)
  
  base_rows <- df_panel[is_base, , drop = FALSE]
  other_rows <- df_panel[!is_base & has_pe, , drop = FALSE]
  
  if (nrow(base_rows) == 0 || nrow(other_rows) == 0) {
    return(df_panel)
  }
  
  neg_rows <- other_rows[other_rows$pe < 0, , drop = FALSE]
  pos_rows <- other_rows[other_rows$pe >= 0, , drop = FALSE]
  
  neg_rows <- neg_rows[order(neg_rows$pe), , drop = FALSE]
  pos_rows <- pos_rows[order(pos_rows$pe), , drop = FALSE]
  
  if (nrow(neg_rows) > 0 && nrow(pos_rows) > 0) {
    return(rbind(neg_rows, base_rows, pos_rows))
  }
  
  other_rows <- other_rows[order(other_rows$pe), , drop = FALSE]
  n_top <- floor(nrow(other_rows) / 2)
  
  top_rows <- other_rows[seq_len(n_top), , drop = FALSE]
  bottom_rows <- other_rows[seq_len(nrow(other_rows)) > n_top, , drop = FALSE]
  
  rbind(top_rows, base_rows, bottom_rows)
}


apply_row_order <- function(x, panel_var) {
  
  x[[panel_var]] <- as.character(x[[panel_var]])
  panels <- unique(x[[panel_var]])
  
  out <- list()
  
  for (g in panels) {
    tmp <- sort_panel_top_to_bottom(x[x[[panel_var]] == g, , drop = FALSE])
    tmp$level <- as.character(tmp$level)
    tmp$level_id <- paste(g, tmp$level, sep = "___")
    out[[g]] <- tmp
  }
  
  result <- bind_rows(out)
  result$level_id <- factor(result$level_id, levels = rev(unique(result$level_id)))
  
  result
}


wrap_y <- function(width = 30) {
  function(z) str_wrap(z, width = width)
}


# сокращения подписей уровней

shorten_politics <- function(labels) {
  
  dplyr::recode(
    labels,
    "Страна помогает России обходить санкции (параллельный экспорт)" =
      "Помогает обходить санкции",
    "Страна присоединилась к санкциям против России" =
      "Присоединилась к санкциям",
    "Страна обычно голосует вместе с США в Генассамблее ООН" =
      "Голосует вместе с США",
    "Страна обычно голосует вместе с Россией в Генассамблее ООН" =
      "Голосует вместе с Россией",
    .default = labels
  )
}


shorten_motivation <- function(labels) {
  
  dplyr::recode(
    labels,
    "Бегство от вооружённого конфликта" =
      "Вооружённый конфликт",
    "Бегство от политических преследований" =
      "Политические преследования",
    "Поиск работы" =
      "Поиск работы",
    "Воссоединение с супругом(ой), ранее приехавшим(ей) в Россию" =
      "Воссоединение с супругом",
    .default = labels
  )
}


clean_labels_main <- function(z, width = 30) {
  
  label <- sub("^.*___", "", z)
  group <- sub("___.*$", "", z)
  
  region_rows <- group == "Регион"
  label[region_rows] <- gsub("\\s*\\([^)]*\\)", "", label[region_rows])
  
  politics_rows <- group == "Политика страны"
  label[politics_rows] <- shorten_politics(label[politics_rows])
  
  motivation_rows <- group == "Мотивация"
  label[motivation_rows] <- shorten_motivation(label[motivation_rows])
  
  str_wrap(label, width = width)
}


clean_labels_vector <- function(labels, group_name = NULL, width = 30) {
  
  labels <- as.character(labels)
  
  if (!is.null(group_name) && group_name == "Регион") {
    labels <- gsub("\\s*\\([^)]*\\)", "", labels)
  }
  
  if (!is.null(group_name) && group_name == "Политика страны") {
    labels <- shorten_politics(labels)
  }
  
  if (!is.null(group_name) && group_name == "Мотивация") {
    labels <- shorten_motivation(labels)
  }
  
  str_wrap(labels, width = width)
}


# график AMCE в два столбца

plot_amce <- function(x, title = NULL,
                      xlim = c(-0.30, 0.30),
                      brk = seq(-0.30, 0.30, 0.10)) {
  
  group_levels <- levels(x$group)
  
  x$group_chr <- as.character(x$group)
  x <- apply_row_order(x, "group_chr")
  x$group <- factor(x$group_chr, levels = group_levels)
  
  ggplot(x, aes(pe, level_id)) +
    facet_wrap(~ group, scales = "free_y", ncol = 2) +
    geom_vline(xintercept = 0, color = "grey50") +
    geom_pointrange(
      data = filter(x, !is.na(lower)),
      aes(xmin = lower, xmax = upper),
      size = 0.5
    ) +
    geom_point(
      data = filter(x, is.na(lower)),
      size = 2
    ) +
    coord_cartesian(xlim = xlim) +
    scale_x_continuous(
      "Эффект на Pr(выбран)",
      breaks = brk,
      labels = function(z) sprintf("%.2f", z)
    ) +
    scale_y_discrete(
      NULL,
      labels = function(z) clean_labels_main(z, width = 30)
    ) +
    labs(title = title) +
    theme_bw(13) +
    theme(
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_rect(fill = "grey95"),
      axis.text.y = element_text(size = 13, lineheight = 0.95),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 13),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(1.8, "lines"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
}


# marginal means

estimate_marginal_means <- function(data, attrs = profile_attrs) {
  
  rhs <- paste(attrs, collapse = " + ")
  
  m <- lm_robust(
    as.formula(paste("outcome ~", rhs)),
    data = data,
    clusters = respondent_id,
    se_type = "stata"
  )
  
  rows <- list()
  
  for (a in attrs) {
    
    lvls <- levels(data[[a]])
    
    for (lvl in lvls) {
      
      nd <- data
      nd[[a]] <- factor(lvl, levels = levels(data[[a]]))
      
      x_mat <- model.matrix(formula(m), data = nd)
      beta <- coef(m)
      vcv <- vcov(m)
      
      x_mat <- x_mat[, names(beta), drop = FALSE]
      
      pred_i <- as.numeric(x_mat %*% beta)
      pe <- mean(pred_i, na.rm = TRUE)
      
      xbar <- colMeans(x_mat, na.rm = TRUE)
      se <- sqrt(as.numeric(t(xbar) %*% vcv %*% xbar))
      
      rows[[length(rows) + 1]] <- data.frame(
        attr = a,
        level = lvl,
        pe = pe,
        se = se,
        lower = pe - 1.96 * se,
        upper = pe + 1.96 * se,
        stringsAsFactors = FALSE
      )
    }
  }
  
  bind_rows(rows) %>%
    mutate(group = factor(group_labels[attr], levels = group_labels[attrs]))
}


sort_mm_panel <- function(df_panel) {
  df_panel$level <- as.character(df_panel$level)
  df_panel[order(df_panel$pe), , drop = FALSE]
}


apply_mm_order <- function(x, panel_var) {
  
  x[[panel_var]] <- as.character(x[[panel_var]])
  panels <- unique(x[[panel_var]])
  
  out <- list()
  
  for (g in panels) {
    tmp <- sort_mm_panel(x[x[[panel_var]] == g, , drop = FALSE])
    tmp$level <- as.character(tmp$level)
    tmp$level_id <- paste(g, tmp$level, sep = "___")
    out[[g]] <- tmp
  }
  
  result <- bind_rows(out)
  result$level_id <- factor(result$level_id, levels = rev(unique(result$level_id)))
  
  result
}


# график marginal means в два столбца

plot_marginal_means <- function(x, title = NULL,
                                xlim = c(0.35, 0.65),
                                brk = seq(0.35, 0.65, 0.05)) {
  
  group_levels <- levels(x$group)
  
  x$group_chr <- as.character(x$group)
  x <- apply_mm_order(x, "group_chr")
  x$group <- factor(x$group_chr, levels = group_levels)
  
  ggplot(x, aes(pe, level_id)) +
    facet_wrap(~ group, scales = "free_y", ncol = 2) +
    geom_vline(xintercept = 0.5, color = "grey50", linetype = "dashed") +
    geom_pointrange(
      aes(xmin = lower, xmax = upper),
      size = 0.5
    ) +
    coord_cartesian(xlim = xlim) +
    scale_x_continuous(
      "Предсказанная вероятность выбора",
      breaks = brk,
      labels = function(z) sprintf("%.2f", z)
    ) +
    scale_y_discrete(
      NULL,
      labels = function(z) clean_labels_main(z, width = 30)
    ) +
    labs(title = title) +
    theme_bw(13) +
    theme(
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_rect(fill = "grey95"),
      axis.text.y = element_text(size = 13, lineheight = 0.95),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 13),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(1.8, "lines"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
}


# условные AMCE для непрерывных модераторов

conditional_amce <- function(data, mod_var, focal_attr,
                             attrs_ctrl = setdiff(profile_attrs, focal_attr),
                             points = c(-1, 0, 1)) {
  
  s <- sd(data[[mod_var]], na.rm = TRUE)
  vals <- points * s
  
  lbls <- ifelse(
    points == 0,
    "среднее",
    paste0(ifelse(points > 0, "+", "−"), abs(points), "σ")
  )
  
  rhs <- paste0(
    focal_attr, " * ", mod_var, " + ",
    paste(attrs_ctrl, collapse = " + ")
  )
  
  m <- lm_robust(
    as.formula(paste("outcome ~", rhs)),
    data = data,
    clusters = respondent_id,
    se_type = "stata"
  )
  
  cf <- coef(m)
  vcv <- vcov(m)
  
  out <- list()
  
  for (i in seq_along(vals)) {
    
    v <- vals[i]
    lbl <- lbls[i]
    rows <- list()
    
    lvls <- levels(data[[focal_attr]])
    
    rows[[length(rows) + 1]] <- data.frame(
      level = lvls[1],
      pe = 0,
      se = NA,
      lower = NA,
      upper = NA,
      mod_value = lbl,
      sd_step = points[i],
      stringsAsFactors = FALSE
    )
    
    for (lvl in lvls[-1]) {
      
      b <- paste0(focal_attr, lvl)
      bx <- paste0(b, ":", mod_var)
      
      if (!(b %in% names(cf))) next
      
      if (bx %in% names(cf)) {
        est <- cf[b] + cf[bx] * v
        var <- vcv[b, b] + v^2 * vcv[bx, bx] + 2 * v * vcv[b, bx]
      } else {
        est <- cf[b]
        var <- vcv[b, b]
      }
      
      se <- sqrt(var)
      
      rows[[length(rows) + 1]] <- data.frame(
        level = lvl,
        pe = unname(est),
        se = unname(se),
        lower = unname(est - 1.96 * se),
        upper = unname(est + 1.96 * se),
        mod_value = lbl,
        sd_step = points[i],
        stringsAsFactors = FALSE
      )
    }
    
    out[[i]] <- bind_rows(rows)
  }
  
  bind_rows(out) %>%
    mutate(mod_value = factor(mod_value, levels = lbls))
}


plot_conditional_amce <- function(x, title = NULL, mod_label = NULL,
                                  focal_attr = NULL) {
  
  steps <- sort(unique(x$sd_step))
  middle_step <- if (any(x$sd_step == 0)) 0 else steps[ceiling(length(steps) / 2)]
  
  middle_panel <- x[x$sd_step == middle_step, , drop = FALSE]
  ord <- sort_panel_top_to_bottom(middle_panel)
  
  order_levels <- as.character(ord$level)
  
  x$level <- as.character(x$level)
  x$y_pos <- match(x$level, rev(order_levels))
  
  lab_df <- data.frame(
    y_pos = seq_along(rev(order_levels)),
    level = rev(order_levels)
  )
  
  group_name <- if (!is.null(focal_attr)) group_labels[[focal_attr]] else NULL
  
  ggplot(x, aes(pe, y_pos)) +
    facet_wrap(~ mod_value, ncol = 3) +
    geom_vline(xintercept = 0, color = "grey50") +
    geom_pointrange(
      data = filter(x, !is.na(lower)),
      aes(xmin = lower, xmax = upper),
      size = 0.5
    ) +
    geom_point(
      data = filter(x, is.na(lower)),
      size = 2
    ) +
    scale_x_continuous(
      "Эффект на Pr(выбран)",
      labels = function(z) sprintf("%.2f", z),
      n.breaks = 5
    ) +
    scale_y_continuous(
      NULL,
      breaks = lab_df$y_pos,
      labels = clean_labels_vector(lab_df$level, group_name = group_name, width = 30),
      expand = expansion(add = 0.4)
    ) +
    labs(title = title, subtitle = mod_label) +
    theme_bw(13) +
    theme(
      strip.text = element_text(size = 13, face = "bold"),
      strip.background = element_rect(fill = "grey95"),
      axis.text.y = element_text(size = 13, lineheight = 0.95),
      axis.text.x = element_text(size = 11),
      axis.title.x = element_text(size = 13),
      panel.spacing.x = unit(1.3, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      plot.subtitle = element_text(
        size = 12,
        color = "grey20",
        margin = margin(b = 6)
      )
    )
}


# AMCE по подвыборкам

amce_by_subset <- function(data, focal_attr, subset_var,
                           attrs_ctrl = setdiff(profile_attrs, focal_attr)) {
  
  v <- data[[subset_var]]
  
  lvls <- if (is.factor(v)) levels(v) else sort(unique(v))
  rhs <- paste0(focal_attr, " + ", paste(attrs_ctrl, collapse = " + "))
  
  out <- list()
  
  for (lvl in lvls) {
    
    sub <- data[!is.na(v) & v == lvl, ]
    
    if (nrow(sub) == 0) next
    
    n_resp <- length(unique(sub$respondent_id))
    
    m <- tryCatch(
      lm_robust(
        as.formula(paste("outcome ~", rhs)),
        data = sub,
        clusters = respondent_id,
        se_type = "stata"
      ),
      error = function(e) NULL
    )
    
    if (is.null(m)) next
    
    e <- tidy(m)
    rows <- list()
    
    fa_levels <- levels(data[[focal_attr]])
    
    rows[[length(rows) + 1]] <- data.frame(
      level = fa_levels[1],
      pe = 0,
      se = NA,
      lower = NA,
      upper = NA,
      subset_value = as.character(lvl),
      n_resp_subset = n_resp,
      stringsAsFactors = FALSE
    )
    
    for (l in fa_levels[-1]) {
      
      one <- e %>%
        filter(term == paste0(focal_attr, l))
      
      if (nrow(one) == 1) {
        rows[[length(rows) + 1]] <- data.frame(
          level = l,
          pe = one$estimate,
          se = one$std.error,
          lower = one$conf.low,
          upper = one$conf.high,
          subset_value = as.character(lvl),
          n_resp_subset = n_resp,
          stringsAsFactors = FALSE
        )
      }
    }
    
    out[[as.character(lvl)]] <- bind_rows(rows)
  }
  
  bind_rows(out) %>%
    mutate(subset_value = factor(subset_value, levels = as.character(lvls)))
}


plot_subset_amce <- function(x, title = NULL, subset_label = NULL,
                             focal_attr = NULL) {
  
  first_subset <- levels(x$subset_value)[1]
  base_panel <- x[x$subset_value == first_subset, , drop = FALSE]
  
  ord <- sort_panel_top_to_bottom(base_panel)
  level_order <- rev(unique(as.character(ord$level)))
  
  x$level <- factor(as.character(x$level), levels = level_order)
  
  n_per <- x %>%
    distinct(subset_value, n_resp_subset) %>%
    { setNames(.$n_resp_subset, as.character(.$subset_value)) }
  
  facet_labels <- function(values) {
    sapply(
      as.character(values),
      function(v) sprintf("%s\n(N = %d)", v, n_per[v])
    )
  }
  
  group_name <- if (!is.null(focal_attr)) group_labels[[focal_attr]] else NULL
  
  ggplot(x, aes(pe, level)) +
    facet_wrap(
      ~ subset_value,
      nrow = 1,
      labeller = labeller(subset_value = facet_labels)
    ) +
    geom_vline(xintercept = 0, color = "grey50") +
    geom_pointrange(
      data = filter(x, !is.na(lower)),
      aes(xmin = lower, xmax = upper),
      size = 0.5
    ) +
    geom_point(
      data = filter(x, is.na(lower)),
      size = 2
    ) +
    scale_x_continuous(
      "Эффект на Pr(выбран)",
      labels = function(z) sprintf("%.2f", z),
      n.breaks = 4
    ) +
    scale_y_discrete(
      NULL,
      labels = function(z) clean_labels_vector(z, group_name = group_name, width = 30)
    ) +
    labs(title = title, subtitle = subset_label) +
    theme_bw(13) +
    theme(
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(size = 13, face = "bold", lineheight = 1.0),
      axis.text.y = element_text(size = 13, lineheight = 0.95),
      axis.text.x = element_text(size = 11),
      axis.title.x = element_text(size = 13),
      panel.spacing.x = unit(1.4, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      plot.subtitle = element_text(
        size = 12,
        color = "grey20",
        margin = margin(b = 6)
      )
    )
}


# сохранение графиков

save_plot <- function(name, p, w = 10, h = 14) {
  
  ggsave(
    paste0("Plots/", name, ".png"),
    p,
    width = w,
    height = h,
    dpi = 300
  )
  
  try(
    ggsave(
      paste0("Plots/", name, ".pdf"),
      p,
      width = w,
      height = h,
      device = cairo_pdf
    ),
    silent = TRUE
  )
}


# главные эффекты

amce_main <- estimate_amce(d)

save_plot(
  "amce_main",
  plot_amce(amce_main, "AMCE: главные эффекты"),
  w = 19,
  h = 12.5
)


# marginal means

mm_main <- estimate_marginal_means(d)

save_plot(
  "marginal_means_main",
  plot_marginal_means(mm_main, "Marginal means: предсказанные вероятности выбора"),
  w = 19,
  h = 12.5
)


# гипотезы с непрерывными модераторами

cont_hyps <- list(
  list(
    name = "region_x_ethnic_identity",
    focal = "region",
    mod = "identity_ethnic_index_c",
    label = "Регион * этническая идентичность",
    mod_label = "Этническая идентичность респондента",
    points = c(-1, 0, 1)
  ),
  list(
    name = "region_x_civic_identity",
    focal = "region",
    mod = "identity_civic_index_c",
    label = "Регион * гражданская идентичность",
    mod_label = "Гражданская идентичность респондента",
    points = c(-1, 0, 1)
  ),
  list(
    name = "politics_x_trust",
    focal = "politics",
    mod = "trust_index_c",
    label = "Политика страны * институциональное доверие",
    mod_label = "Институциональное доверие респондента",
    points = c(-2, 0, 2)
  ),
  list(
    name = "region_x_authoritarianism",
    focal = "region",
    mod = "authoritar_index_c",
    label = "Регион * авторитаризм",
    mod_label = "Авторитаризм респондента",
    points = c(-2, 0, 2)
  ),
  list(
    name = "appearance_x_contact",
    focal = "appearance",
    mod = "contact_index_c",
    label = "Внешний вид * частота контакта",
    mod_label = "Частота контакта респондента с мигрантами",
    points = c(-1, 0, 1)
  ),
  list(
    name = "appearance_x_valence",
    focal = "appearance",
    mod = "valence_index_c",
    label = "Внешний вид * опыт контакта",
    mod_label = "Опыт контакта респондента с мигрантами",
    points = c(-1, 0, 1)
  )
)

plot_heights <- c(
  region = 7.5,
  politics = 6,
  appearance = 5
)

for (h in cont_hyps) {
  
  if (!(h$mod %in% names(d))) next
  
  ca <- conditional_amce(
    d,
    mod_var = h$mod,
    focal_attr = h$focal,
    points = h$points
  )
  
  save_plot(
    h$name,
    plot_conditional_amce(
      ca,
      h$label,
      mod_label = h$mod_label,
      focal_attr = h$focal
    ),
    w = 13,
    h = plot_heights[[h$focal]]
  )
}


# политика страны по поддержке власти

if ("Supporta" %in% names(d)) {
  
  s_sup <- amce_by_subset(d, "politics", "Supporta")
  
  save_plot(
    "politics_x_support",
    plot_subset_amce(
      s_sup,
      title = "Политика страны * поддержка действий власти",
      subset_label = "Уровень поддержки действий власти",
      focal_attr = "politics"
    ),
    w = 14,
    h = 6.5
  )
}


# образование мигранта по образованию респондента

if ("Education_resp" %in% names(d)) {
  
  s_edu <- amce_by_subset(d, "education", "Education_resp")
  
  save_plot(
    "education_x_education_resp",
    plot_subset_amce(
      s_edu,
      title = "Образование мигранта * образование респондента",
      subset_label = "Уровень образования респондента",
      focal_attr = "education"
    ),
    w = 16,
    h = 6.5
  )
}