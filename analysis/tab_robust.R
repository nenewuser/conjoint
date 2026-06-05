library(readxl)
library(dplyr)
library(fixest)
library(tibble)
library(stringr)
library(purrr)
library(knitr)
library(tidyr)

d <- read_excel("long_df_SA.xlsx") |> as.data.frame()

d$outcome <- as.numeric(d$outcome)
d$respondent_id <- as.factor(d$respondent_id)

ATTRS <- c(
  "region", "motivation", "education", "employer", "gender",
  "age", "occupation", "language", "politics", "appearance"
)

d <- d |> mutate(across(all_of(ATTRS), as.factor))

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
  if (REFS[[v]] %in% levels(d[[v]])) {
    d[[v]] <- relevel(d[[v]], ref = REFS[[v]])
  }
}

age_numeric <- suppressWarnings(as.numeric(as.character(d$age)))
if (all(!is.na(age_numeric))) {
  d$age <- factor(d$age, levels = as.character(sort(unique(age_numeric))))
}

if ("candidate" %in% names(d)) d$candidate <- as.factor(d$candidate)
if ("task_num" %in% names(d)) d$task_num <- as.numeric(d$task_num)

ORDER_CONTROLS <- c("candidate", "task_num")
ORDER_CONTROLS <- ORDER_CONTROLS[ORDER_CONTROLS %in% names(d)]

SOC_COVARS <- c("Age_resp", "Female_resp", "Income", "Locality", "Education_resp")
SOC_COVARS <- SOC_COVARS[SOC_COVARS %in% names(d)]

for (v in SOC_COVARS) {
  if (v == "Age_resp") {
    d[[v]] <- as.numeric(d[[v]])
  } else {
    d[[v]] <- as.factor(d[[v]])
  }
}

POL_COVARS <- c(
  "identity_civic_index",
  "identity_ethnic_index",
  "trust_index",
  "authoritar_index",
  "contact_index",
  "valence_index",
  "Supporta"
)

POL_COVARS <- POL_COVARS[POL_COVARS %in% names(d)]

for (v in POL_COVARS) {
  d[[v]] <- as.numeric(d[[v]])
}

attr_formula <- paste(ATTRS, collapse = " + ")
order_formula <- paste(ORDER_CONTROLS, collapse = " + ")
soc_formula <- paste(SOC_COVARS, collapse = " + ")
pol_formula <- paste(POL_COVARS, collapse = " + ")

make_rhs <- function(include_soc = FALSE,
                     include_order = FALSE,
                     include_pol = FALSE) {
  parts <- c(attr_formula)
  
  if (include_soc && nchar(soc_formula) > 0) {
    parts <- c(parts, soc_formula)
  }
  
  if (include_order && nchar(order_formula) > 0) {
    parts <- c(parts, order_formula)
  }
  
  if (include_pol && nchar(pol_formula) > 0) {
    parts <- c(parts, pol_formula)
  }
  
  paste(parts, collapse = " + ")
}

m1_lpm <- feols(
  as.formula(paste("outcome ~", make_rhs(FALSE, FALSE, FALSE))),
  data = d,
  cluster = ~ respondent_id
)

m2_lpm_soc <- feols(
  as.formula(paste("outcome ~", make_rhs(TRUE, FALSE, FALSE))),
  data = d,
  cluster = ~ respondent_id
)

m3_lpm_order <- feols(
  as.formula(paste("outcome ~", make_rhs(FALSE, TRUE, FALSE))),
  data = d,
  cluster = ~ respondent_id
)

m4_lpm_full <- feols(
  as.formula(paste("outcome ~", make_rhs(TRUE, TRUE, TRUE))),
  data = d,
  cluster = ~ respondent_id
)

m5_probit <- feglm(
  as.formula(paste("outcome ~", make_rhs(FALSE, FALSE, FALSE))),
  data = d,
  family = binomial(link = "probit"),
  cluster = ~ respondent_id
)

m6_probit_soc <- feglm(
  as.formula(paste("outcome ~", make_rhs(TRUE, FALSE, FALSE))),
  data = d,
  family = binomial(link = "probit"),
  cluster = ~ respondent_id
)

m7_probit_order <- feglm(
  as.formula(paste("outcome ~", make_rhs(FALSE, TRUE, FALSE))),
  data = d,
  family = binomial(link = "probit"),
  cluster = ~ respondent_id
)

m8_probit_full <- feglm(
  as.formula(paste("outcome ~", make_rhs(TRUE, TRUE, TRUE))),
  data = d,
  family = binomial(link = "probit"),
  cluster = ~ respondent_id
)

models <- list(
  "LPM_базовая" = m1_lpm,
  "LPM_соц" = m2_lpm_soc,
  "LPM_дизайн" = m3_lpm_order,
  "LPM_полная" = m4_lpm_full,
  "Probit_базовая" = m5_probit,
  "Probit_соц" = m6_probit_soc,
  "Probit_дизайн" = m7_probit_order,
  "Probit_полная" = m8_probit_full
)

clean_coef_name <- function(x) {
  x <- str_replace(x, "^region", "")
  x <- str_replace(x, "^motivation", "")
  x <- str_replace(x, "^education", "")
  x <- str_replace(x, "^employer", "")
  x <- str_replace(x, "^gender", "")
  x <- str_replace(x, "^age", "Возраст: ")
  x <- str_replace(x, "^occupation", "")
  x <- str_replace(x, "^language", "")
  x <- str_replace(x, "^politics", "")
  x <- str_replace(x, "^appearance", "")
  x <- str_replace(x, "^::", "")
  
  x <- str_replace(x, "Восточная Азия \\(Китай, Монголия, Южная Корея\\)", "Восточная Азия")
  x <- str_replace(x, "Восточная Европа \\(Беларусь, Молдова, Украина\\)", "Восточная Европа")
  x <- str_replace(x, "Западная Европа \\(Германия, Италия, Франция\\)", "Западная Европа")
  x <- str_replace(x, "Средняя Азия \\(Кыргызстан, Таджикистан, Узбекистан\\)", "Средняя Азия")
  x <- str_replace(x, "Юго-Восточная Европа \\(Болгария, Венгрия, Сербия\\)", "Юго-Восточная Европа")
  x <- str_replace(x, "Южный Кавказ \\(Азербайджан, Армения, Грузия\\)", "Южный Кавказ")
  
  x <- str_replace(x, "Бегство от вооруженного конфликта", "Бегство от вооружённого конфликта")
  x <- str_replace(x, "Бегство от вооружённого конфликта", "Бегство от вооружённого конфликта")
  x <- str_replace(x, "Бегство от политических преследований", "Бегство от политических преследований")
  x <- str_replace(x, "Воссоединение с супругом\\(ой\\), ранее приехавшим\\(ей\\) в Россию", "Воссоединение с супругом(ой)")
  
  x <- str_replace(x, "Среднее \\(школа\\)", "Среднее образование")
  x <- str_replace(x, "Среднее специальное \\(колледж\\)", "Среднее специальное образование")
  x <- str_replace(x, "Высшее \\(университет\\)", "Высшее образование")
  x <- str_replace(x, "Высшее", "Высшее образование")
  
  x <- str_replace(x, "Сфера услуг \\(общепит\\)", "Сфера услуг")
  x <- str_replace(x, "Говорит свободно", "Свободно говорит по-русски")
  
  x <- str_replace(x, "Страна обычно голосует вместе с Россией в Генассамблее ООН", "Голосует с Россией в ГА ООН")
  x <- str_replace(x, "Страна обычно голосует вместе с США в Генассамблее ООН", "Голосует с США в ГА ООН")
  x <- str_replace(x, "Страна помогает России обходить санкции \\(параллельный экспорт\\)", "Параллельный экспорт")
  x <- str_replace(x, "Страна присоединилась к санкциям против России", "Санкции против России")
  
  x <- str_replace(x, "В повседневной жизни носит традиционную национальную одежду", "Традиционная национальная одежда")
  
  x
}

stars_fun <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.1 ~ "+",
    TRUE ~ ""
  )
}

all_coef_names <- unique(unlist(lapply(models, function(x) names(coef(x)))))
attr_pattern <- paste0("^(", paste(ATTRS, collapse = "|"), ")")
attr_coef_names <- all_coef_names[grepl(attr_pattern, all_coef_names)]

extract_model <- function(model, model_name) {
  est <- fixest::coeftable(model) |> as.data.frame()
  est$term_raw <- rownames(est)
  
  p_col <- grep("^Pr\\(", names(est), value = TRUE)[1]
  
  est |>
    filter(term_raw %in% attr_coef_names) |>
    transmute(
      `Тип модели` = case_when(
        str_detect(model_name, "^LPM") ~ "LPM",
        str_detect(model_name, "^Probit") ~ "Probit",
        TRUE ~ model_name
      ),
      `Спецификация` = case_when(
        str_detect(model_name, "базовая") ~ "Базовая",
        str_detect(model_name, "соц") ~ "Соц.-дем. контроль",
        str_detect(model_name, "дизайн") ~ "Дизайн-контроль",
        str_detect(model_name, "полная") ~ "Полная",
        TRUE ~ model_name
      ),
      `Атрибут / уровень` = clean_coef_name(term_raw),
      `Коэффициент` = paste0(sprintf("%.3f", Estimate), stars_fun(.data[[p_col]])),
      `Ст. ошибка` = paste0("(", sprintf("%.3f", `Std. Error`), ")")
    )
}

robust_long <- purrr::imap_dfr(models, extract_model) |>
  mutate(
    `Тип модели` = factor(`Тип модели`, levels = c("LPM", "Probit")),
    `Спецификация` = factor(
      `Спецификация`,
      levels = c("Базовая", "Соц.-дем. контроль", "Дизайн-контроль", "Полная")
    )
  ) |>
  arrange(`Тип модели`, `Спецификация`, `Атрибут / уровень`)

coef_map <- attr_coef_names
names(coef_map) <- attr_coef_names
coef_map <- setNames(clean_coef_name(coef_map), names(coef_map))

models_horizontal <- list(
  "LPM" = m1_lpm,
  "LPM + соц.-дем." = m2_lpm_soc,
  "LPM + дизайн" = m3_lpm_order,
  "LPM + полная" = m4_lpm_full,
  "Probit" = m5_probit,
  "Probit + соц.-дем." = m6_probit_soc,
  "Probit + дизайн" = m7_probit_order,
  "Probit + полная" = m8_probit_full
)

gof_map <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "N", 0,
  "r.squared", "R²", 3,
  "pseudo.r.squared", "Pseudo R²", 3
)

add_rows <- tibble::tribble(
  ~term,
  ~`LPM`,
  ~`LPM + соц.-дем.`,
  ~`LPM + дизайн`,
  ~`LPM + полная`,
  ~`Probit`,
  ~`Probit + соц.-дем.`,
  ~`Probit + дизайн`,
  ~`Probit + полная`,
  
  "Соц.-дем. ковариаты",
  "Нет", "Да", "Нет", "Да", "Нет", "Да", "Нет", "Да",
  
  "Позиция профиля и номер задания",
  "Нет", "Нет", "Да", "Да", "Нет", "Нет", "Да", "Да",
  
  "Установочные и контактные ковариаты",
  "Нет", "Нет", "Нет", "Да", "Нет", "Нет", "Нет", "Да"
)

dir.create("tables", showWarnings = FALSE, recursive = TRUE)

modelsummary(
  models_horizontal,
  coef_map = coef_map,
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  gof_map = gof_map,
  add_rows = add_rows,
  output = "tables/main_effects_horizontal.tex",
  title = "Проверка устойчивости основных эффектов",
  notes = NULL
)

modelsummary(
  models_horizontal,
  coef_map = coef_map,
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  gof_map = gof_map,
  add_rows = add_rows,
  output = "tables/main_effects_horizontal.html",
  title = "Проверка устойчивости основных эффектов",
  notes = NULL
)

POSITION_COL <- "candidate"

d <- d |>
  mutate(
    across(all_of(ATTRS), as.factor),
    candidate = as.factor(.data[[POSITION_COL]])
  )

balance_by_position <- map_dfr(ATTRS, function(attr) {
  tab <- table(d[[attr]], d[[POSITION_COL]])
  test <- chisq.test(tab)
  prop_tab <- prop.table(tab, margin = 2)
  
  max_diff <- if (ncol(prop_tab) == 2) {
    max(abs(prop_tab[, 1] - prop_tab[, 2]))
  } else {
    NA_real_
  }
  
  n <- sum(tab)
  r <- nrow(tab)
  k <- ncol(tab)
  cramers_v <- sqrt(as.numeric(test$statistic) / (n * min(r - 1, k - 1)))
  
  tibble(
    `Атрибут` = attr,
    `Chi-square` = round(as.numeric(test$statistic), 3),
    `df` = as.numeric(test$parameter),
    `p-value` = round(test$p.value, 4),
    `Макс. разница долей` = round(max_diff, 3),
    `Cramer's V` = round(cramers_v, 3)
  )
})

realized_distribution <- map_dfr(ATTRS, function(attr) {
  d |>
    count(.data[[attr]], name = "N") |>
    mutate(
      `Атрибут` = attr,
      `Уровень` = as.character(.data[[attr]]),
      `Доля` = round(N / sum(N), 3)
    ) |>
    select(`Атрибут`, `Уровень`, N, `Доля`)
})

write.csv(
  balance_by_position,
  "tables/balance_by_position.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  realized_distribution,
  "tables/realized_distribution_after_restrictions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

capture.output(
  kable(
    balance_by_position,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Проверка распределения уровней атрибутов между позициями профиля"
  ),
  file = "tables/balance_by_position.tex"
)

capture.output(
  kable(
    balance_by_position,
    format = "html",
    caption = "Проверка распределения уровней атрибутов между позициями профиля"
  ),
  file = "tables/balance_by_position.html"
)

capture.output(
  kable(
    realized_distribution,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Реализованное распределение уровней атрибутов после ограничений профайл-банка"
  ),
  file = "tables/realized_distribution_after_restrictions.tex"
)

capture.output(
  kable(
    realized_distribution,
    format = "html",
    caption = "Реализованное распределение уровней атрибутов после ограничений профайл-банка"
  ),
  file = "tables/realized_distribution_after_restrictions.html"
)

balance_by_position
realized_distribution

d <- d |>
  
  mutate(
    
    candidate = as.factor(candidate),
    
    task_num_c = as.numeric(task_num) - mean(as.numeric(task_num), na.rm = TRUE)
    
  )

order_attr_formula <- paste(ATTRS, collapse = " + ")

f_order_base <- as.formula(
  
  paste("outcome ~", order_attr_formula)
  
)

f_order_controls <- as.formula(
  
  paste("outcome ~", order_attr_formula, "+ candidate + task_num_c")
  
)

f_candidate_interactions <- as.formula(
  
  paste(
    
    "outcome ~",
    
    order_attr_formula,
    
    "+ candidate + task_num_c +",
    
    paste0("(", order_attr_formula, "):candidate")
    
  )
  
)

f_task_interactions <- as.formula(
  
  paste(
    
    "outcome ~",
    
    order_attr_formula,
    
    "+ candidate + task_num_c +",
    
    paste0("(", order_attr_formula, "):task_num_c")
    
  )
  
)

m_order_base <- feols(
  
  f_order_base,
  
  data = d,
  
  cluster = ~ respondent_id
  
)

m_order_controls <- feols(
  
  f_order_controls,
  
  data = d,
  
  cluster = ~ respondent_id
  
)

m_candidate_interactions <- feols(
  
  f_candidate_interactions,
  
  data = d,
  
  cluster = ~ respondent_id
  
)

m_task_interactions <- feols(
  
  f_task_interactions,
  
  data = d,
  
  cluster = ~ respondent_id
  
)

order_models_classic <- list(
  
  "Базовая" = m_order_base,
  
  "+ позиция/задание" = m_order_controls,
  
  "+ атрибут × позиция" = m_candidate_interactions,
  
  "+ атрибут × задание" = m_task_interactions
  
)

joint_test_fixest <- function(model, keep_fun) {
  
  terms <- names(coef(model))
  
  terms <- terms[keep_fun(terms)]
  
  
  
  if (length(terms) == 0) {
    
    return(c(stat = NA_real_, p = NA_real_))
    
  }
  
  
  
  b <- coef(model)[terms]
  
  V <- vcov(model)[terms, terms, drop = FALSE]
  
  q <- length(terms)
  
  
  
  stat <- as.numeric(t(b) %*% qr.solve(V) %*% b / q)
  
  p <- pf(stat, q, df.residual(model), lower.tail = FALSE)
  
  
  
  c(stat = stat, p = p)
  
}

jt_order <- joint_test_fixest(
  
  m_order_controls,
  
  function(x) grepl("candidate|task_num_c", x) & !grepl(":", x)
  
)

jt_candidate <- joint_test_fixest(
  
  m_candidate_interactions,
  
  function(x) grepl("candidate", x) & grepl(":", x)
  
)

jt_task <- joint_test_fixest(
  
  m_task_interactions,
  
  function(x) grepl("task_num_c", x) & grepl(":", x)
  
)

coef_map_order <- attr_coef_names
names(coef_map_order) <- attr_coef_names
coef_map_order <- setNames(clean_coef_name(coef_map_order), names(coef_map_order))

gof_map_order <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "N", 0,
  "r.squared", "R²", 3
)

dir.create("tables", showWarnings = FALSE, recursive = TRUE)

modelsummary(
  order_models_classic,
  coef_map = coef_map_order,
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  gof_map = gof_map_order,
  output = "tables/order_effects_full_no_ftests.tex",
  title = "Проверка устойчивости к позиции профиля и номеру задания",
  notes = NULL
)

modelsummary(
  order_models_classic,
  coef_map = coef_map_order,
  statistic = "({std.error})",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  gof_map = gof_map_order,
  output = "tables/order_effects_full_no_ftests.html",
  title = "Проверка устойчивости к позиции профиля и номеру задания",
  notes = NULL
)