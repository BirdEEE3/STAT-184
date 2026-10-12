
library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)



sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

sales_clean <- sales_raw |>
  mutate(
    item_clean = str_squish(str_to_lower(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~"unrecorded",
      net_amount >= 20 ~ "large",
      .default = "small"
    )
  )

sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size) |>
  print()


sales_codes <- sales_clean |>
  mutate(
    code_clean= str_to_upper(str_trim(code_raw)),
    valid_code =str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

invalid_codes<- sales_codes|>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
print(invalid_codes)


valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  select(sale_id, code_clean) |>
  separate_wider_delim(code_clean, delim = "-",
                       names = c("channel", "number"),
                       cols_remove = FALSE) |>
  select(sale_id, code_clean, channel, number)
print(valid_code_parts)


sale_tags <- sales_clean |>
  select(sale_id, tags)|>
  separate_longer_delim(tags, delim = ";")
print(sale_tags, n = Inf)


# Explanation: separate_wider_delim() splits 1 string into multiple Columns, while separate_longer_delim() splits it into
# several rows, which is why 8 sales become 11 sale-tag rows.


customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),   # 0 when none known
    large_share = if_else(known_n > 0, large_n / known_n, NA_real_),
    .by = customer_id
  ) |>
  arrange(customer_id)
print(customer_summary)

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )

levels(sales_factors$item_factor)
count(sales_factors, item_factor)
levels(sales_factors$item_group)
count(sales_factors, item_group)

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_clean <- reports_labeled |>
  mutate( primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount) |>
  print()


# Explanation: A recorded primary value is the measurement we trust and  the backup is only a substitute for a mising primary.
# Zero is a real recorded value, not missing, so B-2 keeps 0 rather than 99,and A-3 keeps 20 rather than 25. B-1 has neither value, so it stays NA.

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3,
           fill = list(recorded_row = FALSE))
print(reports_complete, n = Inf)

tibble(
  stage= c("before", "after"),
  rows = c(nrow(reports_clean), nrow(reports_complete)),
  original_rows = c(nrow(reports_clean), sum(reports_complete$recorded_row)),
  added_rows = c(0, sum(!reports_complete$recorded_row)),
  missing_amt = c(sum(is.na(reports_clean$amount)),
                    sum(is.na(reports_complete$amount)))
)

customers |> count(customer_id) |> filter(n > 1)
sum(is.na(customers$customer_id))

reports_clean |> count(customer_id, round) |> filter(n > 1)


customers_marked <- customers |> mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(customers_marked,
            by = join_by(customer_id),
            relationship = "many-to-one",
            na_matches = "never") |>
  mutate(customer_found = coalesce(customer_found, FALSE)) 

reports_joined |>
  select(customer_id, round, amount, region, customer_found) |>
  print()

reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)


customers_bad <- tibble(
  customer_id = c("C1", "C2", "C2", "C3", "C5"),
  region = c("North", "South", "East", NA, "West")
)

reports_duplicated <- reports_clean |>
  left_join(customers_bad, by = join_by(customer_id),
            relationship = "many-to-many")

tibble(
  table = c("reports_clean", "reports_duplicated"),
  rows = c(nrow(reports_clean), nrow(reports_duplicated)),
  total_amount = c(sum(reports_clean$amount, na.rm = TRUE),
                   sum(reports_duplicated$amount, na.rm = TRUE))
)

reports_duplicated |>
  count(customer_id, round) |>
  filter(n > 1)

# Explanation: customers_bad has two rows for C2 and each report for C2 matches both, so C2's round-1 
# report is copied once per match. One extra row adds 20 to the total: 75 + 20 = 95.

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round, values_from = amount,
              names_prefix = "round_")
print(reports_wide)

reports_wide |>
  pivot_longer(starts_with("round_"), names_to = "round",
               names_prefix = "round_", values_to = "amount",
               values_drop_na = TRUE) |>
  nrow()

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  n_x <- length(x);n_y <- length(y)
  m_x <- mean(x); m_y <- mean(y)
  v_x <- var(x); v_y <- var(y)

  if (equal_var) {
    method <- "Pooled"
    s2_p <- ((n_x-1) * v_x + (n_y - 1) *v_y) / (n_x+n_y-2)
    se   <- sqrt(s2_p * (1/n_x + 1/n_y))
    df   <- n_x + n_y -2
  } else {
    method <- "Welch"
    a  <- v_x / n_x
    b  <- v_y / n_y
    se <- sqrt(a + b)
    df <- (a + b)^2 / (a^2 / (n_x - 1) + b^2 / (n_y - 1))
  }
  t_stat  <- (m_x - m_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  return(list(method= method,
              t_stat = t_stat,
              df = df,
              p_value= p_value,
              reject= p_value < alpha))
}

x_check <-c(12, 15, 14, 10, 13)
y_check <-c(9, 11, 8, 10, 7, 12, 9, 8)

my_welch  <-two_sample_t(x_check, y_check)
my_pooled <-two_sample_t(x_check, y_check, equal_var = TRUE)
str(my_welch)
str(my_pooled)

ref_welch <- t.test(x_check, y_check, var.equal = FALSE)
ref_pooled <- t.test(x_check, y_check, var.equal = TRUE)

verify <- function(mine, ref) {
  d <- c(t_stat= abs(mine$t_stat-unname(ref$statistic)),
         df = abs(mine$df- unname(ref$parameter)),
         p_value = abs(mine$p_value-ref$p.value))
  data.frame(quantity = names(d), abs_diff = d, below_1e_8 = d < 1e-8,
             row.names = NULL)
}
verify(my_welch, ref_welch)
verify(my_pooled, ref_pooled)


B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

set.seed(2026)
n_rows <- nrow(scenarios) * B * 2
scen_vec <- character(n_rows)
rep_vec <- integer(n_rows)
meth_vec <- character(n_rows)
t_vec <- numeric(n_rows)
df_vec <- numeric(n_rows)
p_vec <- numeric(n_rows)
rej_vec <- logical(n_rows)

row <- 0
for (s in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
    for (eq in c(FALSE, TRUE)) {
      res <- two_sample_t(x, y, equal_var = eq, alpha = alpha)
      row <- row + 1
      scen_vec[row] <- scenarios$scenario[s]
      rep_vec[row] <- b
      meth_vec[row] <- res$method
      t_vec[row] <- res$t_stat
      df_vec[row] <- res$df
      p_vec[row]<-res$p_value
      rej_vec[row]<- res$reject
    }
  }
}

simulation_results <- data.frame(
  scenario = scen_vec,
  replicate = rep_vec,
  method = meth_vec,
  t_stat = t_vec,
  df  = df_vec,
  p_value = p_vec,
  reject =rej_vec
)

head(simulation_results)
nrow(simulation_results)

simulation_summary <- simulation_results |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |> mutate(across(c(mean_t, sd_t, rejection_rate), \(v) round(v, 3)))
print(simulation_summary)

rate_mat <- xtabs(rejection_rate ~ method + scenario, data = simulation_summary)
barplot(rate_mat, beside = TRUE, legend.text = TRUE, ylim = c(0, 1),
        ylab = "Rejection rate", main = "Rejection rate by scenario and method",
        args.legend = list(x = "topleft", bty = "n"))
abline(h = alpha, lty = 2)

# Under equal variances both tests hold their size and Welch's 0.061 under unequal variances is also close,but the pooled test rejects it.
# When the means differ by one unit, power is about 0.96 for both tests with equal variances and drops for both with sd_x = 3.
# Overall, Welch is the safer default because it controls Type I error in both variance settings and loses nothing when variances are equal.