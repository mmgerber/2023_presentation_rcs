# Quantities
att <- .333
n_w1 <- 1500
n_w2 <- floor(n_w1 * (1 - att))
n_w3 <- floor(n_w2 * (1 - att))
es_ew2 <- .8
es_eb23 <- .4
set.seed(185)

w1 <- data.frame(
  y  = rnorm(n_w1, mean = 6, sd = 1.5),
  id = 1:n_w1,
  d  = sample(60:(60 + 35), n_w1, replace = T),
  w  = "A"
)

w2 <- data.frame(
  y  = rnorm(n_w2, mean = 6, sd = 1.5),
  id = sample(w1$id, n_w2),
  d  = sample(152:(152 + 35), n_w2, replace = T),
  w  = "B"
)

w3 <- data.frame(
  y  = rnorm(n_w3, mean = 6, sd = 1.5),
  id = sample(w2$id, n_w3),
  d  = sample(247:(247 + 35), n_w3, replace = T),
  w  = "C"
)

# Join waves
esep <- rbind(w1, w2, w3)

# Visualize
library(ggplot2)

esep |> 
  ggplot(aes(x = d, y = y)) +
  geom_point(alpha = 0.15) +
  coord_cartesian(xlim = c(1, 365), ylim = c(1, 10)) +
  scale_x_continuous(breaks = c(1, seq(10, 360, 20), 365),
                     expand = c(0.005, 0.05)) +
  scale_y_continuous(n.breaks = 10) +
  theme_bw()

# Simulate event within wave B and between wave B & C
library(dplyr)
esep <- esep |> 
  mutate(
    e_ew2 = ifelse(w == "B", rnorm(n_w2, 0, .5), 0),
    e_eb23 = rnorm(n(), 0, .6),
    d_ew2 = ifelse(d >= 165, 1, 0),
    d_eb23 = ifelse(d >= 210, 1, 0),
    y_ew2 = case_when(
      w == "B" & d_ew2 == 1 ~ y + es_ew2 * d_ew2 + e_ew2,
      T ~ y),
    Y = y_ew2,
    E1 = factor(d_ew2), # Event within wave B
    E2 = factor(d_eb23), # Event between waves B & C
    y_eb23 = y + es_eb23 * d_eb23 + e_eb23,
    ytot = y + es_eb23 * d_eb23 + e_eb23 + es_ew2 * d_ew2 + e_ew2)
    # ytot   = case_when(
    #   d_ew2 == 1 ~ y_ew2,
    #   T ~ y_eb23))
    # y_eb23 = case_when(
    #   d_eb23 == 1 ~ y + es_eb23 * d_eb23 + e_eb23,
    #   T ~ y))

# Plots
(p_ew2 <- esep |> 
  ggplot(aes(x = d, y = y_ew2)) +
  geom_point(alpha = 0.15) +
  coord_cartesian(xlim = c(1, 365), ylim = c(1, 10)) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(breaks = c(1, seq(10, 360, 20), 365),
                     expand = c(0.005, 0.05)) +
  geom_vline(xintercept = 165, color = "darkred", linetype = "dashed") +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Day of year", y = "Y") +
  annotate("text", x = (60 + 35 / 2),  y = 9.5, label = "Wave A") +
  annotate("text", x = (152 + 35 / 2), y = 9.5, label = "Wave B") +
  annotate("text", x = (247 + 35 / 2), y = 9.5, label = "Wave C")
  )

(p_eb23 <- esep |> 
    ggplot(aes(x = d, y = y_eb23)) +
    geom_point(alpha = 0.15) +
    coord_cartesian(xlim = c(1, 365), ylim = c(1, 10)) +
    scale_y_continuous(n.breaks = 10) +
    scale_x_continuous(breaks = c(1, seq(10, 360, 20), 365),
                       expand = c(0.005, 0.05)) +
    geom_vline(xintercept = 210, color = "darkred", linetype = "dashed") +
    theme_bw(base_size = 18) +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Day of year", y = "Y") +
    annotate("text", x = (60 + 35 / 2),  y = 9.5, label = "Wave A") +
    annotate("text", x = (152 + 35 / 2), y = 9.5, label = "Wave B") +
    annotate("text", x = (247 + 35 / 2), y = 9.5, label = "Wave C")
)


# Models
library(fixest)
library(lme4)

# Effects with restricted samples (only wave B; and only waves B & C)
m_ewb <- lm(Y ~ E1, data = filter(esep, w == "B"))
m_ebbc <- lm(y_eb23 ~ E2 + id, data = filter(esep, w %in% c("B", "C")))
summary(m_ebbc)

# Effect within wave B
m_ewb_fe <- feols(Y ~ E1 + w | id, data = esep)
m_ewb <- lm(Y ~ E1, data = filter(esep, w == "B"))
m_ewb_ri <- lmer(Y ~ (1 | id) + E1 + w, data = esep)

sjPlot::tab_model(m_ewb, m_ewb_fe, m_ewb_ri, show.se = T, show.ci = F)

# Effect between waves B & C
m_ebbc_fe <- feols(y_eb23 ~ E2 + w | id, data = esep)
m_ebbc <- lm(y_eb23 ~ E2 + w + id, data = esep)
m_ebbc_ri <- lmer(y_eb23 ~ (1 | id) + E2 + w, data = esep)
summary(m_ebbc)
sjPlot::tab_model(m_ebbc, m_ebbc_fe, m_ebbc_ri, show.se = T, show.ci = F)

# Both effects
m_tot_fe <- feols(ytot ~ E1 + E2 + w | id, data = esep)
m_tot <- lm(ytot ~ E1 + E2 + w + id, data = esep)
m_tot_ri <- lmer(ytot ~ (1 | id) + E1 + E2 + w, data = esep)
sjPlot::tab_model(m_tot_fe, m_tot_fe, m_tot_ri, show.se = T, show.ci = F)

feols(ytot ~ E1 + w | id, data = esep)
feols(ytot ~ E2 + w | id, data = esep)

# Equations
library(equatiomatic)
library(latex2exp)

# Equations
# extract equation with `ital_vars = TRUE` to avoid the use of `\operatorname`
eq_ewb <- extract_eq(m_ewb, use_coef = F, ital_vars = T)
eq_ebbc <- extract_eq(m_ebbc, use_coef = F, ital_vars = T)
eq_tot <- extract_eq(m_tot, use_coef = F, ital_vars = T)

# swap escaped underscores for dashes
eq_ewb <- gsub("\\\\_", "", eq_ewb)
eq_ewb <- gsub("E1\\_\\{1\\}", "E1", eq_ewb)
eq_ebbc <- gsub("\\\\_", "", eq_ebbc)
eq_ebbc <- gsub("yeb23", "Y", eq_ebbc)
eq_ebbc <- gsub("E2\\_\\{1\\}", "E2", eq_ebbc)
eq_tot <- gsub("\\\\_", "", eq_tot)
eq_tot <- gsub("ytot", "Y", eq_tot)
eq_tot <- gsub("E1\\_\\{1\\}", "E1", eq_tot)
eq_tot <- gsub("E2\\_\\{1\\}", "E2", eq_tot)


# swap display-style $$ with inline-style $
eq_ewb <- paste("$", as.character(eq_ewb), "$", sep = "")
eq_ebbc <- paste("$", as.character(eq_ebbc), "$", sep = "")
eq_tot <- paste("$", as.character(eq_tot), "$", sep = "")

# Plot effect within wave B
(p_ew2 <- esep |> 
    filter(w == "B", y_ew2 <= 10, y_ew2 >= 1 ) |> 
    ggplot(aes(x = d, y = y_ew2)) +
    geom_point(alpha = 0.15) +
    coord_cartesian(xlim = c(1, 365), ylim = c(1, 10)) +
    scale_y_continuous(n.breaks = 10) +
    scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 
                                  305, 335), labels = month.abb[1:12],
                       expand = c(0.005, 0.05)) +
    geom_vline(xintercept = 165, color = "darkred", linetype = "dashed", 
               linewidth = 1.2) +
    theme_bw(base_size = 18) +
    theme(panel.grid = element_blank(),
          axis.title.x = element_blank()) +
    labs(y = "Y") +
    #  annotate("text", x = (60 + 35 / 2),  y = 9.5, label = "Wave A") +
    annotate("text", x = (152 + 35 / 2), y = 10.2, label = "Wave B",  size = 6) +
  #  annotate("text", x = (247 + 35 / 2), y = 9.5, label = "Wave C") +
  annotate("text", x = 235, y = 9, label = TeX(eq_ewb), color = "darkred", 
           size = 7)
)

# Plot effect between waves B & C
(p_eb23 <- esep |> 
    filter(w %in% c("B", "C"), y_eb23 <= 10, y_eb23 >= 1 ) |> 
    ggplot(aes(x = d, y = y_eb23)) +
    geom_point(alpha = 0.15) +
    coord_cartesian(xlim = c(1, 365), ylim = c(1, 10)) +
    scale_y_continuous(n.breaks = 10) +
    scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 
                                  305, 335), labels = month.abb[1:12],
                       expand = c(0.005, 0.05)) +
    geom_vline(xintercept = 210, color = "darkblue", linetype = "dashed",
               linewidth = 1.2) +
    theme_bw(base_size = 18) +
    theme(panel.grid = element_blank(),
          axis.title.x = element_blank()) +
    labs(y = "Y") +
  # annotate("text", x = (60 + 35 / 2),  y = 9.5, label = "Wave A") +
    annotate("text", x = (152 + 35 / 2), y = 10.2, label = "Wave B", size = 6) +
    annotate("text", x = (247 + 35 / 2), y = 10.2, label = "Wave C", size = 6) +
    annotate("text", x = 255, y = 2, label = TeX(eq_ebbc), color = "darkblue", 
             size =  7)
)

# Plot both effects
(p_tot <- esep |> 
    filter(ytot <= 10, ytot >= 1 ) |> 
    ggplot(aes(x = d, y = ytot)) +
    geom_point(alpha = 0.15) +
    coord_cartesian(xlim = c(1, 365), ylim = c(1, 10)) +
    scale_y_continuous(n.breaks = 10) +
    scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 
                                  305, 335), labels = month.abb[1:12],
                       expand = c(0.005, 0.05)) +
    geom_vline(xintercept = 165, color = "darkred", linetype = "dashed", 
               linewidth = 1.2) +
    geom_vline(xintercept = 210, color = "darkblue", linetype = "dashed",
               linewidth = 1.2) +
    theme_bw(base_size = 18) +
    theme(panel.grid = element_blank(),
          axis.title.x = element_blank()) +
    labs(y = "Y")
  )

(p_tot <- p_tot + 
  annotate("text", x = 238, y = 2, label = TeX(eq_tot), color = "black", 
           size = 7) +
#  annotate("text", x = 235, y = 2, label = TeX(eq_ebbc), color = "darkblue", size = 5) +
  annotate("text", x = (60 + 35 / 2),  y = 10.2, label = "Wave A", size = 6) +
  annotate("text", x = (152 + 35 / 2), y = 10.2, label = "Wave B", size = 6) +
  annotate("text", x = (247 + 35 / 2), y = 10.2, label = "Wave C", size = 6)
)


# Save visualizations ----
# Save as png
ggsave(filename = here::here("output", "images", "f_w_wave_b.png"), plot = p_ew2,
       dpi = 600, width = 320, height = 180, units = "mm")

# Save as png
ggsave(filename = here::here("output", "images", "f_b_waves_bc.png"), plot = p_eb23,
       dpi = 600, width = 320, height = 180, units = "mm")

# Save as png
ggsave(filename = here::here("output", "images", "f_ewb.png"), plot = p_tot,
       dpi = 600, width = 320, height = 180, units = "mm")

# Save as jpg
ggsave(filename = here::here("output", "images", "f_w_wave_b.jpg"), plot = p_ew2,
       dpi = 600, width = 320, height = 180, units = "mm")

# Save as png
ggsave(filename = here::here("output", "images", "f_b_waves_bc.jpg"), plot = p_eb23,
       dpi = 600, width = 320, height = 180, units = "mm")

# Save as png
ggsave(filename = here::here("output", "images", "f_ewb.jpg"), plot = p_tot,
       dpi = 600, width = 320, height = 180, units = "mm")


# Save as .eps
ggsave(filename = here::here("output", "f_ew_wave_b.eps"), plot = p_tot,
       device = cairo_ps, fallback_resolution = 600,
       dpi = 600, width = 320, height = 180, units = "mm")

