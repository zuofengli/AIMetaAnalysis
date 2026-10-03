# =============================================================================
# 04_figures.R
# Publication figures for the dual-paradigm NMA manuscript.
# Run with R 4.5.2.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(gridExtra)
  library(scales)
})

# --- portable paths: resolved relative to this script ------------------------
script_dir <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f)) return(dirname(normalizePath(sub("^--file=", "", f[1]))))
  if (!is.null(sys.frame(1)$ofile)) return(dirname(normalizePath(sys.frame(1)$ofile)))
  normalizePath(getwd())
})

# --- Windows locale guard (see 00_curate.R) ----------------------------------
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE)
}
base <- normalizePath(file.path(script_dir, ".."))
res  <- file.path(base, "analysis/results")
cur  <- file.path(base, "analysis/curated")

# --- manuscript output root: OUTSIDE this git repository, so the manuscript
# and its figures can never be committed with the data/code release.  Resolved
# relative to this script (no drive letter); override with AIMETA_TEX_DIR.
tex_root <- Sys.getenv("AIMETA_TEX_DIR", "")
if (!nzchar(tex_root)) tex_root <- file.path(base, "..", "..", "manuscript_v2")
tex_root <- normalizePath(tex_root, winslash = "/", mustWork = FALSE)
figdir <- file.path(tex_root, "figures")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)

official <- c(Duravlumab = "Durvalumab", Tisleizumab = "Tislelizumab")
fix_names <- function(x) {
  for (i in seq_along(official)) x[x == names(official)[i]] <- unname(official[i])
  x
}

outcome_labels <- c(
  pcr = "pCR", mpr = "MPR", trae_any = "TRAE (any grade)",
  trae_g34 = "TRAE grade >=3", ae_any = "AE any cause (any grade)",
  ae_g34 = "AE any cause (grade >=3)", efs = "EFS", os = "OS")

read_pairs <- function(prefix, outcome) {
  f <- list.files(res, pattern = paste0("^", prefix, "_", outcome, "_fixed_pairs\\.csv$"),
                   full.names = TRUE)
  if (length(f) == 0) return(NULL)
  read.csv(f[1], stringsAsFactors = FALSE)
}

read_bayes_vs_chemo <- function(outcome) {
  f <- list.files(res, pattern = paste0("^bayes_", outcome, "_fixed_pairs\\.csv$"),
                  full.names = TRUE)
  if (length(f) == 0) return(NULL)
  d <- read.csv(f[1], stringsAsFactors = FALSE)
  d %>% distinct(treatment, vs, .keep_all = TRUE) %>%
    filter(vs == "Chemotherapy", treatment != "Chemotherapy") %>%
    transmute(treatment, paradigm = "Bayesian", est = med, lo, hi)
}

read_freq_vs_chemo <- function(outcome) {
  d <- read.csv(file.path(res, "netmeta_vs_chemo_all.csv"),
                stringsAsFactors = FALSE)
  d %>% filter(outcome == !!outcome, treatment != "Chemotherapy") %>%
    transmute(treatment, paradigm = "Frequentist",
              est = est_common, lo = lo_common, hi = hi_common)
}

# -----------------------------------------------------------------------------
# Figure 2: network geometry
# -----------------------------------------------------------------------------
# Every included trial randomised patients between a single ICI-chemotherapy
# regimen and chemotherapy, so every network is a star centred on chemotherapy.
# The layout is therefore computed analytically (chemotherapy at the origin,
# agents equispaced on a circle) instead of by a force-directed algorithm: the
# figure is deterministic and needs no random seed.
#   node area  proportional to the number of patients in that node
#   edge width proportional to the number of trials contributing the contrast
net_levels <- c("Chemotherapy", "Nivolumab", "Pembrolizumab", "Durvalumab",
                "Toripalimab", "Tislelizumab", "Camrelizumab")
net_cols <- c(Chemotherapy  = "#8c8c8c",
              Nivolumab     = "#984EA3",
              Pembrolizumab = "#377EB8",
              Durvalumab    = "#FF7F00",
              Toripalimab   = "#E41A1C",
              Tislelizumab  = "#A65628",
              Camrelizumab  = "#4DAF4A")

net_from_arms <- function(file) {
  d <- read.csv(file.path(cur, file), stringsAsFactors = FALSE)
  d$trt <- fix_names(d$trt)
  nodes <- d %>% group_by(trt) %>% summarise(n = sum(n), .groups = "drop")
  edges <- d %>% filter(arm == "exp") %>% group_by(to = trt) %>%
    summarise(k = dplyr::n(), .groups = "drop") %>%
    transmute(from = "Chemotherapy", to = to, k = k)
  list(nodes = nodes, edges = edges, trials = nrow(d) / 2)
}

net_from_contrasts <- function(file) {
  d <- read.csv(file.path(cur, file), stringsAsFactors = FALSE)
  d$treat1 <- fix_names(d$treat1); d$treat2 <- fix_names(d$treat2)
  nodes <- bind_rows(d %>% transmute(trt = treat1, n = n1),
                     d %>% transmute(trt = treat2, n = n2)) %>%
    group_by(trt) %>% summarise(n = sum(n), .groups = "drop")
  edges <- d %>% group_by(from = treat1, to = treat2) %>%
    summarise(k = dplyr::n(), .groups = "drop")
  list(nodes = nodes, edges = edges, trials = nrow(d))
}

net_panel <- function(net, title) {
  center <- "Chemotherapy"
  nodes <- net$nodes; edges <- net$edges
  # orient every edge as centre -> peripheral agent
  flip <- edges$from != center
  tmp <- edges$from[flip]; edges$from[flip] <- edges$to[flip]; edges$to[flip] <- tmp
  others <- setdiff(c(intersect(net_levels, nodes$trt),
                      setdiff(nodes$trt, net_levels)), center)
  m <- length(others)
  ang <- pi / 2 - (seq_len(m) - 1) * (2 * pi / m)
  pos <- data.frame(trt = c(center, others), x = c(0, cos(ang)), y = c(0, sin(ang)))
  pos <- merge(pos, nodes, by = "trt", all.x = TRUE)
  pos$is_center <- pos$trt == center
  seg <- merge(edges, pos[, c("trt", "x", "y")], by.x = "to", by.y = "trt")
  # edge count printed at the midpoint, nudged perpendicular to the spoke
  seg$lx <- seg$x / 2 - 0.075 * seg$y
  seg$ly <- seg$y / 2 + 0.075 * seg$x
  ctr <- subset(pos, is_center)
  ctr$ly <- -0.235   # just clear of the largest comparator node
  ggplot() +
    geom_segment(data = seg, aes(x = x, y = y, xend = 0, yend = 0,
                                 linewidth = k),
                 colour = "grey62", lineend = "round") +
    scale_linewidth(range = c(0.55, 2.1), guide = "none") +
    geom_point(data = pos, aes(x, y, size = n, fill = trt),
               shape = 21, colour = "white", stroke = 1.0) +
    scale_size_area(max_size = 10, limits = c(0, NET_SIZE_MAX),
                    breaks = NET_BREAKS) +
    scale_fill_manual(values = net_cols, breaks = net_levels) +
    ggrepel::geom_text_repel(data = subset(pos, !is_center), aes(x, y, label = trt),
                             size = 2.55, seed = 1, min.segment.length = 0.25,
                             box.padding = 0.26, point.padding = 0.22,
                             segment.size = 0.25, segment.colour = "grey55") +
    geom_text(data = seg, aes(lx, ly, label = k), size = 2.4, colour = "grey35") +
    # comparator label below the node; the white under-layer masks the spoke
    # that runs behind the text, and is invisible on the white background
    geom_text(data = ctr, aes(x, ly), label = "Chemotherapy", size = 3.5,
              colour = "white", vjust = 1) +
    geom_text(data = ctr, aes(x, ly), label = "Chemotherapy", size = 2.45,
              colour = "grey10", vjust = 1) +
    coord_equal(xlim = c(-1.6, 1.6), ylim = c(-1.6, 1.6), expand = FALSE) +
    labs(title = title) +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 9.6, hjust = 0.5),
          legend.position = "none", plot.margin = margin(3, 3, 3, 3))
}

net_specs <- list(
  list(label = "Pathological response (pCR, MPR)",
       net = net_from_arms("arms_pcr.csv")),
  list(label = "Overall survival",
       net = net_from_contrasts("contrasts_os.csv")),
  list(label = "Treatment-related AEs, any grade",
       net = net_from_arms("arms_trae_any.csv")),
  list(label = "Any-cause AEs, grade >=3",
       net = net_from_arms("arms_ae_g34.csv"))
)
# panel titles are derived from the data, so they stay correct when trials are
# added or removed
net_titles <- paste0(
  LETTERS[seq_along(net_specs)], "  ",
  vapply(net_specs, function(s) s$label, ""),
  " \u2014 ",
  vapply(net_specs, function(s) as.integer(s$net$trials), 1L),
  " trials, ",
  vapply(net_specs, function(s) sum(s$net$nodes$trt != "Chemotherapy"), 1L),
  " agents")
# Node-area scale: the reference is derived from the data so that no node can
# fall outside the scale (an out-of-range value would silently drop the point)
# and so the legend always matches the panels.
NET_SIZE_MAX <- ceiling(max(unlist(lapply(net_specs, function(s) s$net$nodes$n))) / 100) * 100
NET_BREAKS <- pretty(c(0, NET_SIZE_MAX), n = 3)
NET_BREAKS <- NET_BREAKS[NET_BREAKS > 0]
net_panels <- lapply(seq_along(net_specs),
                     function(i) net_panel(net_specs[[i]]$net, net_titles[i]))
fig2 <- arrangeGrob(grobs = net_panels, ncol = 2)
ggsave(file.path(figdir, "fig2_network_geometry.pdf"), fig2,
       width = 9.5, height = 9.0, device = cairo_pdf)

# legend strip shipped separately so LaTeX can set it under the figure
leg_agents <- ggplot(data.frame(trt = factor(net_levels, levels = net_levels),
                                x = seq_along(net_levels), y = 1),
                     aes(x, y, fill = trt)) +
  geom_point(shape = 21, size = 0.1, colour = "white") +
  scale_fill_manual(values = net_cols, name = NULL) +
  guides(fill = guide_legend(override.aes = list(size = 4.6), nrow = 1)) +
  theme_void(base_size = 9) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.text = element_text(size = 8.2),
        legend.key.size = unit(4.4, "mm"),
        legend.margin = margin(0, 0, 0, 0))
leg_sizes <- ggplot(data.frame(n = NET_BREAKS, x = seq_along(NET_BREAKS), y = 1),
                    aes(x, y, size = n)) +
  geom_point(colour = "grey45") +
  scale_size_area(max_size = 10, limits = c(0, NET_SIZE_MAX),
                  breaks = NET_BREAKS, name = "Patients") +
  # the dummy points are pushed outside the panel range so that only the legend
  # is visible (the key sizes still come from the scale, not from the data)
  scale_x_continuous(limits = c(90, 99)) +
  theme_void(base_size = 9) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_text(size = 8.2, face = "plain"),
        legend.text = element_text(size = 8.2),
        legend.margin = margin(0, 0, 0, 0))
# the dummy points of leg_sizes are pushed outside the panel range, which
# raises a harmless "rows removed" warning at build time
fig2_legend <- suppressWarnings(
  arrangeGrob(leg_agents, leg_sizes, ncol = 2, widths = c(4.0, 1.55)))
suppressWarnings(
  ggsave(file.path(figdir, "fig2_network_legend.pdf"), fig2_legend,
         width = 9.5, height = 0.75, device = cairo_pdf))

# -----------------------------------------------------------------------------
# Figure 3: forest, ICI vs chemotherapy, frequentist & Bayesian fixed effects
# -----------------------------------------------------------------------------
outcomes_fig <- c("pcr", "mpr", "efs", "os", "trae_any", "ae_g34")
outcomes_mat <- c(outcomes_fig, "ae_any", "trae_g34")
plot_list <- list()
for (o in outcomes_fig) {
  fr <- read_freq_vs_chemo(o)
  by <- read_bayes_vs_chemo(o)
  d <- bind_rows(fr, by)
  d$treatment <- fix_names(d$treatment)
  ord <- by %>% arrange(est) %>% pull(treatment)
  d$treatment <- factor(d$treatment, levels = ord)
  d$paradigm <- factor(d$paradigm, levels = c("Frequentist", "Bayesian"))
  p <- ggplot(d, aes(x = est, y = treatment, color = paradigm,
                     shape = paradigm)) +
    geom_vline(xintercept = 1, linetype = 2, color = "grey45") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.22,
                   position = position_dodge(width = 0.6), linewidth = 0.55) +
    geom_point(position = position_dodge(width = 0.6), size = 2.3) +
    scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10, 30),
                  labels = c("0.2", "0.5", "1", "2", "5", "10", "30")) +
    scale_color_manual(values = c(Frequentist = "grey20", Bayesian = "#2166ac")) +
    scale_shape_manual(values = c(Frequentist = 19, Bayesian = 15)) +
    labs(x = if (o %in% c("efs", "os")) "Hazard ratio (95% CI)"
         else "Odds ratio (95% CI)", y = NULL,
         title = outcome_labels[[o]]) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  plot_list[[o]] <- p
}

fig2 <- arrangeGrob(grobs = plot_list, ncol = 2,
                    top = grid::textGrob(
                      "ICI plus chemotherapy versus chemotherapy alone: frequentist and Bayesian fixed-effect NMA",
                      gp = grid::gpar(fontface = "bold", fontsize = 12)))
ggsave(file.path(figdir, "fig3_forest_dual.pdf"), fig2,
       width = 10, height = 10, device = cairo_pdf)

legend_p <- ggplot(data.frame(x = 1:2, y = 1:2, g = c("Frequentist", "Bayesian")),
                   aes(x, y, color = g, shape = g)) +
  geom_point(size = 3) +
  scale_color_manual(values = c(Frequentist = "grey20", Bayesian = "#2166ac")) +
  scale_shape_manual(values = c(Frequentist = 19, Bayesian = 15)) +
  theme_void() + theme(legend.position = "bottom")
ggsave(file.path(figdir, "fig3_legend.pdf"), legend_p, width = 4, height = 1.5,
       device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 4: ranking comparison (frequentist P-score vs Bayesian SUCRA)
# -----------------------------------------------------------------------------
ps <- read.csv(file.path(res, "netmeta_pscores_all.csv"), stringsAsFactors = FALSE)
su <- list.files(res, pattern = "bayes_.*_fixed_sucra\\.csv$", full.names = TRUE) %>%
  lapply(read.csv, stringsAsFactors = FALSE) %>% bind_rows()
names(su)[names(su) == "sucra"] <- "score"
ps$score <- ps$pscore_common
ps <- ps %>% filter(treatment != "Chemotherapy") %>%
  select(outcome, treatment, score) %>% mutate(paradigm = "Frequentist (P-score)")
su <- su %>% filter(treatment != "Chemotherapy") %>%
  select(outcome, treatment, score) %>% mutate(paradigm = "Bayesian (SUCRA)")
rankdat <- bind_rows(ps, su)
rankdat$treatment <- fix_names(rankdat$treatment)
rankdat$outcome <- outcome_labels[rankdat$outcome]
rankdat$paradigm <- factor(rankdat$paradigm,
                           levels = c("Bayesian (SUCRA)", "Frequentist (P-score)"))

p3 <- ggplot(rankdat, aes(x = treatment, y = score, fill = paradigm)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  facet_wrap(~ outcome, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Bayesian (SUCRA)" = "#2166ac",
                               "Frequentist (P-score)" = "grey45")) +
  coord_flip() +
  labs(x = NULL, y = "Ranking metric (higher = more favourable)",
       fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 8))
ggsave(file.path(figdir, "fig4_ranking_dual.pdf"), p3,
       width = 11, height = 8.5, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 5: depth of pathological response
# -----------------------------------------------------------------------------
depth <- read.csv(file.path(cur, "arms_depth.csv"), stringsAsFactors = FALSE)
depth$trt <- fix_names(depth$trt)
expd <- depth %>% filter(arm == "exp") %>%
  mutate(pct_no_mpr = 100 - pct_mpr,
         pct_mpr_only = pct_mpr - pct_pcr)
long <- expd %>%
  select(trial, trt, pct_no_mpr, pct_mpr_only, pct_pcr) %>%
  pivot_longer(cols = starts_with("pct_"), names_to = "category",
               values_to = "pct") %>%
  mutate(category = factor(category,
                           levels = c("pct_no_mpr", "pct_mpr_only", "pct_pcr"),
                           labels = c("No MPR", "MPR without pCR", "pCR")))
long$label <- paste0(long$trial, "\n(", long$trt, ")")
long$label <- factor(long$label,
                     levels = long$label[!duplicated(long$label)])
cols <- c("No MPR" = "#d9d9d9", "MPR without pCR" = "#74add1", "pCR" = "#2166ac")

p4a <- ggplot(long, aes(x = label, y = pct, fill = category)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = cols) +
  labs(x = NULL, y = "Patients (%)", fill = NULL,
       title = "Depth of pathological response by neoadjuvant ICI-chemotherapy arm") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 7.5),
        legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))

sc <- expd %>% select(trial, trt, pct_pcr, pct_mpr)
p4b <- ggplot(sc, aes(x = pct_mpr, y = pct_pcr)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_point(size = 2.4, colour = "#2166ac") +
  ggrepel::geom_text_repel(aes(label = trt), size = 3.1, seed = 1) +
  labs(x = "MPR rate (%)", y = "pCR rate (%)",
       title = "pCR as a fraction of MPR across trials",
       subtitle = "Points below the identity line have substantial MPR-without-pCR") +
  coord_equal() + xlim(0, 100) + ylim(0, 100) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11))

fig4 <- arrangeGrob(p4a, p4b, ncol = 2, widths = c(1.15, 1))
ggsave(file.path(figdir, "fig5_depth_response.pdf"), fig4,
       width = 11, height = 5.2, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 6: significance-consistency matrix
# -----------------------------------------------------------------------------
# cells: OR/HR vs chemotherapy; bold if significant in both paradigms
panel_rows <- list()
for (o in outcomes_mat) {
  fr <- read_freq_vs_chemo(o); by <- read_bayes_vs_chemo(o)
  m <- merge(fr[, c("treatment", "est", "lo", "hi")],
             by[, c("treatment", "est", "lo", "hi")],
             by = "treatment", suffixes = c("_f", "_b"))
  m$sig_f <- with(m, (lo_f > 1 & hi_f > 1) | (lo_f < 1 & hi_f < 1))
  m$sig_b <- with(m, (lo_b > 1 & hi_b > 1) | (lo_b < 1 & hi_b < 1))
  m$state <- ifelse(m$sig_f & m$sig_b, "significant in both",
             ifelse(m$sig_f, "frequentist only",
             ifelse(m$sig_b, "Bayesian only", "neither")))
  m$outcome <- outcome_labels[[o]]
  m$treatment <- fix_names(m$treatment)
  panel_rows[[o]] <- m
}
cons <- bind_rows(panel_rows)
cons$outcome <- factor(cons$outcome,
                       levels = unique(outcome_labels[outcomes_mat]))
cons$treatment <- factor(cons$treatment,
                         levels = rev(sort(unique(cons$treatment))))
state_levels <- c("significant in both", "Bayesian only", "frequentist only",
                  "neither")
cons$state <- factor(cons$state, levels = state_levels)

p5 <- ggplot(cons, aes(x = outcome, y = treatment, fill = state)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  scale_fill_manual(values = c("significant in both" = "#1a9850",
                               "Bayesian only" = "#a6d96a",
                               "frequentist only" = "#fdae61",
                               "neither" = "#ffffbf"),
                    drop = FALSE) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Statistical-significance agreement between paradigms, ICI vs chemotherapy") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "bottom", plot.title = element_text(face = "bold", size = 11))
ggsave(file.path(figdir, "fig6_significance_matrix.pdf"), p5,
       width = 9.5, height = 5.5, device = cairo_pdf)

cat("Figures written to", figdir, "\n")
