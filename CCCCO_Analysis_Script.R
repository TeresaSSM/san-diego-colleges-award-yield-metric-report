# =====================================================
# CCC award yield and award mix, 2023-24 to 2025-26
# Data: CCC Chancellor's Office Data Mart (awards + headcount)
# =====================================================

library(pacman)
p_load(tidyverse, skimr, scales, patchwork, ggthemes, ggrepel)

## 1. READ THE RAW FILES (never edited)
awards_raw   <- read.csv("ProgAwards.csv")
headcount_raw <- read.csv("StudentHeadcounts.csv")


## 2. CLEAN THE AWARDS FILE

# Give the columns readable names
awards_named <- awards_raw %>%
  setNames(c("college_label", "award_type", "2023-2024", "2024-2025", "2025-2026")) %>%
  mutate(across(c(`2023-2024`, `2024-2025`, `2025-2026`),
                ~ parse_number(as.character(.x))))

# Copy each college's name down onto its award-type rows.
awards_flagged <- awards_named %>%
  mutate(is_total = !is.na(college_label) & college_label != "",
         college  = if_else(is_total, str_remove(college_label, " Total$"), NA_character_)) %>%
  fill(college, .direction = "down") %>%
  filter(!str_detect(college, "State of California"))

# One row per college x row type x year
awards_long <- awards_flagged %>%
  pivot_longer(cols = c(`2023-2024`, `2024-2025`, `2025-2026`),
               names_to = "year",
               values_to = "n_awards")

# Split into college totals and award-type detail
college_totals <- awards_long %>% filter(is_total)
award_detail   <- awards_long %>% filter(!is_total)

# CHECK: do the award types add up to each college's total? (expect 0 rows)
award_detail %>%
  group_by(college, year) %>%
  summarize(detail_sum = sum(n_awards, na.rm = TRUE), .groups = "drop") %>%
  left_join(select(college_totals, college, year, total = n_awards),
            by = c("college", "year")) %>%
  filter(detail_sum != total)


## 3. CLEAN THE HEADCOUNT FILE

headcount_long <- headcount_raw %>%
  slice(-(1:2)) %>%
  setNames(c("college", "2023-2024", "2024-2025", "2025-2026")) %>%
  pivot_longer(cols = -college,
               names_to = "year",
               values_to = "headcount") %>%
  mutate(headcount = parse_number(as.character(headcount)))


## 4. MATCH COLLEGE NAMES BETWEEN THE TWO FILES

fix_names <- function(df) {
  df %>%
    mutate(college = str_squish(college),
           college = case_when(college == "Calbright College" ~ "CalBright",
                               college == "Norco"             ~ "Norco College",
                               TRUE                           ~ college))
}

college_totals <- fix_names(college_totals)
award_detail   <- fix_names(award_detail)
headcount_long <- headcount_long %>% mutate(college = str_squish(college))

# CHECK in both directions (expect 0 rows each)
anti_join(distinct(college_totals, college), distinct(headcount_long, college), by = "college")
anti_join(distinct(headcount_long, college), distinct(college_totals, college), by = "college")


## 5. METRIC 1: AWARD YIELD (awards per 100 students)
yield <- college_totals %>%
  select(college, year, awards = n_awards) %>%
  left_join(headcount_long, by = c("college", "year")) %>%
  mutate(yield_per_100 = awards / headcount * 100)

# CHECKS: expect 0 missing headcounts and 354 rows (118 colleges x 3 years)
sum(is.na(yield$headcount))
nrow(yield)
skim(yield)


## 6. METRIC 2: AWARD MIX (share of awards that are degrees / certificates / noncredit)
# Question: what kind of awards does each college give out?

# Sort every award type into one of 3 groups ("Noncredit" must be tested first)
award_class <- award_detail %>%
  mutate(award_type  = str_squish(award_type),
         award_group = case_when(str_detect(award_type, "(?i)^noncredit")   ~ "Noncredit",
                                 str_detect(award_type, "(?i)degree")       ~ "Degree",
                                 str_detect(award_type, "(?i)^certificate") ~ "Credit certificate",
                                 TRUE                                       ~ "Unclassified"))

# CHECK: expect no "Unclassified" row
count(award_class, award_group)

# Share of each college's awards in each group, one column per group
award_mix <- award_class %>%
  group_by(college, year, award_group) %>%
  summarize(n = sum(n_awards, na.rm = TRUE), .groups = "drop") %>%
  group_by(college, year) %>%
  mutate(share = n / sum(n) * 100) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = award_group, values_from = share, values_fill = 0)

# CHECK: shares add up to 100 (expect 0 rows)
award_mix %>%
  mutate(total = Degree + `Credit certificate` + Noncredit) %>%
  filter(abs(total - 100) > 0.001)

# Put yield and mix in one table
yield_mix <- yield %>% left_join(award_mix, by = c("college", "year"))


## 7. COMPARISON SET: CREDIT COLLEGES IN SAN DIEGO COUNTY (2025-26)
# Question: how do colleges in one region compare on yield and award mix?
# Rule: all 8 credit colleges in San Diego County. San Diego College of
# Continuing Education is excluded (100% noncredit, so yield isn't comparable).

latest <- yield_mix %>% filter(year == "2025-2026")
core   <- latest %>% filter(college != "North Orange Continuing Education" &
                            college != "San Diego College of Continuing Ed")      # all credit-based colleges
median_yield <- median(core$yield_per_100)        # statewide median, for context

# Find exact spellings 
latest %>%
  filter(str_detect(college, "(?i)san diego|grossmont|cuyamaca|palomar|miracosta|southwestern")) %>%
  select(college, headcount, Noncredit)

sd_colleges <- tribble(
  ~college,             ~district,
  "San Diego City",     "San Diego CCD",
  "San Diego Mesa",     "San Diego CCD",
  "San Diego Miramar",  "San Diego CCD",
  "Grossmont",          "Grossmont-Cuyamaca CCD",
  "Cuyamaca",           "Grossmont-Cuyamaca CCD",
  "Palomar",            "Palomar CCD",
  "MiraCosta",          "MiraCosta CCD",
  "Southwestern",       "Southwestern CCD"
)

# CHECKS: expect character(0) (every name found) and 8 rows
setdiff(sd_colleges$college, latest$college)
nrow(sd_colleges)

sd_data <- latest %>% inner_join(sd_colleges, by = "college")

## 8. CHARTS

# Chart 1: award yield, colored by district
fig1<-sd_data %>%
  mutate(fill_group=case_when(district == "San Diego CCD" ~ "San Diego Community College District (SDCCD)",
                              TRUE ~ "Other San Diego county districts"))%>%
  ggplot(aes(x = reorder(college, yield_per_100), y = yield_per_100, fill = fill_group)) +
  geom_col() +
  geom_hline(yintercept = median_yield, linetype = "dashed") +
  coord_flip() +
  labs(title = "SDCCD's 3 colleges have the lowest award yields in the county",
       subtitle = "Awards per 100 students, 2025-2026. Dashed line is statewide median of credit-based colleges",
       x = NULL,
       y = "Awards per 100 students")+ theme_stata() + 
  scale_fill_manual(values=c("#FFE080", "#FFB600" ), name= NULL)+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

# Fig 2: award mix at each college
yield_order <- sd_data %>% arrange(yield_per_100) %>% pull(college)

fig2<-sd_data %>%
  select(college, Degree, `Credit certificate`, Noncredit) %>%
  pivot_longer(cols = c(Degree, `Credit certificate`, Noncredit),
               names_to = "award_group", values_to = "share") %>%
  mutate(college = factor(college, levels = yield_order),
         award_group = factor(award_group))%>%
  ggplot(aes(x = college, y = share, fill = award_group)) +
  geom_col() +
  coord_flip() +
  labs(title = "Degrees are the largest share of awards at 7 of 8 colleges",
       subtitle = "Share of awards by type, 2025-2026",
       x = NULL,
       y = "% of awards") + 
  theme_stata() + 
  scale_fill_viridis_d(option = "mako",begin = 0.1, end = 0.9,direction = -1, name = NULL)+
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

#Fig 3: Awards vs Headcount 
cor(core$headcount, core$awards)

fig3<-core%>%
  mutate(fill_group = case_when(
    college %in% sd_colleges$college ~ "sd_college",
    TRUE ~ "non_sd_college"))%>%
  ggplot(aes(x=headcount, y=awards))+
  geom_smooth(method = "lm", se = FALSE, colour = "black")+
  geom_point(size=6, aes(color = fill_group), alpha=0.75) + 
  theme_stata() + 
  labs(title = "Headcount vs Awards, 2025-2026",
       x = "College Headcount",
       y = "Awards") +
  scale_color_manual(values=c("#555759", "#FFB600" ), name = NULL, 
                     labels = c("sd_college"     = "San Diego county",
                                            "non_sd_college" = "Other community colleges")) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1))+
  geom_text_repel(data = filter(core, college %in% sd_colleges$college),
                  aes(label = college),
                  size = 4,
                  fontface = "bold",
                  box.padding = 2.2,        # space around each label (was 0.8)
                  point.padding = 0.8,      # space kept between labels and points (was 0.6)
                  force = 3,                # labels push apart harder
                  force_pull = 0.2,         # lower = labels are pulled less back toward their point
                  min.segment.length = 0,
                  max.overlaps = Inf,
                  show.legend = FALSE)

fig1 
fig2


fig3+(fig1/fig2)

ggsave("chart1_yield.png", fig1, width = 8, height = 5, dpi = 1000)
ggsave("chart2_mix.png",   fig2, width = 8, height = 5, dpi = 1000)

## STATISTICAL TESTS
# Q1a: are the 8 San Diego colleges centered on the statewide median?
wilcox.test(sd_data$yield_per_100, mu = median_yield)
# Q2: 3 SDCCD colleges vs. the other 5 San Diego County colleges
sd_grouped <- sd_data %>%
  mutate(sdccd = if_else(district == "San Diego CCD", "SDCCD", "Other SD County"))

sd_grouped %>%
  group_by(sdccd) %>%
  summarize(n = n(),
            median_yield = median(yield_per_100),
            min = min(yield_per_100),
            max = max(yield_per_100))

wilcox.test(yield_per_100 ~ sdccd, data = sd_grouped)

# Small table
sd_data %>%
  select(college, district, headcount, yield_per_100, Degree, `Credit certificate`, Noncredit) %>%
  arrange(desc(yield_per_100))
