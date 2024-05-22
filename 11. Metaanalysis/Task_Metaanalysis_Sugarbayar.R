library(readxl)
library(meta)
library(metafor)
library(dplyr)
# 1. Import data
df=read_excel('data/metaanalysis_data.xlsx')

# 2. Experiment

# a. combine the effects
m <- metagen(TE=Mean_boys_play_male,
             seTE=SD_boys_play_male,
             data=df,
             studlab=paste(Study),
             comb.fixed = TRUE,
             comb.random = FALSE)
m
m.raw <- metacont(n.e=N_boys,
                  mean.e=Mean_boys_play_male,
                  sd.e=SD_boys_play_male,
                  n.c=N_girls,
                  mean.c=Mean_boys_play_female,
                  sd.c=SD_boys_play_female,
                  data=df,
                  studlab=paste(Study),
                  comb.fixed = TRUE,
                  comb.random = TRUE,
)
m.raw
m_re <- metagen(TE=Mean_boys_play_male,
                seTE=SD_boys_play_male,
                data=df,
                studlab=paste(Study),
                comb.fixed = FALSE,
                comb.random = TRUE)
m_re

# b. create a funnel plot (what do you see?)
contour_levels <- c(0.90, 0.95, 0.99)
contour_colors <- c("darkblue", "blue", "lightblue")
funnel(m, contour = contour_levels, col.contour = contour_colors)
legend("topright", c("p < 0.10", "p < 0.05", "p < 0.01"), bty = "n", fill = contour_colors)
m %>% funnel()
m %>% forest(sortvar=Mean_boys_play_male)

# c. check if methods / quality affect the results

# d. does author gender affect it?
print("Author gender affects on it. We can see it from forest plot")
