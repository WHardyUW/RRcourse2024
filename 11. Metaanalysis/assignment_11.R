rm(list = ls())

## Get this script present location
LOC_CODE = dirname(rstudioapi::getSourceEditorContext()$path)

print(LOC_CODE)
## Set it to working direcotry
setwd(LOC_CODE)

library(dplyr)
library(meta)

m_data <- readxl::read_excel("data/metaanalysis_data.xlsx")

m.raw_boys_play_female <- metacont(n.e=N_boys,
                  mean.e=Mean_boys_play_female,
                  sd.e=SD_boys_play_female,
                  n.c=N_boys,
                  mean.c=Mean_boys_play_male,
                  sd.c=SD_boys_play_male,
                  data=m_data,
                  studlab=paste(Study),
                  comb.fixed = TRUE,
                  comb.random = TRUE,
)

m.raw_boys_play_female

m.raw_girsl_play_male <- metacont(n.e=N_girls,
                       mean.e=Mean_girls_play_male,
                       sd.e=SD_girls_play_male,
                       n.c=N_girls,
                       mean.c=Mean_girls_play_female,
                       sd.c=SD_girls_play_female,
                       data=m_data,
                       studlab=paste(Study),
                       comb.fixed = TRUE,
                       comb.random = TRUE,
)

m.raw_girsl_play_male

m.raw_boys_play_female %>% forest()

m.raw_girsl_play_male %>% forest()

m.raw_boys_play_female %>% funnel()

m.raw_girsl_play_male %>% funnel()

m.raw_boys_play_female %>% metareg(`Study` + `Male authors`)
m.raw_boys_play_female %>% metareg(`Study` + `Female authors`)
