# install.packages("tidyverse")
library(tidyverse)
library(patchwork)
library(ggplot2)

# Step 1
data1 = read.csv("data/essentia.data.csv")
allentown = read.csv("data/essentia.data.allentown.csv")
# help(group_by)
# help(quantile)
# help(summarize)
# help(mutate)
view(data1)

# Step 1

out = function(data, feature){
  summary = data %>%
    group_by(artist) %>%
    summarize(
      min = min(data[[feature]], na.rm = TRUE),
      LF = quantile(data[[feature]], 0.25, na.rm = TRUE),
      UF = quantile(get(feature), 0.75, na.rm = TRUE),
      max = max(get(feature), na.rm = TRUE),
    ) %>%
    
    mutate(
      allentown.val = allentown %>% pull(get(feature)),
      out.of.range = allentown.val < min | allentown.val > max,
      unusual = allentown.val < LF | allentown.val > UF,
      description = case_when(
        out.of.range ~ "Out of Range",
        unusual ~ "Outlying",
        TRUE ~ "Within Range"
      )
    )
    
  return(summary)
}

# Step 2

numeric = names(data1)[sapply(data1, is.numeric)]
df = data.frame()

for (col in numeric){
  first = out(data1, col)
  first$feature = col
  df = bind_rows(df, first)
}

view(df)

# Step 3
help(print)
latex.table = xtable(df)

# Step 4

ggplot(data = data1, aes(x = artist, y = value, fill = statistic)) + 
  geom_col(position = "dodge") +
  facet_wrap(~feature, scales = "free") +
  theme_minimal +
  xlab("Artist") +
  ylab("Value")


  



