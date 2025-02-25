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
help(ggplot)
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
print(latex.table)

# Step 4

features = c("overall_loudness", "tempo", "danceability", "emotion")

df.plot = df %>%
  filter(feature %in% features) %>%
  select(artist, feature, LF, UF, min, max, allentown.val)

box.plot = ggplot(df.plot, aes(x = artist, y = allentown.val)) +
  geom_boxplot(aes(ymin = min, lower = LF, middle = (LF+UF) / 2, upper = UF, ymax = max),
               stat = "identity", width = 0.3) + 
  geom_point(size = 3, color = "red") +
  theme_bw() + 
  facet_wrap(~feature, scales = "free", drop = FALSE) +
  xlab("Artist") + 
  ylab("Feature Value") + 
  ggtitle("Boxplot Comparison of Allentown's Features")

scatter.plot = ggplot(df.plot, aes(x = artist, y = allentown.val)) + 
  geom_jitter(width = 0.2, size = 3) +
  theme_bw() + 
  facet_wrap(~feature, scales = "free", drop = FALSE) + 
  xlab("Artist") +
  ylab("Feature Value") +
  ggtitle("Jittered Scatter Plot of Allentown's Features Across Artists")

df.summary = df %>%
  group_by(artist, description) %>%
  summarize(count = n(), .groups = "drop")

outlier.plot = ggplot(df.summary, aes(x = artist, y = count, fill = description)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  scale_fill_manual(values = c("Out of Range" = "red", "Outlying" = "blue",
                               "Within Range" = "green")) +
  xlab("Band") +
  ylab("Count of Features")



scatter.plot / box.plot / outlier.plot

