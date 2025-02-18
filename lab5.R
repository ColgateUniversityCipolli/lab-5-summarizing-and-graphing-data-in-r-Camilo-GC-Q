install.packages("tidyverse")
library(tidyverse)

# Step 1
data1 = read.csv("data/essentia.data.csv")
allentown = read.csv("data/essentia.data.allentown.csv")
# help(group_by)
help(quantile)
help(summarize)
help(mutate)
view(data)


out = function(data, feature){
  data |>
    group_by(artist) |>
    summarize(min = min(feature, na.rm = TRUE), LF = median(feature, na.rm = TRUE) - 2*IQR(feature, na.rm = TRUE), UF = median(feature, na.rm = TRUE) + 2*IQR(feature, na.rm = TRUE), max = max(feature, na.rm = TRUE)) |>
    mutate(out.of.range = allentown$feature > max | allentown$feature < min) |>
    mutate(unusual = allentown$feature < LF | allentown$feature > UF) |>
    mutate(description = case_when(out.of.range == TRUE ~ "Out of Range",
                                   unusual == TRUE ~ "Outlying",
                                   TRUE"Within Range"))
}

for (col in colnames(data1)){
  if(class(data1$col) == "numeric"){
    out1 = out(data1, col)
  }
}
view(data1)

