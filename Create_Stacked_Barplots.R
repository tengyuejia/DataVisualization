library(dplyr)
library(ggplot2)

# Generate a simulate dataset 
set.seed(123)
data <- data.frame(
  item1 = as.factor(sample(1:5, 500, replace = TRUE, prob = c(0.1, 0.15, 0.2, 0.25, 0.3))),
  item2 = as.factor(sample(1:5, 500, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1))),
  item3 = as.factor(sample(1:5, 500, replace = TRUE, prob = c(0.2, 0.1, 0.3, 0.2, 0.2))),
  item4 = as.factor(sample(1:5, 500, replace = TRUE, prob = c(0.15, 0.25, 0.1, 0.3, 0.2))),
  item5 = as.factor(sample(1:5, 500, replace = TRUE, prob = c(0.2, 0.2, 0.25, 0.1, 0.25))),
  respondent_type = as.factor(sample(c("qualtrics", "organic"), 500, replace = TRUE))
)

# Melt the data to a long format
data_long <- reshape2::melt(data, id.vars = "respondent_type", variable.name = "item", value.name = "response")

# Calculate the percentages for each item-response type combination
data_percent <- data_long %>%
  group_by(item, response, respondent_type) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))


# ----------------- create stacked ---------------

# Define custom colors for the graph
my_colors <- c("#FC9272", "#9E9AC8", "#A1D99B", "#FD8D3C", "#E6550D")

# Adjust the theme for custom font and font size
my_theme <- theme(
  text = element_text(size = 12, family = "Arial", colour = "black"),
  axis.text = element_text(size = 10, colour = "black"),
  axis.title = element_text(size = 12, colour = "black"),
  plot.title = element_text(size = 16, colour = "black"),
  legend.text = element_text(size = 12, colour = "black")
)

#
combined_graph <- ggplot(data_percent
                         , aes(x = respondent_type, y = percentage, fill = response)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Percentage of Responses for Each Item - Combined Samples", y = "Percentage", x = "Respondent Type", fill = "Response") +
  theme_minimal() +
  my_theme +
  scale_fill_manual(values = my_colors) +
  geom_text(aes(label = paste0(round(percentage*100), "%")), position = position_stack(vjust = 0.5), colour = "white", size = 3) +
  facet_wrap(~ item)

print(combined_graph)


# ----------------- create nonstacked ---------------
# 
combined_graph <- ggplot(data_percent, aes(x = respondent_type, y = percentage, fill = response)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Responses for Each Item - Combined Samples"
       , y = "Percentage", x = "Respondent Type", fill = "Response") +
  theme_minimal() +
  my_theme +
  scale_fill_manual(values = my_colors) +
  geom_text(aes(label = paste0(round(percentage*100), "%"))
            , position = position_dodge(width = 0.9), colour = "black", size = 3) +
  facet_wrap(~ item)

print(combined_graph)

# Copyright 2023 Yuejia Teng, Ph.D.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
