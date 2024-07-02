library(tidyverse)
library(recipes)
library(h20)

accident_data <- read_csv("D:/Tableau/accident data.csv")
View(accident_data)



# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-3.46.0/3/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()



data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(25, 35, 20, 20)
)
# Calculate the percentage
data <- data %>%
  mutate(percentage = value / sum(value) * 100,
         ypos = cumsum(percentage) - 0.5 * percentage) # Calculate position for text
# Create the plot
donut_chart <- ggplot(data, aes(x = 2, y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) + # Adjust to create a donut shape
  theme_void() +  # Remove background, grid, and axis
  theme(legend.position = "right") +
  geom_text(aes(y = ypos, label = paste0(round(percentage, 1), "%")), color = "white") +
  labs(title = "Donut Chart Example")

print(donut_chart)


