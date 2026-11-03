# PCA on random 3D data, with the 3 principal components drawn as 3D vectors
#options(repos = c(CRAN = "https://cloud.r-project.org"))
#install.packages("pak", dependencies=TRUE)
#pak::pkg_install(c("MASS", "plotly"))   # run once if needed

library(plotly)
library(MASS)

set.seed(42)

# ---- 1. Random correlated 3D data -------------------------------------------
# Change this section based on data collected from survey
n     <- 300
Sigma <- matrix(c(4.0, 2.4, 1.2,
                  2.4, 3.0, 0.9,
                  1.2, 0.9, 1.5), nrow = 3, byrow = TRUE)
X <- mvrnorm(n, mu = c(5, 3, 2), Sigma = Sigma)
colnames(X) <- c("X1", "X2", "X3")

# ---- 2. PCA -----------------------------------------------------------------
pca <- prcomp(X, center = TRUE, scale. = FALSE)
print(summary(pca))

center <- colMeans(X)
scale_factor <- 2                                   # length = 2 standard deviations
vecs <- t(t(pca$rotation) * pca$sdev) * scale_factor  # columns = PC1..PC3 vectors
tips <- sweep(vecs, 1, center, "+")                 # arrow tips in original space

pc_cols   <- c("#e41a1c", "#377eb8", "#4daf4a")
var_expl  <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
pc_names  <- paste0("PC", 1:3, " (", var_expl, "%)")

# ---- 3. Interactive plot -------------------------------------------------
# This can only be visualized in 2d or 3d 
# Our data has more than 3 dimensions so the code below may not work

fig <- plot_ly() |>
  add_markers(x = X[, 1], y = X[, 2], z = X[, 3],
              marker = list(size = 3, color = "grey50", opacity = 0.5),
              name = "Data", hoverinfo = "none")

for (i in 1:3) {
  # vector shaft
  fig <- fig |>
    add_trace(type = "scatter3d", mode = "lines",
              x = c(center[1], tips[1, i]),
              y = c(center[2], tips[2, i]),
              z = c(center[3], tips[3, i]),
              line = list(color = pc_cols[i], width = 8),
              name = pc_names[i], legendgroup = paste0("pc", i)) |>
    # arrowhead
    add_trace(type = "cone",
              x = tips[1, i], y = tips[2, i], z = tips[3, i],
              u = vecs[1, i], v = vecs[2, i], w = vecs[3, i],
              sizemode = "absolute", sizeref = 0.8, anchor = "tip",
              colorscale = list(c(0, 1), c(pc_cols[i], pc_cols[i])),
              showscale = FALSE, legendgroup = paste0("pc", i),
              showlegend = FALSE, hoverinfo = "none")
}

fig <- fig |>
  layout(title = "PCA: principal components as 3D vectors",
         scene = list(xaxis = list(title = "X1"),
                      yaxis = list(title = "X2"),
                      zaxis = list(title = "X3"),
                      aspectmode = "data"))

fig  # opens in RStudio Viewer / browser; rotate, zoom and toggle PCs in the legend

# Optional: save as a standalone interactive HTML file
# htmlwidgets::saveWidget(fig, "pca_3d.html", selfcontained = TRUE)
