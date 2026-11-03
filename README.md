# PCA

# Download R
First, download R for your OS from one of the mirrors within https://cran.r-project.org/mirrors.html

# Using R Console
First add pca.r to your directory 

Run R Console using this command:
```powershell
R --vanilla
```

Within R Console, run this command (make sure to add pca.r to your directory first):
```R
# Specify the repo to get your libraries from
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install "pak" for easier library installation in the future
install.packages("pak", dependencies=TRUE)

# Using "pak" to install our libraries
pak::pkg_install(c("MASS", "plotly"))   

# Run pca.r
source("pca.r", echo=TRUE)
```

