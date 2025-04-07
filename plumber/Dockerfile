# Use a specific R version base image
FROM rocker/r-ver:4.2.0

# Install system dependencies needed for R packages
# Combine steps and clean up apt cache to reduce image size
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    # Added fortran for some statistical packages that might require it
    gfortran \
    # Added for debugging
    procps \
    vim \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages from CRAN with specific error output
RUN R -e "options(warn=2); cat('Installing base packages...\n'); tryCatch({install.packages(c('plumber', 'jsonlite', 'dplyr', 'tidyr', 'readr', 'purrr', 'remotes', 'rlang'), repos='https://cloud.r-project.org/')}, error = function(e) {cat('ERROR:', conditionMessage(e), '\n'); quit(status=1)}); cat('Base packages installed successfully\n')"

# Install your custom package from GitHub with error handling
RUN R -e "options(warn=2); cat('Installing peakPerformR from GitHub...\n'); tryCatch({remotes::install_github('elivatsaas/peakPerformR')}, error = function(e) {cat('ERROR:', conditionMessage(e), '\n'); quit(status=1)}); cat('peakPerformR installed successfully\n')"

# Set the working directory in the container
WORKDIR /app

# Create a startup debugging script
RUN echo '#!/bin/bash\n\
echo "=== Container Startup - $(date) ==="\n\
echo "Working directory: $(pwd)"\n\
echo "R version: $(R --version | head -n 1)"\n\
echo "Directory contents:"\n\
ls -la\n\
echo ""\n\
echo "Data directory contents:"\n\
ls -la data/ || echo "Warning: data/ directory not found or not accessible"\n\
echo ""\n\
echo "Checking R packages:"\n\
R -e "cat(paste0(\"peakPerformR version: \", packageVersion(\"peakPerformR\")), \"\\\n\")"\n\
echo ""\n\
echo "Starting Plumber API..."\n\
R -e "tryCatch({\n\
  cat(\"Loading plumber...\\\n\");\n\
  library(plumber);\n\
  cat(\"Plumber loaded. Setting up API...\\\n\");\n\
  pr <- plumber::plumb(\"plumber.R\");\n\
  cat(\"API set up. Starting server...\\\n\");\n\
  pr\$run(host=\"0.0.0.0\", port=8080)\n\
}, error = function(e) {\n\
  cat(\"ERROR STARTING API:\", conditionMessage(e), \"\\\n\");\n\
  cat(\"Traceback:\\\n\");\n\
  cat(capture.output(print(rlang::trace_back())), sep=\"\\\n\");\n\
  quit(status=1)\n\
})"\n' > /app/start.sh && chmod +x /app/start.sh

# Copy only necessary application files
# plumber.R contains the API logic
COPY plumber.R /app/

# Copy pre-computed data required by the API
COPY data/ /app/data/

# Make sure the plumber.R file is readable
RUN chmod 644 /app/plumber.R

# Expose the port the Plumber API will run on
EXPOSE 8080

# Use the debugging startup script
CMD ["/app/start.sh"]
