# Start with the specific R version you require (matching the *newly generated* renv.lock)
FROM rocker/r-ver:4.3.1

# Set DEBIAN_FRONTEND to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# --- Install System Dependencies ---
# Required by renv/R packages for compilation
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

# --- Configure renv & Install Packages ---
WORKDIR /app

# Copy the renv lockfile (This should now be the one generated with R 4.3.1)
COPY renv.lock .

# Restore packages specified in renv.lock using renv
# This should now work correctly as the lockfile matches the R version
RUN R -e "renv::restore()"

# --- Verification Step (Optional but Recommended) ---
# Check if renv believes all packages are installed after restore
RUN R -e "cat('--- renv status after restore ---\n'); options(width=120); renv::status()"

# --- Configure Application ---
# Copy the rest of your application code
COPY . /app/

# Install your custom GitHub package AFTER renv::restore()
# (Only needed if 'peakPerformR' is NOT managed within your renv.lock file)
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Verify files copied (Optional Debugging)
RUN echo "--- Files in /app after COPY ---" && ls -lha /app && echo "--- Files in /app/plumber ---" && ls -lha /app/plumber || echo "/app/plumber not found or empty"

# Ensure the data directory exists (safer after COPY)
RUN mkdir -p ./data

# --- Setup Startup Script ---
# Make the copied start.sh executable
RUN chmod +x /app/start.sh

# --- Expose Port and Run ---
# Expose the standard port 80, which matches the Azure Target Port
EXPOSE 80

# Run via the start.sh script
CMD ["/app/start.sh"]
