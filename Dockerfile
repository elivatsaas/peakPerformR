# Start with a specific R version image
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

# Copy only the renv lockfile first (leverages Docker cache)
# Ensure renv.lock is in your project root and up-to-date!
COPY renv.lock .

# Install renv itself if not already part of the base image (or update it)
# RUN R -e "install.packages('renv', version = '1.1.4')" # Optionally pin renv version

# Restore packages specified in renv.lock using renv
# This installs packages into the renv library within the image
RUN R -e "renv::restore()"

# --- Configure Application ---
# Copy the rest of your application code
# This includes your plumber/, data/, .Rprofile, renv/ directory (if needed), start.sh etc.
COPY . /app/

# Install your custom GitHub package AFTER renv::restore()
# (Unless it's already managed within your renv.lock file, in which case this is redundant)
RUN R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Verify files copied (Optional Debugging)
RUN echo "--- Files in /app after COPY ---" && ls -lha /app && echo "--- Files in /app/plumber ---" && ls -lha /app/plumber || echo "/app/plumber not found or empty"

# Ensure the data directory exists (safer after COPY)
RUN mkdir -p ./data

# --- Setup Startup Script ---
# Make the copied start.sh executable (Assuming you use the COPY method for start.sh)
RUN chmod +x /app/start.sh

# --- Expose Port and Run ---
EXPOSE 80

# Run via the start.sh script (which will now run within the renv-aware environment)
CMD ["/app/start.sh"]
