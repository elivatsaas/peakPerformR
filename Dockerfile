# Dockerfile with renv debugging steps

# Start with the specific R version you require (ensure renv.lock matches!)
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

# Copy only the renv lockfile first
# Ensure renv.lock in your build context is the one generated for R 4.3.1!
COPY renv.lock .

# --- DEBUG STEP 1: Verify R Version in Lockfile ---
# Print the R version recorded inside the renv.lock file that was copied.
# This helps confirm if the correct lockfile is being used in the build.
# We need to ensure renv itself is available first to read the lockfile.
# We install a minimal renv here just for this check if needed, restore will use the lockfile version later.
RUN R -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv')" && \
    echo "--- Checking R version recorded in copied renv.lock ---" && \
    R -e "cat(paste0('Lockfile R Version: ', renv::lockfile_read('renv.lock')$R$Version, '\n'))" || \
    echo "!!! Warning: Could not read R version from renv.lock, proceeding with restore attempt. !!!"

# --- Restore packages specified in renv.lock using renv ---
# --- DEBUG STEP 2: Add verbose = TRUE ---
# This provides detailed output during the restore process to pinpoint failures.
RUN echo "--- Attempting renv::restore() with verbose output ---" && \
    R -e "renv::restore(verbose = TRUE)"
# If the above line fails, the build will stop here, and the detailed logs before the failure are what we need.

# --- Verification Step (Optional but Recommended) ---
# Check if renv believes all packages are installed after restore.
# This runs ONLY IF the restore step above succeeded.
RUN echo "--- Running renv::status() after successful restore attempt ---" && \
    R -e "options(width=120); renv::status()"

# --- Configure Application ---
# Copy the rest of your application code
# This includes your plumber/, data/, .Rprofile, renv/ directory (if needed), start.sh etc.
COPY . /app/

# Install your custom GitHub package AFTER renv::restore()
# (Only needed if 'peakPerformR' is NOT managed within your renv.lock file)
# This step should only run if renv::restore() was successful and installed 'remotes'.
RUN echo "--- Attempting to install GitHub package ---" && \
    R -e "remotes::install_github('elivatsaas/peakPerformR')"

# Verify files copied (Optional Debugging)
RUN echo "--- Files in /app after COPY ---" && ls -lha /app && echo "--- Files in /app/plumber ---" && ls -lha /app/plumber || echo "/app/plumber not found or empty"

# Ensure the data directory exists (safer after COPY)
RUN mkdir -p ./data

# --- Setup Startup Script ---
# Make the copied start.sh executable (ensure it exists and is copied by 'COPY . /app/')
RUN chmod +x /app/start.sh

# --- Expose Port and Run ---
# Expose the standard port 80 (ensure start.sh uses this port)
EXPOSE 80

# Run via the start.sh script (ensure it uses port 80)
CMD ["/app/start.sh"]
