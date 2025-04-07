#!/bin/bash
echo "--- Running start.sh ---"
echo "Working Directory: $(pwd)" # Should be /app inside the container

PLUMBER_FILE="plumber.R"
EXPECTED_PATH="/app/plumber/plumber.R" # Specific path you mentioned
ROOT_PATH="/app/plumber.R"

# Check explicit path first
if [ -f "${EXPECTED_PATH}" ]; then
  PLUMBER_PATH="${EXPECTED_PATH}"
  echo "Found plumber script at expected path: ${PLUMBER_PATH}"
# Then check root path
elif [ -f "${ROOT_PATH}" ]; then
  PLUMBER_PATH="${ROOT_PATH}"
  echo "Found plumber script at root path: ${PLUMBER_PATH}"
else
  # If not found in expected places, search within /app using find
  echo "Searching for ${PLUMBER_FILE} in /app and subdirectories..."
  FOUND_PATH=$(find /app -name "${PLUMBER_FILE}" -print -quit)
  if [ -n "${FOUND_PATH}" ]; then
      PLUMBER_PATH="${FOUND_PATH}"
      echo "Found ${PLUMBER_FILE} via find at: ${PLUMBER_PATH}"
  else
      # Critical error if plumber.R is not found anywhere
      echo "ERROR: Cannot find ${PLUMBER_FILE} in ${EXPECTED_PATH}, ${ROOT_PATH}, or via find!"
      # List files for debugging
      echo "Listing files in /app:"
      ls -lha /app
      echo "Listing files in /app/plumber:"
      ls -lha /app/plumber || echo "/app/plumber not found"
      exit 1
  fi
fi

# Ensure data directory exists at runtime
mkdir -p /app/data
echo "Data directory /app/data ensured."

echo "Starting API using R script: ${PLUMBER_PATH}"

# Execute Plumber API, listening on all interfaces (0.0.0.0) on the exposed port
# Need to escape the $ in pr$run for the shell executing this line.
exec R -e "options(warn=2); pr <- plumber::plumb(file='${PLUMBER_PATH}'); pr\\$run(host='0.0.0.0', port=80)"
