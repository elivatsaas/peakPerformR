# PeakPerformR API

A Plumber API for the PeakPerformR package.

## Endpoints

- `GET /` - Health check and API documentation
- `POST /recalculate` - Recalculate metrics based on thresholds

## Deployment

This API is deployed to Azure Container Apps using GitHub Actions.

## Local Development

To run the API locally:

```r
library(plumber)
pr <- plumb("plumber.R")
pr$run(host = "0.0.0.0", port = 8080)
```
