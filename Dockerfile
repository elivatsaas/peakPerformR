name: Build and Deploy to Azure
on:
  push:
    branches: [ master ]
  workflow_dispatch:

permissions:
  id-token: write
  contents: read

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout code
        uses: actions/checkout@v3
      
      - name: Login to Docker Hub
        uses: docker/login-action@v2
        with:
          username: ${{ secrets.DOCKER_HUB_USERNAME }}
          password: ${{ secrets.DOCKER_HUB_ACCESS_TOKEN }}
      
      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v2
      
      - name: Build and push Docker image
        uses: docker/build-push-action@v4
        with:
          context: .
          file: ./Dockerfile
          push: true
          tags: ${{ secrets.DOCKER_HUB_USERNAME }}/peakperformr-api:latest
      
      - name: Azure login
        uses: azure/login@v1
        with:
          client-id: ${{ secrets.PEAKPERFORMRAPI_AZURE_CLIENT_ID }}
          tenant-id: ${{ secrets.PEAKPERFORMRAPI_AZURE_TENANT_ID }}
          subscription-id: ${{ secrets.PEAKPERFORMRAPI_AZURE_SUBSCRIPTION_ID }}
          enable-AzPSSession: true
      
      - name: Update Container App
        uses: azure/CLI@v1
        with:
          inlineScript: |
            echo "Updating container app..."
            az containerapp update \
              --name peakperformr-api-final \
              --resource-group NAUFLG-asc-export \
              --container-name peakperformr-api-container \
              --image ${{ secrets.DOCKER_HUB_USERNAME }}/peakperformr-api:latest \
              --debug
