// Container Registry
param location string = resourceGroup().location
param containerRegistryName string = 'acr${uniqueString(resourceGroup().id)}'
param appServicePlanName string = 'asp-${uniqueString(resourceGroup().id)}'
param appServiceName string = 'app-${uniqueString(resourceGroup().id)}'
param appServiceName2 string = 'app2-${uniqueString(resourceGroup().id)}'

resource containerRegistry 'Microsoft.ContainerRegistry/registries@2021-09-01' = {
  name: containerRegistryName
  location: location
  sku: {
    name: 'Standard'
  }
  properties: {
    adminUserEnabled: true
    publicNetworkAccess: 'Enabled'
  }
}

//Azure App Service
resource appServicePlan 'Microsoft.Web/serverfarms@2021-03-01' = {
  name: appServicePlanName
  location: location
  sku: {
    name: 'B1'
    tier: 'Basic'
  }
  kind: 'linux'
  properties: {
    reserved: true
  }
}

resource appService 'Microsoft.Web/sites@2021-03-01' = {
  name: appServiceName
  location: location
  properties: {
    serverFarmId: appServicePlan.id
    containerSize: 0
    dailyMemoryTimeQuota: 0
  }
}

resource appService2 'Microsoft.Web/sites@2021-03-01' = {
  name: appServiceName2
  location: location
  properties: {
    serverFarmId: appServicePlan.id
    containerSize: 0
    dailyMemoryTimeQuota: 0
  }
}

