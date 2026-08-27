param containerAppsEnvName string 
param appName string 
param location string 
param registryServer string
param registryIdentityId string
param httpPort int
param containerImage string 

resource caEnvironment 'Microsoft.App/managedEnvironments@2022-01-01-preview' existing = {
  name: containerAppsEnvName
}

resource containerApp 'Microsoft.App/containerApps@2022-03-01' ={
  name: appName
  location: location
  identity: {
    type: 'UserAssigned'
    userAssignedIdentities: {
      '${registryIdentityId}': {}
    }
  }
  properties:{
    managedEnvironmentId: caEnvironment.id
    configuration: {
      registries: [
        {
          server: registryServer
          identity: registryIdentityId
        }
      ]
      ingress: {
        targetPort: httpPort
        external: true
      }
      dapr: {
        enabled: true
        appId: appName
        appProtocol: 'http'
        appPort: httpPort
      }
    }
    template: {
      containers: [
        {
          image: containerImage
          name: appName
        }
      ]
    }
  }
}
