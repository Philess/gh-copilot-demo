# Music store migration implementation plan

## Problem and approach

Replace the original Vue catalog and ASP.NET Core API with an Angular + Angular
Material frontend and Java 25 + Spring Boot API. Persist the existing six-album
catalog in Azure Database for PostgreSQL Flexible Server. Deploy new containers
side by side with the original app on Azure Container Apps, retaining Dapr
sidecars and service invocation. Validate the replacement before switching the
published application link; keep the old URL available for rollback.

This is a plan only. No repository implementation, deployment, or existing
worktree cleanup is authorized by this artifact.

## Confirmed scope and decisions

- Baseline: `albums-api\` and `album-viewer\`, not the React/microservices variant.
- Preserve the read-only catalog, sample content, English copy, USD display,
  responsive layout, purple/indigo palette, loading state, error/retry state,
  lazy images, image fallback, and nonfunctional Add to Cart/Preview controls.
- Preserve `GET /albums` and its JSON contract. Preserve the numeric-ID
  `GET /albums/{id}` endpoint's existing empty HTTP 200 response, including
  nonexistent numeric IDs. Do not turn the stub into a detail feature.
- No CRUD, cart, orders, pricing service, authentication, playback, i18n, or
  migration of data from the alternate Dapr state stores.
- Use PostgreSQL through the Spring API's JDBC connection, not a Dapr state store.
- Keep Dapr enabled on both new Container Apps; frontend-to-API calls use Dapr
  service invocation. The browser never contacts a sidecar directly.
- Private PostgreSQL networking, TLS, Key Vault-managed credentials, separate
  schema-migration and read-only application roles.
- Use a new frontend Container App URL. Cutover switches the published link,
  not DNS, an existing hostname, or traffic weights across different apps.
- Leave original app projects, existing infrastructure entrypoints, and the
  React/microservices implementation in place throughout the migration.

## Current-state findings

| Evidence | Finding and migration implication |
|---|---|
| `albums-api\Controllers\AlbumController.cs` | MVC controller serves the list and an empty-200 detail stub; capture both behaviors before replacing it. |
| `albums-api\Models\Album.cs` | Six hardcoded records, IDs 1-6, prices 10.99-14.99, external image URLs; these records are the entire approved data migration. |
| `albums-api\Program.cs` | Dapr variables are unused; no database integration. Swagger is development-only and CORS is permissive. |
| `albums-api\Controllers\UnsecuredController.cs` | Educational anti-pattern artifact, not the catalog implementation; do not port it. |
| `album-viewer\src\App.vue` | Axios calls relative `/albums`; local loading/error state and explicit retry; no router or global store. |
| `album-viewer\src\components\AlbumCard.vue` | Displays title, artist, USD price, lazy image/fallback, and placeholder controls. |
| `album-viewer\src\types\album.ts` | JSON fields are `id`, `title`, `artist`, `price`, and `image_url`; retain the underscore field name. |
| `album-viewer\vite.config.ts` | Development proxy exists, but this is not a production frontend server/proxy definition. |
| `album-viewer\package.json` | Build/type-check commands exist; Vitest is declared, but no catalog tests were found. No backend test framework is configured. |
| `iac\bicep\main.bicep` and `modules\container-app.bicep` | Original deployment uses two external-ingress apps, Dapr, blob state, and registry credentials; no PostgreSQL or private-network configuration. |
| `.github\workflows\build-and-push.yaml` | Legacy workflow references `album-api` instead of `albums-api` and assumes Dockerfiles not found in the original projects. It is not a usable migration pipeline as-is. |
| Untracked `services\`, `shared\`, `album-viewer-react\`, `dapr\`, and related Bicep/workflows | Separate React + .NET microservices work has real Dapr invocation, state, cart/orders/pricing, and a different contract; preserve it, do not adopt its broader feature scope. |
| `iac\bicep\modules\microservice-app.bicep` | Useful reference for managed identity, ingress, probes, and Dapr, but currently untracked and configured for single revisions; do not make the new deployment depend on this unfinished work. |
| Working tree | Contains modified instructions and substantial untracked work. Do not overwrite, stage, delete, or commit unrelated files. |
| Local tools | Java executable currently resolves to a JDK 21 installation; provision/select JDK 25 for implementation. Maven, Node/npm, Docker, Azure CLI, and Dapr executables exist, but versions, daemon readiness, and Azure permissions are unverified. |

The root README has stale path/architecture claims; use executable source as the
baseline and update directly related documentation when implementing.

## Target design

### Projects and request flow

Create `albums-api-java\` and `album-viewer-angular\`. Keep the original projects
unchanged as the rollback baseline.

```text
Browser
  -> HTTPS: new Angular frontend Container App
  -> static server reverse proxy for /albums and /albums/{id}
  -> frontend Dapr sidecar
  -> Dapr service invocation: album-api-java
  -> internal Spring Boot API Container App
  -> JDBC over TLS and private networking
  -> Azure Database for PostgreSQL Flexible Server
```

- Use Maven Wrapper, Java 25, Spring MVC controllers, explicit JSON DTOs, a
  repository/service boundary, PostgreSQL JDBC, Flyway, and Actuator.
- Use standalone Angular components, HttpClient, and Angular Material. Keep the
  app single-page with component-local state; no unnecessary router/store.
- Use a static server such as Nginx in the frontend image. Translate `/albums`
  requests to the local sidecar's `/v1.0/invoke/album-api-java/method/albums`
  endpoint, preserving suffixes, query strings, status codes, and timeouts.
  Sidecar port and target app ID are server-side runtime configuration.
- Production app containers listen on port 8080; Dapr app ports match.
  Local development uses unused ports such as API 3100 and frontend 3101,
  with distinct sidecar ports, avoiding the original and React app ports.
- For Angular development, proxy the same relative API paths through the local
  frontend sidecar, using a dedicated Dapr multi-app configuration. Do not
  modify the existing `dapr.yaml`.
- API ingress is internal; frontend ingress is external HTTPS. Use minimum
  one replica initially to avoid introducing cold-start behavior at cutover.
  Set bounded maximum replicas and DB connection pools together.
- Database failures produce explicit errors, not an empty successful catalog.
  Readiness checks database connectivity; liveness does not depend on the DB.
  Frontend probes must distinguish static-server health from API-path readiness.

### Data and contract

- Create an `albums` table with integer primary key, title, artist, image URL,
  and `NUMERIC(10,2)` price. Use Java BigDecimal and emit a JSON number.
- Version the schema and exact six seed records with Flyway. Preserve IDs,
  strings, URLs, and price values. Return list results in ascending ID order.
- Run schema changes and seed migrations once through a dedicated migration
  process/job. Do not run privileged migrations on every API replica startup.
- Give the runtime DB role SELECT access only; migration credentials must not
  be available to serving containers. Define bootstrap, schema ownership,
  grants, and Flyway history ownership explicitly.
- Preserve the empty-200 detail stub and numeric-ID binding behavior. Capture
  invalid-ID behavior from the baseline and preserve the status-level contract
  without copying framework-specific error wording.
- Empty tables may legitimately return `[]`; connection/query errors must not
  be mistaken for empty tables or trigger silent reseeding.
- Use a bundled, loop-safe fallback image to preserve the fallback experience
  without relying on a second external placeholder service.

### Azure and delivery

- Add `iac\bicep\main-migration.bicep` and migration-specific modules under
  `iac\bicep\modules\`; leave both existing entrypoints untouched.
- Provision isolated, distinctly named resources in a dedicated resource
  group: VNet, Container Apps environment subnet, PostgreSQL delegated subnet,
  private DNS/linking, PostgreSQL Flexible Server/database, Key Vault, managed
  identities, logging/telemetry, and two Dapr-enabled Container Apps.
- Use private-access PostgreSQL connectivity and certificate-verified TLS.
  Confirm region/SKU/version availability and subnet constraints before deploy.
- Use ACR for immutable images and managed identity with correctly scoped pull
  permissions; use managed identity and Key Vault references for API secrets.
  Do not print secrets or put them in source, build arguments, or SPA bundles.
- Execute role bootstrap and Flyway in an in-network migration job/process;
  a public GitHub-hosted runner cannot directly access the private database.
  Define a narrowly authorized bootstrap path and credential rotation handling.
- Add dedicated migration CI/CD workflow(s) with path-scoped PR checks,
  build/test gates, OIDC Azure authentication, explicit permissions, immutable
  tags/digests, staging deployment, migration-job completion checks, and
  protected deployment approval. Do not use `latest` for release or rollback.
- Existing broad legacy workflow triggers can still run on new repository
  changes. Identify that interaction and agree a minimal exclusion before
  enabling new deployment workflows; avoid silently changing unrelated
  microservices pipelines or allowing duplicate deployments.
- Use new Dapr app IDs to avoid invocation collisions with old apps.
  PostgreSQL does not require catalog/pricing/cart/order Dapr components.

## Implementation todos and dependencies

1. **Capturing baseline and contract** (`migration-baseline`).
   Record list payload/status/content type, numeric and invalid detail-ID
   behavior, six seed records, and UI states. Establish compatibility fixtures,
   acceptance criteria, tool prerequisites, and pinned version choices.
   No prerequisites.
2. **Scaffolding replacement projects** (`migration-scaffold`).
   Generate Maven Wrapper/Spring and Angular/Material projects in new folders.
   Pin Java 25 and compatible framework/build/container versions, lock npm
   dependencies, and add scoped Java/Angular contributor guidance without
   altering existing Vue/C# rules. Depends on `migration-baseline`.
3. **Implementing PostgreSQL schema and migration** (`migration-data`).
   Add Flyway schema/seed migrations, role/grant bootstrap, local PostgreSQL
   setup, and repeatability/data-integrity tests. Depends on `migration-scaffold`.
4. **Implementing Spring catalog API** (`migration-api`).
   Implement the list contract, legacy detail stub, safe data access, explicit
   error handling, probes/telemetry, configuration, and Java container image.
   Add controller/contract tests and real-PostgreSQL integration tests.
   Depends on `migration-data`.
5. **Implementing Angular catalog frontend** (`migration-frontend`).
   Recreate approved states/layout/cards with Material and HttpClient.
   Add component/HTTP tests, server-side Dapr proxy, health routes, frontend
   image, and isolated local Dapr configuration.
   Depends on `migration-scaffold`; can proceed independently of API coding
   using the captured list contract.
6. **Provisioning isolated Azure infrastructure** (`migration-infrastructure`).
   Add new Bicep entrypoint/modules for networking, DB, roles/secret delivery,
   observability, in-network migrations, and Dapr-enabled apps. Validate module
   interfaces and configuration against generated containers before deploy.
   Depends on `migration-baseline`; can proceed independently of app coding.
7. **Wiring migration delivery pipeline** (`migration-delivery`).
   Add gated PR validation, immutable image publication, OIDC deployment,
   Bicep validation/what-if, migration execution, staging outputs, and smoke
   checks. Address workflow trigger interactions narrowly.
   Depends on `migration-api`, `migration-frontend`, and
   `migration-infrastructure`.
8. **Validating compatibility and rollback** (`migration-validation`).
   Exercise local and Azure Dapr flows, contract/data/UI parity, explicit
   failure handling, private connectivity, least privilege, readiness, image
   rollback, and recovery to the original published link.
   Depends on `migration-delivery`.
9. **Documenting migration operations** (`migration-documentation`).
   Update the root README with verified new-project commands and retained
   legacy commands. Add migration deployment/cutover/rollback instructions,
   configuration inventory, credential rotation and DB backup/restore
   procedures, and new scoped instructions. Do not rewrite unrelated docs.
   Depends on `migration-validation`.
10. **Executing approved link cutover** (`migration-cutover`).
    After explicit deployment/cutover approval, switch the published app link
    to the verified new frontend URL, check the user journey, and retain the old
    URL/resources and previous immutable images. No legacy deletion.
    Depends on `migration-documentation`.

## Validation and acceptance gates

- Existing baseline: build `albums-api\albums-api.csproj`; run the Vue
  type-check/build commands and capture HTTP fixtures if execution is feasible.
  Report pre-existing failures separately; do not fix unrelated demo artifacts.
  Tests have not been executed during this planning task.
- Java: `.\mvnw.cmd verify` in `albums-api-java\`, including PostgreSQL
  integration tests (Testcontainers requires a running Docker daemon).
- Angular: `npm ci`, `npm run build`, and the generated non-watch test command
  in `album-viewer-angular\`; document the exact final scripts after scaffolding.
- API parity: exactly the approved six records, ascending IDs, exact JSON field
  names including `image_url`, price values, and empty-200 numeric detail stub.
  Assert no unexpected write/cart/order endpoints are exposed.
- Persistence: apply migrations twice without duplicated rows or schema damage;
  restart API replicas without reseeding or losing data. Runtime role cannot
  modify data/schema; serving containers cannot access migration credentials.
- UI: loading, successful catalog, genuine empty catalog, API error/retry,
  image failures, two-decimal USD display, placeholder controls, keyboard
  accessibility, and layout above/below the 768px breakpoint.
- Dapr: verify the browser's relative request reaches the frontend proxy,
  frontend sidecar, target app ID, internal API, and DB. Test sidecar/API/DB
  unavailability; preserve error status and show the retry state.
- Infrastructure: `az bicep build --file iac\bicep\main-migration.bicep`,
  parameterized deployment validation and what-if, then approved staging
  deployment. No old-resource modification or unexpected deletion in what-if.
- Azure smoke checks: HTTPS frontend, private API exposure, private DB DNS/TLS,
  DB role grants, Key Vault references, ACR identity access, health probes,
  migrations, six-album response through Dapr, and useful telemetry.
- Bound pools against the DB connection limit and maximum replicas; measure
  the observed baseline before defining and agreeing any numerical performance
  gate. Do not invent a performance target or require an unrelated benchmark.
- Cutover gate: all required checks pass, actual published-link location and
  owner are identified, DB backups are configured, old URL is still usable,
  and rollback is rehearsed. Link rollback requires no DB reverse migration
  because the replacement catalog is read-only and the old app is independent.
- Planning verification only checks this artifact and its SQL dependencies;
  application builds, cloud provisioning, and runtime tests belong to execution.

## Considerations and execution prerequisites

- Keep dirty/untracked user work untouched. If implementation encounters
  conflicting edits or new paths, stop and clarify instead of overwriting.
- Determine subscription, resource group, region, ACR, environment approvals,
  deployment permissions, published-link owner, and hosting budget before
  creating resources. These values are not established by the repository.
- Java 25 is explicitly required. The checked Spring Boot system requirements
  report Java 25 compatibility; pin a supported release and verify JDBC,
  Flyway, test tooling, and container-image compatibility during scaffolding.
  Select a supported Angular/Material major together using Angular's official
  Node/TypeScript/RxJS compatibility matrix; do not reuse Vue toolchain versions.
- No Dapr removal exception is needed: the user chose to retain sidecars and
  service invocation. Its additional startup/proxy failure modes need tests.
- Do not reuse or add dependencies on the untracked alternate Bicep/workflows.
  Copy patterns only where justified, keeping migration files self-contained.
- New isolated resource names are essential: current infrastructure variants
  derive overlapping environment names from resource group/subscription seeds.
- Link cutover is not transparent migration of existing bookmarks. Users with
  the old URL continue to use the retained original app until the link changes.
- Legacy retirement, production data beyond the six seeds, new features,
  domain migration, and gradual shared-gateway traffic routing require a
  separate approved scope.

## Framework references

- Spring Boot system requirements:
  https://docs.spring.io/spring-boot/system-requirements.html
- Angular version compatibility:
  https://angular.dev/reference/versions
