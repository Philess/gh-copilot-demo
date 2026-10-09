# Catalog migration baseline

Captured 2026-10-09 from the original `albums-api` and Vue `album-viewer`.
This is a read-only compatibility baseline; it does not scaffold replacement
applications or modify the original projects.

## Artifacts and use

- `api-baseline.json` is the machine-readable captured API response and status
  contract. It preserves all five JSON fields and the six exact records.
- `capture_api.py` makes the same list, numeric-detail, and invalid-ID requests
  against a running original API using only the Python standard library.
- `verify_api_contract.py` checks a capture against the committed baseline,
  including exact list content, ascending IDs, JSON content type, empty numeric
  detail responses, and invalid-ID status codes.
- `ui-acceptance.md` records the source-derived UI checklist and marks browser
  observations that were not performed.

With the original API running on its documented port:

```sh
python3 migration/baseline/capture_api.py --output /tmp/catalog-capture.json
python3 migration/baseline/verify_api_contract.py --capture /tmp/catalog-capture.json
```

The baseline fixture itself can be checked without starting the API:

```sh
python3 migration/baseline/verify_api_contract.py
```

The assertion deliberately ignores invalid-ID error bodies and the response
charset suffix; those are framework details, not compatibility requirements.

## Captured API observations

The original API was built and run locally in this Linux workspace. Its actual
responses were:

| Request | Status | Content type/body |
|---|---:|---|
| `GET /albums` | 200 | `application/json; charset=utf-8`; six records in `api-baseline.json`, IDs 1–6 |
| `GET /albums/1` | 200 | Empty body |
| `GET /albums/6` | 200 | Empty body |
| `GET /albums/999` | 200 | Empty body |
| `GET /albums/abc` | 400 | Status only retained |
| `GET /albums/1.5` | 400 | Status only retained |
| `GET /albums/2147483648` | 400 | Status only retained |

The numeric detail probes establish an existing ID, the last seed ID, and a
nonexistent numeric ID. Invalid values cover nonnumeric, fractional, and
out-of-range integer inputs. Their response wording is intentionally not
preserved.

## Original UI evidence

The checklist in `ui-acceptance.md` was traced to executable Vue templates,
handlers, and styles. `npm run type-check` and `npm run build` passed, but no
interactive/rendered-browser checks are claimed: the browser tool could not
authenticate, and no browser-level UI results were obtained.

## Commands and outcomes

All commands below were run without editing original project source or lockfiles.

| Command | Result |
|---|---|
| `dotnet build albums-api/albums-api.csproj` | Passed; targets `net8.0` with SDK 10.0.401. Four existing compiler warnings: unused `AlbumStateStore` and nullable warnings in `Controllers/UnsecuredController.cs`; no errors. |
| `ASPNETCORE_ENVIRONMENT=Development dotnet run --no-build --no-launch-profile --project albums-api/albums-api.csproj --urls http://127.0.0.1:3000` | Started successfully; the API responses above were captured using `curl`. |
| `cd album-viewer && npm ci && npm run type-check && npm run build` | Passed using Node v24.21.0/npm 11.19.0. Build emitted the existing Vite CJS Node API deprecation warning. `npm ci` reported 15 audit findings (3 moderate, 9 high, 3 critical); no dependency or lockfile changes were made. |
| Browser UI interaction | Not run: the browser MCP session failed with an OAuth-required error. UI expectations are explicitly source-derived in `ui-acceptance.md`. |
| Azure deployment/database/Dapr checks | Not run; this baseline does not provision cloud resources, and Dapr is not installed. |

The Vue install/build generated ignored `node_modules/` and `dist/` only. The
tracked Vue lockfile and original API/frontend sources remained unchanged.

An unrelated existing `main` workflow run (`Build and Deploy`, run
`28087980586`) also failed in `package-services (album-api, ./album-api)`.
GitHub Actions returned HTTP 410 when its job logs were requested, so its failure
details could not be inspected or attributed to this migration.

## Tool and environment inventory

Observed in this capture container (Ubuntu 24.04.5 LTS), not the issue's Windows
baseline:

| Tool | Observed version/status | Migration implication |
|---|---|---|
| Java/Javac | OpenJDK 17.0.20.1; `javac 17.0.1` | JDK 25 is required but unavailable here; no Java 25 build/runtime validation was possible. Pin/select JDK 25 in the scaffold and CI. |
| Maven | 3.9.16 installed | No Maven Wrapper exists in the original repository. Add and commit `mvnw`, `mvnw.cmd`, and wrapper configuration with the scaffold; wrapper supports Linux and Windows. |
| Node/npm | Node v24.21.0; npm 11.19.0 | Node 24.21.0 satisfies the official Angular 22 Node range. |
| Python | 3.12.3 | Standard-library-only API capture/assertion harness; no Python packages required. |
| Docker | CLI/server 28.0.4; daemon reachable on Ubuntu 24.04.5 | Container/Testcontainers execution is available, but no migration containers were built in this baseline. |
| Dapr | Executable not found | Local sidecar/service-invocation validation remains unexecuted. |
| Azure CLI | 2.90.0; an account context is configured | Subscription, selected tenant/resource group, permissions, region, budget, and deployment approvals remain unverified inputs; no Azure request was made. |
| .NET SDK | 10.0.401 on Linux | Original project targets .NET 8; build succeeded here. |

## Proposed scaffold versions

These are proposed pinned choices for the dependent scaffold step, not proof that
the scaffold or its complete dependency tree has been built. Spring Boot's BOM
should manage Flyway and Testcontainers; use a current pgJDBC patch rather than
the BOM's older point release.

| Component | Proposed choice | Compatibility evidence / boundary |
|---|---|---|
| Java | JDK 25 | Required by the migration plan. Spring Boot 4.1.1's official system requirements state Java 17 through Java 27. The local JDK is only 17, so the JDK 25 vendor/patch must be selected and verified in scaffold CI. |
| Spring Boot | 4.1.1 | Current stable release at capture; official system requirements cover Java 25 and Maven 3.6.3+. |
| Maven Wrapper | Apache Maven 3.10.0 | Current stable Maven release at capture; use the official Wrapper on both `mvnw` (POSIX/Linux) and `mvnw.cmd` (Windows). |
| PostgreSQL JDBC | `org.postgresql:postgresql:42.7.14` | pgJDBC 42.7.14 is an official release and supports Java 8+; it includes the October 2026 security fixes. Spring Boot 4.1.1's BOM manages 42.7.13, so explicitly align to 42.7.14. |
| Flyway | `org.flywaydb:flyway-core:12.4.0` and `org.flywaydb:flyway-database-postgresql:12.4.0` | Use the Spring Boot 4.1.1 BOM-managed Flyway version and matching PostgreSQL database module; verify a real PostgreSQL migration in issue #93 rather than treating this baseline as a database test. |
| Java container | `eclipse-temurin:25-jre-noble` (resolve/pin image digest in scaffold) | Linux Ubuntu Noble JRE 25 candidate; actual OCI tag/digest and build/run compatibility still require scaffold CI validation. |
| Angular / CLI / Material | 22.2.2 / 22.2.2 / 22.2.2 | Angular and Angular Material/CLI official release tags align at 22.2.2. |
| Node.js | 24.21.0 | Angular's official active-support matrix for 22.0.x accepts `^24.15.0`; 24.21.0 is within that range and is present in this container. |
| TypeScript | 6.0.3 | Angular 22 requires `>=6.0.0 <6.1.0`; 6.0.3 is the current compatible 6.0 patch available at capture. |
| RxJS | 7.8.2 | Angular 22 accepts `^7.4.0`; 7.8.2 is within that supported range. |

Pin exact npm dependencies and commit `package-lock.json` during scaffolding.
Recheck official compatibility ranges and container image digests at that step.
The Azure PostgreSQL server version/SKU/region and any cloud image registry
remain intentionally undecided pending the required environment inputs.

Official compatibility references reviewed 2026-10-09:

- [Spring Boot 4.1.1 release](https://github.com/spring-projects/spring-boot/releases/tag/v4.1.1)
- [Spring Boot system requirements](https://docs.spring.io/spring-boot/system-requirements.html)
- [Spring Boot 4.1.1 dependency-management BOM](https://repo.maven.apache.org/maven2/org/springframework/boot/spring-boot-dependencies/4.1.1/spring-boot-dependencies-4.1.1.pom)
- [Angular version compatibility matrix](https://angular.dev/reference/versions)
- [Angular 22.2.2 release](https://github.com/angular/angular/releases/tag/v22.2.2)
- [Angular Material 22.2.2 release](https://github.com/angular/components/releases/tag/v22.2.2)
- [Angular CLI 22.2.2 release](https://github.com/angular/angular-cli/releases/tag/v22.2.2)
- [pgJDBC supported PostgreSQL and Java versions](https://github.com/pgjdbc/pgjdbc#supported-postgresql-and-java-versions)
- [pgJDBC 42.7.14 release](https://github.com/pgjdbc/pgjdbc/releases/tag/REL42.7.14)
- [Maven Wrapper documentation](https://maven.apache.org/tools/wrapper/)
- [Apache Maven 3.10.0 release](https://github.com/apache/maven/releases/tag/maven-3.10.0)
- [Eclipse Temurin container images](https://github.com/adoptium/containers)
