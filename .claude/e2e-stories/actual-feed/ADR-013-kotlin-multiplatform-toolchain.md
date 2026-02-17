# ADR-013: Kotlin Multiplatform Toolchain with Tauri Desktop Shell

## Status
Accepted

## Context
The core engine (crawling, LLM extraction, ranking, schema management, sync protocol, maturity filtering) runs on the device — not on a server. It must run on desktop (macOS, Windows, Linux), Android, and iOS. The engine is thousands of lines of non-trivial business logic. Maintaining two implementations (Java + Swift) doubles every feature, bug fix, and schema migration indefinitely.

The desktop app embeds platform content (YouTube IFrame Player API, Twitter oEmbed, Stack Overflow HTML). This requires a web rendering context.

Candidates considered:
- **Pure Java + Swift iOS rewrite**: Java runs on JVM (desktop + Android). iOS requires a full engine rewrite in Swift. Two codebases maintained forever. Rejected — the cost compounds over every release.
- **Kotlin Multiplatform (KMP)**: Kotlin compiles to JVM bytecode (desktop + Android, identical Java runtime, all Java libraries accessible) AND to native ARM64/x86_64 via Kotlin/Native + LLVM (iOS). One engine codebase, three platforms. UI stays platform-native (Swift on iOS, Kotlin Compose on Android, web on desktop).
- **Rust core + FFI**: Cross-platform via compilation. But: Kotlin/JVM interop is JNI (same as C), iOS interop is C FFI, Android interop is JNI. Loses JVM ecosystem for the engine. Harder to hire. Rejected.
- **Flutter/Dart**: Cross-platform UI + logic. But: Dart ecosystem is thin for JNI to llama.cpp/whisper.cpp. No existing SQLite tooling comparable to SQLDelight. Weak server-side story for cloud services. Rejected.

For the desktop UI:
- **Compose Multiplatform (Kotlin)**: JetBrains desktop UI framework. Renders via Skia. Would share Kotlin stack with engine. But: platform embeds (YouTube IFrame, Twitter oEmbed) require WebView shims inside Compose — additional complexity for every embed type. The feed IS rich web content.
- **Electron**: Bundles Chromium. ~150MB overhead. Proven (VS Code, Slack). But: heavy, resource-hungry alongside local LLM inference.
- **Tauri + web frontend**: System webview (WebKit/WebView2/WebKitGTK). ~5MB shell. Platform embeds work natively in web context. Frontend in any web framework.
- **Solid (web framework)**: Fine-grained reactivity without virtual DOM. When one feed item's pipeline_state changes from `raw` → `enriched`, Solid updates that DOM node. React would diff the entire feed list. For a data-heavy feed rendering hundreds of items with real-time status transitions, Solid produces fewer DOM mutations per state change.

## Decision

### Language & Compilation
Kotlin with Kotlin Multiplatform (KMP). Shared engine module compiles to:
- **JVM bytecode** on desktop and Android (Java 21+ target, virtual threads for crawling concurrency)
- **Native binary** on iOS (Kotlin/Native via LLVM, ARM64)

All Java libraries are accessible from Kotlin/JVM. The JVM runtime experience is identical to pure Java. Kotlin adds: iOS compilation, null safety, coroutines, sealed classes, and multiplatform declarations (expect/actual).

### Project Structure
```
actual-feed/
  shared/                    # KMP shared engine
    src/
      commonMain/            # Platform-agnostic: ranking, schema, sync, extraction
      jvmMain/               # JVM: sqlite-jdbc, JNI bridges, virtual threads
      iosMain/               # iOS: native SQLite, cinterop bridges
  desktop/                   # Tauri shell + Solid frontend
    src-tauri/               # Tauri config, Rust glue (minimal)
    src/                     # Solid + TypeScript UI
  android/                   # Android app (Kotlin + Jetpack Compose)
  ios/                       # iOS app (Swift + SwiftUI)
  emacs/                     # actual-feed.el (Elisp JSON-RPC client)
  cloud/                     # Cloud services (Kotlin/JVM: auth, sync relay, crawler)
```

### Core Dependencies

| Component | Library | Platform |
|---|---|---|
| Build system | Gradle + Kotlin DSL | All |
| SQLite | SQLDelight | All (generates Kotlin from SQL, multiplatform) |
| LLM inference | llama.cpp | JVM: JNI bridge. iOS: cinterop. |
| Speech-to-text | whisper.cpp | JVM: JNI bridge. iOS: cinterop. |
| Embeddings | ONNX Runtime | JVM: onnxruntime-java. iOS: onnxruntime-objc via cinterop. |
| Neural taste model | ONNX Runtime + DJL | JVM: training + inference. iOS: inference only (train on JVM, export ONNX). |
| HTTP server | Ktor Server | JVM (desktop: serves JSON-RPC to Tauri/Emacs) |
| HTTP client | Ktor Client | All (multiplatform, used by crawlers) |
| JSON | kotlinx.serialization | All (compile-time, no reflection, multiplatform) |
| Async | Kotlin Coroutines | All (multiplatform, structured concurrency) |
| Desktop shell | Tauri 2.x | Desktop (system webview, ~5MB) |
| Desktop UI | SolidJS + TypeScript | Desktop (fine-grained reactivity, no virtual DOM) |
| Android UI | Jetpack Compose | Android |
| iOS UI | SwiftUI | iOS |

### Platform-Specific Notes

**Desktop (JVM)**: The Kotlin/JVM engine runs as a background process. Ktor serves JSON-RPC on a Unix domain socket (Linux/macOS) or localhost HTTP (Windows). Tauri connects to it. Emacs connects to it. Java 21+ virtual threads handle concurrent crawling — one virtual thread per source adapter, thousands concurrent, minimal memory overhead.

**Android**: The shared engine is an Android Service. Jetpack Compose UI calls engine functions in-process via Kotlin — no JSON-RPC hop needed on Android. llama.cpp and whisper.cpp via JNI, same as desktop.

**iOS**: The shared engine compiles to a native `.framework` via Kotlin/Native. Swift/SwiftUI imports it as a dependency. llama.cpp and whisper.cpp via Kotlin/Native cinterop (calling C directly). SQLite via native iOS driver (SQLDelight generates platform-specific driver code). Neural taste model: inference only via ONNX Runtime iOS — training happens on a JVM device, deterministically from the merged interaction log (per ADR-007: weights are NOT synced, each device retrains from the same interactions). iOS retrains from its local copy of the synced interaction log, producing an identical model. No ONNX file transfer needed.

**Cloud**: Kotlin/JVM. Same shared engine code for the cloud crawler (it's a virtual device in the sync protocol). Ktor server for auth and sync relay endpoints. Deploys as a standard JVM service.

### SQLDelight over sqlite-jdbc

SQLDelight generates type-safe Kotlin data classes and query functions from `.sq` SQL files. The same SQL schema definition produces:
- `JdbcSqliteDriver` on JVM (wraps sqlite-jdbc)
- `NativeSqliteDriver` on iOS (wraps native SQLite)

Schema migrations are `.sqm` files — versioned, diffable, applied automatically. No raw SQL strings scattered through the codebase. The schema defined in story 005 becomes a set of `.sq` files that compile to multiplatform Kotlin.

### Why Not Pure Java

Java compiles to JVM bytecode only. The core engine cannot run on iOS without a JVM. GraalVM native-image has experimental iOS support but is not production-ready for mobile targets as of February 2026. Maintaining a parallel Swift implementation of the engine is an unbounded ongoing cost that grows with every feature.

Kotlin on JVM IS Java at runtime. All Java libraries work. The JVM is the JVM. The only difference is the source language — and Kotlin's source language also compiles to iOS.

## Consequences
- **Positive**: One engine codebase for three platforms. All Java libraries accessible on JVM. iOS gets full local engine without a rewrite. Platform-native UI everywhere (SwiftUI, Jetpack Compose, Solid). Platform embeds work natively in Tauri's web context. SQLDelight provides type-safe multiplatform SQL.
- **Negative**: KMP adds build complexity (Gradle multiplatform configuration). Kotlin/Native is slower than JVM (no JIT — AOT compiled). iOS debugging is harder than pure Swift. Two UI languages (TypeScript for desktop, Swift for iOS). Engineers must know Kotlin, not just Java.

## Constraints Implemented
- EC-070: Emacs client uses the same JSON-RPC API as GUI clients
- OC-021: Neural taste model on JVM using ONNX Runtime + DJL
- OC-040: SQLite as storage engine across all platforms (via SQLDelight)
- OC-043: UUIDv7 for item IDs (generated in shared Kotlin code)
