#!/bin/bash

clj -Mplantuml -e "(compile 'defsquare.plantuml)"

native-image \
    -cp "$(clojure -Spath -Aplantuml):classes" \
    -H:Name=plantuml-watcher \
    -H:+ReportExceptionStackTraces \
    -H:+AllowDeprecatedBuilderClassesOnImageClasspath \
    -H:IncludeResources=".*/plantuml.skin$" \
    -H:+BuildReport \
    -O3 \
    --features=clj_easy.graal_build_time.InitClojureClasses \
    --verbose \
    --no-fallback \
    defsquare.plantuml
