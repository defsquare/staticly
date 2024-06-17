#!/bin/bash

clj -e "(compile 'defsquare.staticly)"

sdk use java 21.0.3-graal

native-image \
    -cp "$(clojure -Spath):classes" \
    -H:Name=staticly \
    -H:+ReportExceptionStackTraces \
    -H:+AllowDeprecatedBuilderClassesOnImageClasspath \
    -O3 \
    --features=clj_easy.graal_build_time.InitClojureClasses \
    --verbose \
    --no-fallback \
    defsquare.staticly
