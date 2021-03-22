[![Build Status](https://travis-ci.org/flowcommerce/lib-util.svg?branch=primary)](https://travis-ci.com/flowcommerce/lib-util)

# lib-util

A library that contains a bunch of Scala utility classes.

## Publishing a new version

    go run release.go

## Publishing a new snapshot for local development

    edit build.sbt and append -SNAPSHOT to version
    sbt +publishLocal
