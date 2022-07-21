#!/usr/bin/env bash

_error() {
    echo "$@" 1>&2
}

_usage() {
    _error "Usage: $0 -n <VERSION>"
    exit 1
}

_VERSION_NEXT=""

while getopts ":n:" o; do
    case "${o}" in
    n)
        _VERSION_NEXT="${OPTARG}"
        ;;
    *)
        _usage
        ;;
    esac
done
shift $((OPTIND - 1))


if [ -z "${_VERSION_NEXT}" ]; then
    _usage
fi

## Update version:
sed -i -E "s/^version:([ ]+).*/version:\\1${_VERSION_NEXT}/g" package.yaml
hpack

## Update CHANGELOG.md:
git-chglog -o CHANGELOG.md --next-tag "${_VERSION_NEXT}"

## Add files:
git add package.yaml zamazingo.cabal CHANGELOG.md

## Commit:
git commit -m "chore(release): ${_VERSION_NEXT}"

## Tag:
git tag -a -m "Release ${_VERSION_NEXT}" "${_VERSION_NEXT}"

## Push:
git push --follow-tags origin main

## Release
gh release create "${_VERSION_NEXT}" --title "v${_VERSION_NEXT}" --generate-notes
