#! /usr/bin/env bash

if [ -z "$BUILD_SOURCEBRANCH" ]
then
    echo "Initialising local branch state"
    BUILD_SOURCEBRANCH=`git branch --show-current`
fi

if [ -z "$BUILD_CONFIGURATION" ]
then
    echo "Setting configuration to Release"
    BUILD_CONFIGURATION="Release"
fi

set -euxo pipefail

# Restore global tools for use later in the build
dotnet tool restore

# Work out the version number for this build
export $(dotnet tool run octoversion --CurrentBranch=${BUILD_SOURCEBRANCH} --OutputFormats:0=Environment | grep -v '^\[' | xargs)
versionFlags="/P:Version=${OCTOVERSION_NuGetVersion} /P:InformationalVersion=${OCTOVERSION_InformationalVersion}"

# Build, test, and pack the libraries
dotnet build --configuration ${BUILD_CONFIGURATION} $versionFlags
dotnet test --no-build --configuration ${BUILD_CONFIGURATION} $versionFlags --logger 'trx' --logger 'console;verbosity=normal'
dotnet pack --no-build --configuration ${BUILD_CONFIGURATION} $versionFlags --output=./artifacts

# Check code format. Fantomas for F#, dotnet-format for C#
dotnet tool run fantomas --check --recurse .
dotnet tool run dotnet-format --check