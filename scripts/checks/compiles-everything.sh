#!/bin/bash

set -euo pipefail

ROOT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")/../..

cd $ROOT_DIRECTORY

elm make src/Algorithm.elm src/AUF.elm src/Cube.elm src/PLL.elm src/Cube/Advanced.elm --output=/dev/null
