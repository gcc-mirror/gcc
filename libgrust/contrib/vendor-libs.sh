#!/bin/sh

# Copyright (C) 2026 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

set -e

SCRIPT_DIR="$(dirname "$0")"

VENDOR_VERSION=1.49.0
VENDOR_LIBS="core alloc stdarch"

# double check, rm -rf needs to be safe
if [ x"${SCRIPT_DIR}" = x ]; then
    echo "failed to get script directory" >&2
    exit 2
fi

VENDOR_DIR="${SCRIPT_DIR}"/../rustc-lib

rm -rf "${VENDOR_DIR}" && mkdir -p "${VENDOR_DIR}"

TEMP_GIT_DIR="$(mktemp -d)"

# double check this too
if [ x"${TEMP_GIT_DIR}" = x ]; then
    echo "failed to get temporary directory" >&2
    exit 2
fi

rm_git_dir() {
    rm -rf "${TEMP_GIT_DIR}"
}

trap rm_git_dir EXIT INT TERM HUP

git clone \
    --recurse-submodules \
    --filter=blob:none \
    --also-filter-submodules \
    --depth=1 \
    --shallow-submodules \
    --branch="${VENDOR_VERSION}" \
    "git@github.com:rust-lang/rust.git" \
    "${TEMP_GIT_DIR}"

find "${TEMP_GIT_DIR}" -name .git -exec rm -rf {} +

for FILENAME in LICENSE-MIT LICENSE-APACHE; do
    cp "${TEMP_GIT_DIR}/${FILENAME}" "${VENDOR_DIR}/${FILENAME}"
done

for VENDOR_LIB in $VENDOR_LIBS; do
    cp -R \
        "${TEMP_GIT_DIR}"/library/"${VENDOR_LIB}" \
	"${VENDOR_DIR}/${VENDOR_LIB}"
    echo "lib${VENDOR_LIB}_tag=${VENDOR_VERSION}" \
        | tr '[:lower:]' '[:upper:]' \
	>> "${VENDOR_DIR}/version-info"
done
