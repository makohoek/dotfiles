#!/bin/bash
# Android build helper

main() {
    local source_root=$1
    local lunch_target=$2
    local module_name=$3

    pushd $source_root
    source build/envsetup.sh
    lunch $lunch_target
    m $module_name
    popd
}

main $@
