#!/bin/bash
# Android build helper

main() {
    local source_root=$1
    local lunch_target=$2
    local module_name=$3

    pushd $source_root
    source build/envsetup.sh
    lunch $lunch_target

    local build_out=$(get_build_var PRODUCT_OUT)
    installed_files=$(eval jq \'.[\"$module_name\"]\' $build_out/module-info.json | jq -r -M '.installed | .[]')

    adb root && adb remount && adb wait-for-device

    for f in $installed_files; do
        local_file=$f
        remote_file=${f#$build_out}

        echo "adb push $local_file $remote_file"
        adb push $local_file $remote_file
    done

    popd
}

main $@
