#!/bin/sh

BLENDER_DIR="$HOME/Library/Application Support/Blender/2.78"

mkdir -p "$BLENDER_DIR/scripts/startup/"
ln -s "`pwd`/compat/blender/" "$BLENDER_DIR/scripts/startup/yahr"
