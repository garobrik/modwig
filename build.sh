#!/bin/sh

SCRIPT_DIR=$(dirname $(readlink -f "$0"))

cd $SCRIPT_DIR/web
npm run build
mkdir -p $SCRIPT_DIR/src/main/resources/web
cp -r $SCRIPT_DIR/web/build/* $SCRIPT_DIR/src/main/resources/web/
cd $SCRIPT_DIR
mvn package
