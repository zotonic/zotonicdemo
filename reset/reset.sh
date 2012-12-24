#!/bin/bash

cd `dirname $0`

set -e
set -v

DEMO="/home/zotonic/zotonicdemo"
PSQL="sudo -u postgres psql zotonicdemo"

/etc/init.d/zotonic stop

/usr/bin/sudo -u zotonic -i $DEMO/reset/update.sh

echo "DROP SCHEMA public CASCADE" | $PSQL
echo "CREATE SCHEMA public AUTHORIZATION zotonic" | $PSQL
$PSQL < $DEMO/reset/demo.sql

rm -rf $DEMO/files/*/*

/etc/init.d/zotonic start


