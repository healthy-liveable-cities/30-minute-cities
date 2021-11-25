#!/bin/bash
cd "$(dirname "$0")"

# just hardcoded this but could read from directory
COMPOUND_REGIONS=($(seq 1 1 13))

# convert the osm files to sqlite, keeping only the roads
for i in ${COMPOUND_REGIONS[@]}; do
  ogr2ogr -f "SQLite" -dsco SPATIALITE=YES -dialect SQLite -overwrite -sql     \
  "SELECT osm_id, GEOMETRY FROM lines where highway NOT NULL AND highway NOT   \
     in ('pedestrian', 'footway', 'cycleway','path','steps', 'motorway',       \
     'motorway_link', 'living_street', 'proposed', 'track', 'service')"        \
  "./data-input/osm_data/30minute_cities_"${i}".sqlite"                        \
  "./data-output/osm_data/30minute_cities_"${i}".osm"
done
