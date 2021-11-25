#!/bin/bash
cd "$(dirname "$0")"

INPUT_DIR="./data-output/OTP_raw"
OUTPUT_DIR="./data-output/OTP_cleaned"
NAME_CRITERIA="sa1_dzn_region*_2019_0745_max_3hrs.db"
TABLE_NAME="od_modes_0745"

# NAME_CRITERIA="sa1_dzn_region*_2019_1045_max_3hrs.db"
# TABLE_NAME="od_modes_1045"

DB_LIST=(`find ${INPUT_DIR} -name ${NAME_CRITERIA} | sort -n`)
# DB_LIST=(`find -name sa1_dzn_region*_2019_0745_max_3hrs.db | sort -n`)
# echo "${DB_LIST[@]}"
# basename ${DB_LIST[@]}



for DB in ${DB_LIST[@]}; do
  # DB=${DB_LIST[3]} # region 4 is the smallest DB
  DB_OLD_NAME=`basename ${DB}`
  DB_NEW_NAME=${DB_OLD_NAME/sa1_dzn_}
  DB_NEW_NAME=${DB_NEW_NAME/_2019_0745_max_3hrs}
  DB_NEW_NAME_PATH="$(realpath ${OUTPUT_DIR}/${DB_NEW_NAME})"
#  DB_NAME=${DB_NAME:8:8}.db
  DB_FULLPATH="$(realpath $DB)"
  # echo "${DB_FULLPATH}"
  # echo "${DB_NEW_NAME_PATH}"
  sqlite3 "${DB_NEW_NAME_PATH}" "ATTACH \"${DB_FULLPATH}\" AS \`a\`;
    CREATE TABLE ${TABLE_NAME} AS
    SELECT
      CAST(origin AS integer) AS origin,
      CAST(destination AS integer) AS destination,
      CAST(dist_m AS integer) AS dist_m,
      CAST(CASE mode
        WHEN '\"WALK\"' THEN '1'
        WHEN '\"BICYCLE\"' THEN '2'
        WHEN '\"CAR\"' THEN '3'
        WHEN '\"WALK,TRANSIT\"' THEN '4'
      END AS integer) AS mode,
      CAST(CAST(time_mins AS FLOAT)*60 AS INTEGER) AS time_secs
    from a.${TABLE_NAME};
    CREATE INDEX idx_${TABLE_NAME} ON ${TABLE_NAME} (destination);"
  echo "${DB_NEW_NAME} complete"
done
