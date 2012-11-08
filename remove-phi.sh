#!/bin/bash

for i in *.csv
do
  echo $i
  head -n 10 $i
  echo Kill this file?
  read killornot

  if [ "$killornot" = "yes" ]
  then
   echo Removing!
   rm $i
  fi 
done

