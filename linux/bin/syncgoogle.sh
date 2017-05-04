#!/bin/bash

# customize these
WGET=/usr/bin/wget
ICS2ORG=/home/dan/bin/ical2org.awk
ICSFILE=/tmp/google.ics
ORGFILE=/home/dan/pers/plan/GoogleCal.org
URL=https://calendar.google.com/calendar/ical/dan.pomohaci%40gmail.com/private-08382799d0f37c8f744b774f0e697843/basic.ics

# no customization needed below

$WGET -O $ICSFILE $URL
$ICS2ORG < $ICSFILE > $ORGFILE
