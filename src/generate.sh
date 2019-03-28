#!/bin/bash
set -xe
bnfc -p Quartz.Syntax Quartz.cf
cd Quartz/Syntax/
alex LexQuartz.x
happy -i ParQuartz.y
rm LexQuartz.x
rm ParQuartz.y
rm TestQuartz.hs
