#!/bin/bash
set -xe
bnfc Quartz.bnfc -p Quartz.Syntax
cd Quartz/Syntax/
alex LexQuartz.x
happy ParQuartz.y
rm LexQuartz.x
rm ParQuartz.y
