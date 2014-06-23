#!/bin/bash

echo 'Use Ctrl-Z to stop server'
R -e "shiny::runApp('/var/data/ohi/usr/local/src/toolbox/code/inspector')"