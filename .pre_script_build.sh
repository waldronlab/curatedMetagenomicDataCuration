#!/bin/bash

R CMD build curatedMetagenomicDataCuration
./curatedMetagenomicDataCuration/.push_gh_pages.sh
rm *.tar.gz
