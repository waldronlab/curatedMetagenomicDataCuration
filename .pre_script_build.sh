#!/bin/bash

cd ..
R CMD build curatedMetagenomicDataCuration
cd curatedMetagenomicDataCuration/
./.push_gh_pages.sh
rm *.tar.gz
