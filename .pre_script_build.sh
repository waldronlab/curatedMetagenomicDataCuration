#!/bin/bash

R CMD build curatedMetagenomicDataCuration
./.push_gh_pages.sh
rm *.tar.gz
