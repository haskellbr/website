#!/bin/bash
mkdir -p to-s3
cp -r index.html static to-s3/
aws s3 sync to-s3/ s3://haskellbr.com/
