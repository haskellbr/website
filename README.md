# haskellbr-website
[![Build Status](https://travis-ci.org/haskellbr/website.svg?branch=master)](https://travis-ci.org/haskellbr/website)
- - -
Code for haskellbr.com.

## Usage
Generate the static assets with
```
$ stack exec haskellbr-website
```

Push them to S3.

## Continous integration
Travis CI is configured to automatically push the `master` branch to
http://haskellbr.com.s3-website-sa-east-1.amazonaws.com

## License
All work under this repository is licensed under the MIT license.
