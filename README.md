# haskellbr-website
Code for haskellbr.com.

## Deployment
Initially this was deployed on a Machine running Ubuntu by running
`make deploy`. This would:

* Build the Haskell binary in a docker container with `stack`
* Compress it with `upx`
* Compress all required files for the binary to work with `tar + gzip`
* Copy everything over to a server
* Restart a service which was configured there

- - -

The on-going deployment story intends to:
* Build a ~~dinamically-linked~~ binary on a docker container
* Compress the binary
* ~~Use `dockerize` or the `haskell-scratch` image to build a minimal docker image~~
* Deploy that anywhere

## License
All work under this repository is licensed under the MIT license.
