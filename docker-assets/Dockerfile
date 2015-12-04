FROM ubuntu:14.04

RUN apt-get update
RUN apt-get install -y libpq-dev

ADD . /haskellbr-website/
WORKDIR /haskellbr-website

CMD ./dist/bin/haskellbr-website
