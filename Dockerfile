FROM debian:jessie

MAINTAINER Eduardo Trujillo <ed@chromabits.com>

RUN apt-get update \
  && apt-get install -y libcurl4-gnutls-dev libgmp-dev \
  && apt-get clean

COPY dist/shift /usr/local/bin/shift

ENTRYPOINT ["/usr/local/bin/shift"]
