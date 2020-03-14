FROM library/ubuntu:bionic

# docker build . -t registry.gitlab.com/tseenshe/hsinspect:8.8.3
# docker login --username=tseenshe registry.gitlab.com
# docker push registry.gitlab.com/tseenshe/hsinspect:8.8.3

ENV PATH "/root/.cabal/bin:/opt/ghc/bin:$PATH"
RUN apt-get update &&\
    apt-get install -y software-properties-common &&\
    add-apt-repository -y ppa:hvr/ghc &&\
    apt-get install -y git python cabal-install-3.0 ghc-8.6.5 ghc-8.8.3 &&\
    rm -rf /var/lib/apt/lists/*
