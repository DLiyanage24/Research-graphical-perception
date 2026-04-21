FROM rocker/geospatial:4.5.0
RUN apt-get update -y && apt-get install -y  make libcurl4-openssl-dev cmake libuv1-dev libssl-dev libssh2-1-dev pandoc zlib1g-dev git libsndfile1-dev libicu-dev && rm -rf /var/lib/apt/lists/*
RUN apt install python3-venv python3-pip python3-pipdeptree python3-pip-whl && python3 -m venv /venv && source venv/bin/activate && apt install ffmpeg & pip install -U openai-whisper setuptools-rust
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("pak")'
RUN R -e 'pak::pak("renv", version = "1.0.3")'
COPY renv.lock renv.lock
RUN --mount=type=cache,id=renv-cache,target=/root/.cache/R/renv R -e 'renv::restore()'
RUN --mount=target=/srv/shiny-server,source=/server/shiny/apps,rw
RUN --mount=target=/var/log/shiny-server,source=/server/shiny/logs,rw
WORKDIR /srv/shiny-server/
COPY . /srv/shiny-server/
EXPOSE 3840
CMD R -e 'shiny::runApp("/srv/shiny-server",host="0.0.0.0",port=3840)'
