FROM rocker/shiny:4.5.0

RUN apt-get update -y && apt-get install -y \
    make libcurl4-openssl-dev cmake libuv1-dev libssl-dev libssh2-1-dev \
    pandoc zlib1g-dev git libsndfile1-dev libicu-dev \
    python3 python3-pip python3-venv ffmpeg \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" \
    | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages("pak")' && \
    R -e 'pak::pak("renv@1.1.5")'

# WORKDIR matches the Shiny Server app path so renv builds its library here
WORKDIR /srv/shiny-server/measure-graphics/

COPY renv.lock renv.lock
RUN --mount=type=cache,id=renv-cache,target=/root/.cache/R/renv \
    R -e 'options(renv.config.pak.enabled = FALSE); renv::restore()'

COPY . /srv/shiny-server/measure-graphics/

# Build the Python venv at the path whisper-transcribe expects
RUN python3 -m venv /srv/shiny-server/measure-graphics/venv && \
    /srv/shiny-server/measure-graphics/venv/bin/pip install --upgrade pip && \
    /srv/shiny-server/measure-graphics/venv/bin/pip install openai-whisper setuptools-rust

RUN mkdir -p /srv/shiny-server/measure-graphics/data/recordings \
             /srv/shiny-server/measure-graphics/data/transcripts \
             /var/log/shiny-server && \
    chown -R shiny:shiny /srv/shiny-server/measure-graphics/data \
                         /var/log/shiny-server

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838
