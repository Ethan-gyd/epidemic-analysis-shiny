# 自托管示例。需要联网构建；该配置尚需在目标服务器完成验收。
FROM rocker/r-ver:4.5.3
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
    libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libpng-dev libtiff5-dev libjpeg-dev cmake \
    libreoffice-writer fonts-noto-cjk fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /srv/surveillance
COPY scripts/install_dependencies.R scripts/install_dependencies.R
RUN Rscript scripts/install_dependencies.R
COPY app.R data_preprocessing.R outbreak_detection.R report_generation.R report_analysis.R report_module.R disease_dictionary.csv ./
COPY data/demo_surveillance.csv data/demo_surveillance.csv
RUN useradd --create-home --uid 10001 surveillance && chown -R surveillance:surveillance /srv/surveillance
USER surveillance
ENV TZ=Asia/Shanghai
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
