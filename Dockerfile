FROM rocker/shiny:latest

# Instala los paquetes necesarios
RUN R -e "install.packages(c('shiny', ggplot2','dplyr', 'tidyr', 'ggplot2','lubridate','readr','writexl','readxl','purrr), repos='https://cloud.r-project.org')"

# Copia la app en el contenedor
COPY . /srv/shiny-server/

# Permisos
RUN chown -R shiny:shiny /srv/shiny-server

# Exponer el puerto (Shiny corre en el 3838)
EXPOSE 3838

# Comando por defecto
CMD ["/usr/bin/shiny-server"]
