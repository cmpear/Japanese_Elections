FROM continuumio/miniconda3:4.5.11

RUN apt-get update -y; apt-get upgrade -y; \
    apt-get install -y vim-tiny vim-athena ssh \
    build-essential gcc gfortran g++

# Always save your environments in a conda env file. 
# This makes it so much easier to fix your environment when you inadvertantly clobber it
# COPY (Relative to project) (/root)

COPY environment.yml environment.yml
RUN conda env create -f environment.yml
RUN echo "alias l='ls -lah'" >> ~/.bashrc

# This is the conda magic. If you are running through a shell just activating the environment in your profile is peachy
RUN echo "source activate r-shiny" >> ~/.bashrc

# This is the equivalent of running `source activate`
# Its handy to have in case you want to run additional commands in the Dockerfile
# env > before_activate.txt
# source activate r-shiny
# env > after_activate.txt
# diff before_activate.txt after_activate.txt

ENV CONDA_EXE /opt/conda/bin/conda
ENV CONDA_PREFIX /opt/conda/envs/r-shiny
ENV CONDA_PYTHON_EXE /opt/conda/bin/python
ENV CONDA_PROMPT_MODIFIER (r-shiny)
ENV CONDA_DEFAULT_ENV r-shiny
ENV PATH /opt/conda/envs/r-shiny/bin:/opt/conda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# This is how we install custom R packages
RUN R -e "install.packages('devtools', repos = 'http://cran.us.r-project.org')"
#RUN R -e "install.packages('shiny', repos = 'http://cran.us.r-project.org')"
#RUN R -e "install.packages('shinythemes', repos = 'http://cran.us.r-project.org')"
#RUN R -e "install.packages('rgdal', repos = 'http://cran.us.r-project.org',configure.args=c('--with-gdal-config=/Library/Frameworks/GDAL.framework/Programs/gdal-config','--with-proj-include=/Library/Frameworks/PROJ.framework/Headers','--with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib'))"
# Copy our app.R (or the entire project)
COPY Japanese_Elections.Rproj ./
COPY app.R ./
COPY R ./R
#COPY R/data ./R/data
#COPY R/data/jpmap ./R/data/jpmap
# replaced file paths with github paths to save space...also moved data out of R

# Add shiny user
RUN groupadd  shiny \
&& useradd --gid shiny --shell /bin/bash --create-home shiny

CMD ["/bin/bash", "-c", "./app.R"]