FROM gleamlang/gleam:0.11.0-rc1
RUN apt-get update && \
    apt-get install postgresql postgresql-contrib -y
