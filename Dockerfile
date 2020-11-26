FROM gleamlang/gleam:0.12.1
RUN apt-get update && \
    apt-get install postgresql postgresql-contrib -y
