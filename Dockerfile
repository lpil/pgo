FROM gleamlang/gleam:0.10.1
RUN apt-get update && \
    apt-get install postgresql postgresql-contrib -y
