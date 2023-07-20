FROM haskell:latest

RUN apt update
RUN apt install -y postgresql libpq-dev

VOLUME /root/.stack

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy the local project files to the container
COPY . .

# RUN stack setup
# RUN stack build

CMD ["bash"]

EXPOSE 8080
