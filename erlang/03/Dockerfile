FROM erlang
ADD . /usr/dist
WORKDIR /usr/dist
ENTRYPOINT ["erl", "-noshell", "-setcookie", "kolacic", "-pa", "/usr/dist/out/production/Distributed"]
