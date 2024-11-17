# cache dependencies to avoid needing to rebuild.
# I may live to regret this.
FROM ghcr.io/stefan-hoeck/idris2-pack:latest
WORKDIR /root/
COPY tui.ipkg .
COPY pack.toml .
COPY src       src
COPY examples  examples
RUN pack build
RUN pack build gallery
RUN pack build scrollclip
RUN pack build todo
RUN pack build user_event
