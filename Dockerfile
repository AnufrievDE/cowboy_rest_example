FROM erlang:latest

RUN mkdir /cowboy_rest_example
COPY . /cowboy_rest_example

WORKDIR /cowboy_rest_example
RUN rebar3 as prod release

FROM erlang:latest
RUN mkdir /cowboy_rest_example
WORKDIR /cowboy_rest_example
COPY --from=0 /cowboy_rest_example/_build/prod/rel/cowboy_rest_example .

ENTRYPOINT ["bin/cowboy_rest_example"]
CMD ["foreground"]
