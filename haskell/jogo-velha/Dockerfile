FROM fpco/stack-build

WORKDIR /app

COPY . .

RUN stack build

CMD ["stack", "exec", "jogo-velha-exe"]
