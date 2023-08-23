default:
    just list

install-tools:
    cabal update
    cabal install fourmolu-0.13.1.0 hlint-3.6.1 apply-refact \
        --install-method=copy --overwrite-policy=always

format:
    fourmolu -i lib

lint:
    hlint lib 

build0:
    cabal build -v0 -O0 -j fine-types

test:
    cabal test -v0 -O0 -j unit

repl: 
    cabal repl -v0 -O0 -j fine-types

prepare: format lint build0 test
