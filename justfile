set positional-arguments

seed := "0"

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
    cabal build -v0 -O0 -j all

test:
    @cabal test -v0 -O0 all \
        --test-show-details=direct \
        --test-options="--format=checks --color"
    @cabal test -v0 -O0 haskell \
        --test-show-details=direct \
        --test-options="--format=checks --color"

test-autogen:
    @cabal test -v0 -O0 -j haskell \
        --test-show-details=direct \
        --test-options="--format=checks --no-color"



test-seed:
    @cabal test -v0 -O0 -j unit \
        --test-show-details=direct \
        --test-options="--format=checks --no-color" \
        --test-options="--seed {{seed}}"

repl:
    cabal repl -v0 -O0 -j fine-types

prepare: format lint build0 test


@run *args='--help':
    just build0
    cabal exec -v0 -O0 -j fine-types -- $@

pushf:
    git push -f origin HEAD

push:
    git push origin HEAD
