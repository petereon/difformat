fmt:
    ormolu --mode inplace $(find src app test -name '*.hs')
    hlint $(find src app test -name '*.hs')
    cabal-fmt *.cabal -i

test:
    cabal test

watch target:
    watchexec -e cabal,hs -c -r "just {{target}}"