# TODO:
# make it runable every where, (currently need to run from root of the project or cabal build will failed
# convert to nix-shell shebang or add inotify-tools to shell.nix

# watch for specific src file, run doctest when file is saved and show the first failed test

SRCFILE=$1

echo ${SRCFILE}

cabal new-build

while inotifywait -e close_write $SRCFILE; do
  clear
  doctest $SRCFILE 2>&1 | head
done
