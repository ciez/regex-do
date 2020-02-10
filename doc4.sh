# cabal haddock --builddir="dist-doc" --for-hackage --haddock-option=--hyperlinked-source
# if [ $# -eq 0 ] ; then
# 	echo './dist-doc/*-docs.tar.gz'
# 	exit 1;
# fi
cabal upload -d dist-doc/$1-docs.tar.gz
