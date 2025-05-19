GHCFLAGS='-O2'
BUILDDIR='build'
BINNAME="$BUILDDIR/hmacro"
GHCNAME='ghc'

# i should really use a makefile but this was simpler

mkdir -p $BUILDDIR
$GHCNAME $GHCFLAGS hmacro.hs -outputdir $BUILDDIR -o $BINNAME
