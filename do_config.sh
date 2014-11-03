export CFLAGS='-O3 -V -DSQLITE_MAX_MMAPSIZE=1048576 -qTARG=zOSV1R12 '
export CFLAGS="${CFLAGS} -qLANG=EXTC99 -qFLOAT=IEEE -qnolist -qnosource "
export CFLAGS="${CFLAGS} -D_POSIX_C_SOURCE=200112L -D_XOPEN_SOURCE=600"
export CFLAGS="${CFLAGS} -DSQLITE_ENABLE_COLUMN_METADATA"
export CFLAGS="${CFLAGS} -DSQLITE_ENABLE_SQLLOG"
export CFLAGS="${CFLAGS} -DHAVE_POSIX_FALLOCATE=0"
export CFLAGS="${CFLAGS} -DSQLITE_FILE_HEADER='\"\\\\\\\\123\\\\\\\\121\\\\\\\\114\\\\\\\\151\\\\\\\\164\\\\\\\\145\\\\\\\\040\\\\\\\\146\\\\\\\\157\\\\\\\\162\\\\\\\\155\\\\\\\\141\\\\\\\\164\\\\\\\\040\\\\\\\\063\\\\\\\\000\"'"
# The above is the octal encoding for the phrase "SQLite format 3\0" in ASCII.
# This is the first phase of being compatible with the ASCII version of the sqlite data base file.
# Each octal value requires eight \ in front of it due to multiple shell evaluations. When presented
# to the compiler, it will look like a single \ as it really should.
./configure CC=xlc CPP="xlc -E" CXX=xlc++ CXXPP="xlc++ -E" --prefix=$PWD/sqlite-run
