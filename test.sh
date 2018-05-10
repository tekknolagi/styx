#!usr/bin/bash
set -e

################### ENVIRONMENT ###################
# CI w/o D by default or local tests
if [ -z $DC ]; then
	DC=dmd
fi;

###################### TESTS ######################
# grammar
cd grammar && dub run --config=generate && dub run --config=test
cd ..

# in-source unit tests + cover main appl piping features
if [ -f bin/yatol ]; then rm bin/yatol; fi
dub build --build=unittest-cov --config=compiler --compiler=${DC}
echo "unit b;" > b.ya && bin/yatol -v -p -a -t b.ya <<< "unit a;"
rm bin/yatol

# release build
dub --build=release --config=compiler --compiler=${DC}
rm bin/yatol
dub build --build=release --config=library --compiler=${DC}

# file-based tests
dub --build=cov --config=compiler --compiler=${DC}
cd tests
$DC test.d
./test
