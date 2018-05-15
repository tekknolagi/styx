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
dub build --build=unittest-cov --config=compiler --compiler=${DC}
echo "unit b;" > b.sx && bin/styx -v -p -a -t b.sx <<< "unit a;"

# release build
dub build --build=release --config=compiler --force --compiler=${DC}
dub build --build=release --config=library --force --compiler=${DC}

# file-based tests
dub build --build=cov --config=compiler --force --compiler=${DC}
cd tests
$DC test.d
./test
