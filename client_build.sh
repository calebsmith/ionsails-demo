mkdir -p build
rm -rf build/*
lein clean
npx shadow-cljs release app
cp -R resources/public/* build
