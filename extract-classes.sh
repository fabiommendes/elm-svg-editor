#!/bin/sh
npx tailwindcss -i src/css/main.css -o .main-tmp.css -c tailwind.config.js
node extract-selectors.js
rm .main-tmp.css