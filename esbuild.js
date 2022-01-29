require('esbuild').build({
  entryPoints: ['entry.js'],
  bundle: true,
  outfile: 'public/index.js',
  minify: true,
  sourcemap: false,
  target: ['chrome70'],
}).catch(() => process.exit(1))
