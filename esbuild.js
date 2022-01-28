require('esbuild').build({
  entryPoints: ['out/index.js'],
  bundle: true,
  outfile: 'public/index.js',
  minify: true,
  sourcemap: false,
  target: ['chrome70'],
}).catch(() => process.exit(1))
