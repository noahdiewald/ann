const esbuild = require('esbuild');
const ElmPlugin = require('esbuild-plugin-elm');

//const watch = process.argv.includes('--watch')
const isProd = process.env.NODE_ENV === 'production'

esbuild.build({
  entryPoints: ['client/index.js'],
  bundle: true,
  outdir: 'static',
  minify: isProd,
//  watch,
  plugins: [
    ElmPlugin({
      debug: true,
      optimize: isProd,
//      clearOnWatch: watch,
      verbose: true,
    }),
  ],
}).catch(_e => process.exit(1))
