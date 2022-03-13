const path = require('path');

module.exports = {
  entry: './out/index.js',
  mode: 'production',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'out'),
  },
  resolve: {
    // Use preact with jsx:
    alias: {
      'react': 'preact/compat',
      "react-dom/test-utils": "preact/test-utils",
      "react-dom": "preact/compat",  // Must be below test-utils
      "react/jsx-runtime": "preact/jsx-runtime"
    },
  },
};
