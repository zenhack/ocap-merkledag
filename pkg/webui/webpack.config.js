const path = require('path');

module.exports = {
  entry: './out/index.js',
  mode: 'production',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'out'),
  }
};
