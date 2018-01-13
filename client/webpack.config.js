module.exports = {
    entry: './src/main.js',
    output: {
        filename: 'client.js',
    },
    node: {
        fs: "empty"
    }
};
