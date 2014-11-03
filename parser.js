function readWhitespace(in) {
    for (var char = readChar(in, null); true; char = readChar(in, null)) {
        if (!member(char, [null, 'hashnewline', null])) {
            return char;
        };
    };
};
function parseIntExpression() {
    return 0;
};
