module yatol.utils;

import
    yatol.lexer, yatol.parser, yatol.parser.ast;

/**
 * Lexes and parses some code. Usually used for the embedded compiler tests.
 *
 * Params:
 *      code = The source code to process.
 * Returns:
 *      The AST for the code passed as parameter.
 */
UnitContainerAstNode lexAndParse(const(char)[] code, string file = __FILE_FULL_PATH__,
    size_t line = __LINE__)
{
    Lexer lx;
    lx.setSourceFromText(code, file, line);
    lx.lex();
    return Parser(&lx).parse();
}
///
unittest
{
    UnitContainerAstNode uc = lexAndParse("unit a; const s8 a = 42:s8;");
    assert(uc !is null);
}

