module yatol.semantic;

import
    yatol.lexer, yatol.parser.ast,
    yatol.semantic.node_protection, yatol.semantic.versions;


/**
 * Performs some semantic analysis at the unit level.
 *
 * Params:
 *      uc = The AST produced by a parser for a unit.
 *      lexer = The lexer that scanned the unit.
 */
bool unitSemantic(UnitContainerAstNode uc, Lexer* lexer)
{
    if (!(new NodeProtectionVisitor(uc, lexer)).success())
        return false;

    return true;
}
