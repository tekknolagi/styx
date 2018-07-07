/**
 * Drives the semantic phase.
 */
module styx.semantic;

import
    styx.lexer, styx.ast, styx.session,
    styx.semantic.node_protection, styx.semantic.versions,
    styx.semantic.desugar, styx.semantic.echo, styx.semantic.symbolize,
    styx.semantic.import_attach;


/**
 * Performs some semantic analysis at the unit level.
 *
 * <li>A first pass of symbolization</li>
 * <li>the templatized declarations are rewritten as templates</li>
 * <li>the unreachable branches in $(D version()) are supressed</li>
 * <li>$(D protection()) is verified and applied to the nodes</li>
 *
 * Params:
 *      uc = The AST produced by a parser for a unit.
 *      lexer = The lexer that scanned the unit.
 */
bool unitSemantic(UnitAstNode u, Lexer* lexer)
{
    new AstSymbolizerA(u, lexer);
    if (session.hasErrors)
        return false;

    new DesugarVisitor(u);
    if (session.hasErrors)
        return false;

    new VersionEvaluatorVisitor(u, session.userVersions, true);
    if (session.hasErrors)
        return false;

    new NodeProtectionVisitor(u, lexer);
    if (session.hasErrors)
        return false;

    new EchoSemantic(u, lexer);
    if (session.hasErrors)
        return false;

    return true;
}

/**
 * Performs some semantic analysis between units.
 *
 * Params:
 *      uc = The AST produced by a parser for a unit.
 *      lexer = The lexer that scanned the unit.
 */
bool crossUnitSemantic(UnitAstNode u, Lexer* lexer)
{
    new ImportAttacherVisitor(u, lexer);
    if (session.hasErrors)
        return false;

    return true;
}

