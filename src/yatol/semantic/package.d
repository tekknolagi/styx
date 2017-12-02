/**
 * Drives the semantic phase.
 */
module yatol.semantic;

import
    yatol.lexer, yatol.ast, yatol.session,
    yatol.semantic.node_protection, yatol.semantic.versions,
    yatol.semantic.desugar, yatol.semantic.echo, yatol.symbol,
    yatol.semantic.import_attach;


/**
 * Performs some semantic analysis at the unit level.
 *
 * <li>A first pass of symbolization without handling the imports</li>
 * <li>The import declarations are linked to their corresponding symbol</li>
 * <li>the templatized declarations are rewritten as templates</li>
 * <li>the unreachable branches in $(D version()) are supressed</li>
 * <li>$(D protection()) is verified and applied to the nodes</li>
 *
 * Params:
 *      uc = The AST produced by a parser for a unit.
 *      lexer = The lexer that scanned the unit.
 */
bool unitSemantic(UnitContainerAstNode uc, Lexer* lexer)
{
    new AstSymbolizerA(uc);
    if (session.hasErrors)
        return false;

    new DesugarVisitor(uc);
    if (session.hasErrors)
        return false;

    new VersionEvaluatorVisitor(uc, session.userVersions, true);
    if (session.hasErrors)
        return false;

    new NodeProtectionVisitor(uc, lexer);
    if (session.hasErrors)
        return false;

    new EchoSemantic(uc, lexer);
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
bool crossUnitSemantic(UnitContainerAstNode uc, Lexer* lexer)
{
    new ImportAttacherVisitor(uc, lexer);
    if (session.hasErrors)
        return false;

    return true;
}


