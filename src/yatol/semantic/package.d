/**
 * Drives the semantic phase.
 */
module yatol.semantic;

import
    yatol.lexer, yatol.ast, yatol.session,
    yatol.semantic.node_protection, yatol.semantic.versions,
    yatol.semantic.desugar;


/**
 * Performs some semantic analysis at the unit level.
 *
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
    new DesugarVisitor(uc);
    if (session.hasErrors)
        return false;

    new VersionEvaluatorVisitor(uc, session.userVersions, true);
    if (session.hasErrors)
        return false;

    new NodeProtectionVisitor(uc, lexer);
    if (session.hasErrors)
        return false;

    return true;
}

