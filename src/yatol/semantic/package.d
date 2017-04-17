module yatol.semantic;

import
   yatol.parser.ast, yatol.semantic.node_protection;


/**
 *
 */
bool unitSemantic(UnitContainerAstNode uc)
{
    NodeProtectionVisitor npv = new NodeProtectionVisitor(uc);

    return true;
}
