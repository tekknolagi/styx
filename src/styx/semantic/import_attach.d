module styx.semantic.import_attach;

import
    std.stdio;
import
    styx.session, styx.token, styx.lexer, styx.ast, styx.symbol;

//TODO-cMaybe: Move this to SymbolizerA
class ImportAttacherVisitor: AstVisitor
{
    alias visit = AstVisitor.visit;

private

    Lexer* _lx;

public:

    this(UnitAstNode u, Lexer* lexer)
    {
        _lx = lexer;
        visit(u);
    }

    override void visit(ImportDeclarationAstNode node)
    {
        foreach (list; node.importList)
        {
            Symbol result = root;
            foreach (i; 0..list.chain.length)
            {
                Symbol[] c = result.find(list.chain[i], SymbolKind.unit);
                if (c.length == 1)
                {
                    c[0].astNode = node;
                    result = c[0];
                }
                else
                {
                    result = null;
                    break;
                }
            }
            if (result is null)
            {
                session.error(_lx.filename, list.startPos,
                "cannot find imported unit `%s`", styx.token.tokenChainText(list.chain));
            }
            else
            {
                node.symbol = result;
                if (session.verbose)
                {
                    writeln("binded import `", styx.token.tokenChainText(list.chain),
                        "` declared in ", _lx.filename);
                }
            }
        }
    }
}

