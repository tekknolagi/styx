module yatol.semantic.import_attach;

import
    std.stdio;
import
    yatol.session, yatol.token, yatol.lexer, yatol.ast, yatol.symbol;

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
                if (Symbol c = result.find(list.chain[i], SymbolKind.unit))
                {
                    result = c;
                }
                else
                {
                    result = null;
                    break;
                }
            }
            if (result is null)
            {
                session.error(_lx.filename, list.position.line, list.position.column,
                "cannot find imported unit `%s`", yatol.token.tokenChainText(list.chain));
            }
            else
            {
                node.symbol = result;
                if (session.verbose)
                {
                    writeln("binded import `", yatol.token.tokenChainText(list.chain),
                        "` declared in ", _lx.filename);
                }
            }
        }
    }
}

