/**
 * Contains an AstVisitor that prints the AST of a UnitContainer.
 */
module yatol.ast_printer;

import
    std.stdio, std.traits, std.format, std.algorithm;
import
    yatol.token, yatol.lexer, yatol.ast;

/**
 * An $(D AstVisitor) that prints the AST of a $(D UnitContainer).
 */
class AstPrinter: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    ptrdiff_t _indentLevel;

    void visitImpl(Node)(Node node)
    {
        static if (!isGrammatic!Node)
        {
            node.accept(this);
        }
        else
        {
            _text ~= _indentText ~ Node.stringof ~ "\n";
            const bool hasChildren = true;//!node.isTerminal;
            if (hasChildren)
            {
                _text ~= _indentText ~ "{\n";
                indent();
            }
            static immutable specifier1 = "%s- %s : `%s` \n";
            static immutable specifier2 = "%s- %s[%d] : `%s` \n";
            static immutable specifier3 = "%s- %s : `%d` \n";
            foreach (member; __traits(allMembers, Node))
            {
                static if (member.among("__monitor", "__ctor", "__vtbl", "Monitor"))
                    continue;
                else static if (is(typeof((){auto a = __traits(getMember, node, member);}))
                    && !hasUDA!(__traits(getMember, Node, member), Semantic))
                {
                    alias NT = typeof(__traits(getMember, node, member));
                    static if (is(NT == Token*))
                    {
                        if (__traits(getMember, node, member) !is null)
                            _text ~= specifier1.format(_indentText, member,
                                __traits(getMember, node, member).text());
                    }
                    else static if (is(NT == Token*[]))
                        foreach(i, t; __traits(getMember, node, member))
                    {
                        _text ~= specifier2.format(_indentText, member, i, t.text);
                    }
                    else static if (is(NT == Token*[][]))
                        foreach(i, t; __traits(getMember, node, member))
                    {
                        _text ~= specifier2.format(_indentText, member, i, t.tokenPointerArrayText);
                    }
                    else static if (is(NT == enum) || isSomeString!NT || is(NT == bool))
                    {
                        _text ~= specifier1.format(_indentText, member,
                            __traits(getMember, node, member));
                    }
                    else static if (isIntegral!NT)
                    {
                        _text ~= specifier3.format(_indentText, member,
                            __traits(getMember, node, member));
                    }
                    else static if (!isCallable!(__traits(getMember, node, member)))
                    {
                        _text ~= specifier1.format(_indentText, member,
                            __traits(getMember, node, member));
                    }
                }
            }
            node.accept(this);
            if (hasChildren)
            {
                outdent();
                _text ~= _indentText ~ "}\n";
            }
        }
    }

    void indent()
    {
        ++_indentLevel;
        _indentText.length = _indentLevel * 4;
        _indentText[] = ' ';
    }

    void outdent()
    {
        --_indentLevel;
        if (_indentLevel < 0)
            _indentLevel = 0;
        _indentText.length = _indentLevel * 4;
    }

    char[] _text;
    char[] _indentText;

public:

    void printText()
    {
        writeln(_text);
    }

    mixin(genVisitMethods("visitImpl(node);"));
}

