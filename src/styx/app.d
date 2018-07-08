/**
 * Styx compiler main application.
 */
module app;

import
    std.getopt, std.file, std.stdio, std.path, std.datetime.stopwatch,
    std.array, std.format;
import
    styx.token, styx.lexer, styx.parser, styx.ast,
    styx.session, styx.semantic, styx.ast_printer;

private enum Until
{
    _,
    lexing,
    parsing,
    semantic,
}

private struct Options
{
private: __gshared:
    bool mtime;
    bool ast;
    bool pipe;
    Until until;
    bool m32;
    bool m64;
}

alias options = Options;

private struct Timer
{
__gshared: private:

    ubyte _stage;
    Duration[4] _times;
    StopWatch _sw;

public:

    static void start()
    {
        _sw.start();
    }

    static void peek()
    {
        _times[_stage++] = _sw.peek();
    }

    static void print()
    {
        writeln("duration:");
        writefln("lexing  :%s ", _times[0]);
        writefln("parsing :%s ", _times[1] - _times[0]);
        writefln("semantic:%s ", _times[2] - _times[1]);
        writefln("codegen :%s ", _times[3] - _times[2]);
        writefln("total   :%s ", _times[3]);
    }
}

alias timer = Timer;

void showHelp()
{
    writeln(
`
=============
Styx compiler
=============
Command line syntax:
    styx <.ya Files...> [<Options>]
Files:
    a list of styx source files, space-separated.
Options:
    -a or --ast           : prints the AST of each source.
    -h or --help          : prints this message.
          --m32           : compiles 32 bit code.
          --m64           : compiles 64 bit code.
    -p or --pipe          : creates a source named "stdin" by piping the input.
    -t or --time          : measures the time spent to compile.
          --until=<phase> : stops after <phase>, either "lexing", "parsing" or "semantic".
    -v or --verbose       : verbose output.
          --versions<=ids>: defines the version() identifiers list, comma separated.
`
    );
}

int main(string[] args)
{
    version(unittest)
        session.resetErrorsCount();

    import core.runtime: dmd_coverSetMerge;
    dmd_coverSetMerge(true);

    GetoptResult gr;
    string[] sources;
    Lexer*[] lexers;
    Parser*[] parsers;

    foreach (arg; args[1..$])
    {
        if (arg.exists)
        {
            if (arg.length > 3 && arg[$-3..$] == ".sx")
            {
                sources ~= arg;
            }
            else
            {
                writefln(`error, unrecognized file extension for "%s"`, arg);
                return 1;
            }
        }
        else if (arg[0] != '-')
        {
            writefln(`error, the file "%s" does not seem to exist`, arg);
            return 1;
        }
    }

    arraySep = ",";
    try gr = getopt(args,
        "a|ast", &options.ast,
        "p|pipe", &options.pipe,
        "t|time", &options.mtime,
        "until", &options.until,
        "v|verbose", &session.verbose,
        "versions", &session.userVersions,
        "m32", &options.m32,
        "m64", &options.m64,
    );
    catch (GetOptException ge)
    {
        stderr.writefln("options error, %s", ge.msg);
        return 1;
    }

    if (gr.helpWanted)
    {
        showHelp();
        return 0;
    }

    if (!sources.length && !options.pipe)
    {
        showHelp();
        writeln("nothing to compile, exited !");
        return 0;
    }

    if (options.pipe)
    {
        char[] c = stdin.byLine.join("\n");
        lexers ~= new Lexer;
        lexers[$-1].setSourceFromText(c, "stdin");
    }

    if (options.m32)
        session.regSize = 32;
    else if (options.m64)
        session.regSize = 64;

    if (options.mtime)
        timer.start();

    // lexes each source
    if (options.pipe)
    {
        lexers[0].lex();
    }
    foreach (source; sources)
    {
        if (session.verbose)
            writefln("lexing %s...", source);
        try
        {
            lexers ~= new Lexer(source);
            lexers[$-1].lex();
        }
        catch (FileException fe)
        {
            stderr.writeln("File exception raised when lexing ", source);
            stderr.writeln(fe.msg);
            return 1;
        }
    }
    if (options.mtime)
    {
        timer.peek();
    }
    if (options.until == Until.lexing)
    {
        if (options.mtime)
            timer.print();
        writeln("lexing phase finished, exited.");
        return 0;
    }

    // parses each lexer
    foreach (ref lexer; lexers)
    {
        if (session.verbose)
            writefln("parsing %s...", lexer.filename);
        parsers ~= new Parser(lexer);
        if (UnitAstNode u = parsers[$-1].parse())
        {
            if (options.ast && options.until == Until.parsing)
            {
                writefln("AST for %s", lexer.filename);
                AstPrinter ap = new AstPrinter();
                ap.visit(u);
                writeln(ap.text);
            }
        }
        else
        {
            writefln("error, failed to parse `%s`", lexer.filename);
            return 1;
        }
    }
    if (options.mtime)
    {
        timer.peek();
    }
    if (options.until == Until.parsing)
    {
        if (options.mtime)
            timer.print();
        writeln("parsing phase finished, exited.");
        return 0;
    }

    foreach (i, ref parser; parsers)
    {
        if (session.verbose)
            writefln("unit semantic for %s...", lexers[i].filename);
        if (!unitSemantic(parser.unit, lexers[i]))
        {
            writefln("error, unit semantic for `%s` failed", lexers[i].filename);
            return 1;
        }
        if (options.ast)
        {
            writefln("AST for %s", lexers[i].filename);
            AstPrinter ap = new AstPrinter();
            ap.visit(parser.unit);
            writeln(ap.text);
        }
    }
    foreach (i, ref parser; parsers)
    {
        if (session.verbose)
            writefln("crossed-unit semantic for %s...", lexers[i].filename);
        if (!crossUnitSemantic(parser.unit, lexers[i]))
        {
            writefln("error, crossed-unit semantic for `%s` failed", lexers[i].filename);
            return 1;
        }
    }
    if (options.mtime)
    {
        timer.peek();
    }
    if (options.until == Until.semantic)
    {
        if (options.mtime)
            timer.print();
        writeln("semantic phase finished, exited.");
        return 0;
    }

    if (options.mtime)
    {
        timer.peek();
        timer.print();
    }

    return 0;
}

