/**
 * Yatol compiler main application.
 */
module app;

import
    std.getopt, std.file, std.stdio, std.path, std.datetime, std.array,
    std.format;
import
    yatol.token, yatol.lexer, yatol.parser, yatol.ast,
    yatol.session, yatol.semantic, yatol.ast_printer;

enum Until
{
    _,
    lexing,
    parsing,
    semantic,
}

struct Options
{
__gshared:
    bool mtime;
    bool ast;
    bool pipe;
    Until until;
}

alias options = Options;

struct Timer
{
__gshared: private:

    ubyte _stage;
    TickDuration[4] _times;
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
        session.info("duration:");
        session.info("lexing  :%s ", dur!"hnsecs"(_times[0].hnsecs));
        session.info("parsing :%s ", dur!"hnsecs"((_times[1] - _times[0]).hnsecs));
        session.info("semantic:%s ", dur!"hnsecs"((_times[2] - _times[1]).hnsecs));
        session.info("codegen :%s ", dur!"hnsecs"((_times[3] - _times[2]).hnsecs));
        session.info("total   :%s ", dur!"hnsecs"(_times[3].hnsecs));
    }
}

alias timer = Timer;

void showHelp()
{
    session.info(
`
==============
Yatol compiler
==============
Command line syntax:
    yatol <.ya Files...> [<Options>]
Files:
    a list of yatol source files, space-separated.
Options:
    -a or --ast           : prints the AST of each source.
    -h or --help          : prints this message.
    -p or --pipe          : creates a source named "stdin" by piping the input.
    -t or --time          : measures the time spent to compile.
          --until=<phase> : compiles and stops after <phase>, either "lexing", "parsing" or "semantic".
    -v or --verbose       : verbose output.
          --versions<=ids>: defines the version() identifiers list, comma separated.
`
    );
}

int main(string[] args)
{
    GetoptResult gr;
    string[] sources;
    Lexer*[] lexers;
    Parser*[] parsers;

    scope(exit)
        session.printMessages();

    foreach (arg; args[1..$])
    {
        if (arg.exists)
        {
            if (arg.length > 3 && arg[$-3..$] == ".ya")
            {
                sources ~= arg;
            }
            else
            {
                session.error(`error, unrecognized file extension for "%s"`, arg);
                return 1;
            }
        }
        else if (arg[0] != '-')
        {
            session.error(`error, the file "%s" does not seem to exist`, arg);
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
        session.info("\nnothing to compile, exited !");
        return 0;
    }

    if (options.pipe)
    {
        char[] c = stdin.byLine.join("\n");
        lexers ~= new Lexer;
        lexers[$-1].setSourceFromText(c, "stdin");
    }

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
            session.info("lexing %s...", source);
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
        session.info("lexing phase finished, exited.");
        return 0;
    }

    // parses each lexer
    foreach (ref lexer; lexers)
    {
        if (session.verbose)
            session.info("parsing %s...", lexer.filename);
        parsers ~= new Parser(lexer);
        if (UnitContainerAstNode uc = parsers[$-1].parse())
        {
            if (options.ast && options.until == Until.parsing)
            {
                session.info("AST for %s", lexer.filename);
                AstPrinter ap = new AstPrinter();
                ap.visit(uc);
                session.info(ap.text);
            }
        }
        else
        {
            session.error("error, failed to parse `%s`", lexer.filename);
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
        session.info("parsing phase finished, exited.");
        return 0;
    }

    foreach (i, ref parser; parsers)
    {
        if (session.verbose)
            session.info("unit semantic for %s...", lexers[i].filename);
        if (!unitSemantic(parser.unitContainer, lexers[i]))
            return 1;
        if (options.ast)
        {
            session.info("AST for %s", lexers[i].filename);
            AstPrinter ap = new AstPrinter();
            ap.visit(parser.unitContainer);
            session.info(ap.text);
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
        session.info("semantic phase finished, exited.");
        return 0;
    }

    if (options.mtime)
    {
        timer.peek();
        timer.print();
    }

    return 0;
}

