module app;

import
    std.getopt, std.file, std.stdio, std.path, std.datetime;
import
    yatol.lexer, yatol.lexer.types, yatol.parser, yatol.parser.ast,
    yatol.semantic,
    yatol.parser.debug_visitor;

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
    bool verbose;
    bool print;
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
        writeln("duration:");
        writeln("lexing  : ", dur!"hnsecs"(_times[0].hnsecs));
        writeln("parsing : ", dur!"hnsecs"((_times[1] - _times[0]).hnsecs));
        writeln("semantic: ", dur!"hnsecs"((_times[2] - _times[1]).hnsecs));
        writeln("codegen : ", dur!"hnsecs"((_times[3] - _times[2]).hnsecs));
        writeln("total   : ", dur!"hnsecs"(_times[3].hnsecs));
    }
}

alias timer = Timer;

void showHelp()
{
    write(
`
==============
Yatol compiler
==============
Command line syntax:
    yatol <.ya Files...> [<Options>]
Files:
    a list of yatol source files, space-separated.
Options:
    -h or --help          : prints this message.
    -p or --print         : print the AST of each source.
    -t or --time          : measure the time spent to compile.
          --until=<phase> : compiles and stops after <phase>, either "lexing", "parsing" or "semantic".
    -v or --verbose       : verbose output.
`
    );
    stdout.flush;
}

int main(string[] args)
{
    GetoptResult gr;
    string[] sources;
    Lexer*[] lexers;
    Parser*[] parsers;

    foreach (arg; args[1..$])
    {
        if (arg.exists)
        {
            if (extension(arg) == ".ya")
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

    if (!sources.length)
    {
        showHelp();
        writeln("nothing to compile, exited !");
        return 0;
    }

    try gr = getopt(args,
        "p|print", &options.print,
        "t|time", &options.mtime,
        "until", &options.until,
        "v|verbose", &options.verbose,

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

    if (options.mtime)
        timer.start();

    // lexes each source
    foreach (source; sources)
    {
        if (options.verbose)
            writeln("lexing ", source, "...");
        try
        {
            lexers ~= new Lexer(source);
            lexers[$-1].lex();
        }
        catch (FileException fe)
        {
            stderr.writeln("Exception raised when lexing ", source);
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
        if (options.verbose)
            writeln("parsing ", lexer.filename, "...");
        parsers ~= new Parser(lexer);
        if (UnitContainerAstNode uc = parsers[$-1].parse())
        {
            if (options.print)
            {
                writeln("AST for ", lexer.filename);
                DebugVisitor dbgv = new DebugVisitor();
                dbgv.visit(uc);
                dbgv.printText;
                writeln;
            }
        }
        else
        {
            writeln("error, failed to parse `",  lexer.filename, "`");
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
        if (options.verbose)
            writeln("unit semantic for ", lexers[i].filename, "...");
        if (!unitSemantic(parser.unitContainer, lexers[i]))
            return 1;
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

