module app;

import
    std.getopt, std.file, std.stdio, std.path;
import
    yatol.lexer, yatol.lexer.types, yatol.parser, yatol.parser.debug_visitor;

enum Until
{
    all,
    lexing,
    parsing,
    semantic,
}

struct Options
{
    static bool verbose;
    static Until until;
}

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
    -h --help       : prints this message.
    -v --verbose    : verbose output.
    --until=<phase> : compiles and stops after <phase>, either "all", "lexing", "parsing" or "semantic".
`
    );
    stdout.flush;
}

int main(string[] args)
{
    Options options;
    GetoptResult gr;
    string[] sources;
    Lexer*[] lexers;
    Parser*[] parsers;

    foreach (arg; args[1..$])
    {
        if (arg.exists)
        {
            string ext = extension(arg);
            if (ext == ".ya")
            {
                sources ~= arg;
            }
            else
            {
                stderr.writefln(`error, unrecognized file extension for "%s"`, arg);
                return 1;
            }
        }
        else if (arg[0] != '-')
        {
            stderr.writefln(`error, the file "%s" cannot be found`, arg);
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
        "v|verbose", &options.verbose,
        "until", &options.until,
    );
    catch (GetOptException ge)
    {
        writefln("options error, %s", ge.msg);
        return 0;
    }
    if (gr.helpWanted)
    {
        showHelp();
        return 0;
    }

    // lexes each source
    foreach (source; sources)
    {
        if (options.verbose)
            writeln("lexing ", source, "...");
        lexers ~= new Lexer(source);
        lexers[$-1].lex();
    }
    if (options.until == Until.lexing)
    {
        writeln("lexing phase finished, exited.");
        return 0;
    }

    // parses each lexer
    foreach (lexer; lexers)
    {
        if (options.verbose)
            writeln("parsing ", lexer.filename, "...");
        parsers ~= new Parser(lexer);
        parsers[$-1].parseMainUnit();

        DebugVisitor dbgv = new DebugVisitor(parsers[$-1].unitContainer);
        dbgv.printText;

    }
    if (options.until == Until.parsing)
    {
        writeln("parsing phase finished, exited.");
        return 0;
    }

    return 0;
}

