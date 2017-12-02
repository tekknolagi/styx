module tester;

import
    std.stdio, std.file, std.process, std.string, std.path, std.array;

enum TestType
{
    compile,
    fail,
}

immutable string yatolPath;
immutable string testsPath;
immutable string compileCwd;
immutable string failCwd;

static this()
{
    enum d = __FILE_FULL_PATH__.dirName;
    yatolPath   = d.dirName ~ dirSeparator ~ "bin" ~ dirSeparator ~ "yatol";
    testsPath   = d ~ dirSeparator;
    compileCwd  = d ~ dirSeparator ~ "compile";
    failCwd     = d ~ dirSeparator ~ "fail";
}

bool executeTest(const(char)[] filename, TestType type)
{
    char[] argsFile = filename.stripExtension ~ ".args";
    char[] cmd = yatolPath ~ " " ~ filename ~ " ";
    cmd ~= argsFile.exists ? cast(char[]) read(filename.stripExtension ~ ".args") : "";
    string cwd = type == TestType.compile ? compileCwd : failCwd;

    ProcessPipes pp = pipeShell(cmd, Redirect.stderrToStdout, null, Config.none, cwd);
    int r = pp.pid.wait();
    stdout.flush;

    if (type == TestType.compile) return r == 0;
    else if (type == TestType.fail) return r != 0;
    else assert(0);
}

int main(string[] args)
{
    writeln("========================================================");
    writeln("Starting the tests that must compile...");
    writeln("========================================================");

    auto centries = dirEntries(testsPath ~ "compile", "*.ya", SpanMode.shallow).array;
    foreach(i, e; centries)
    {
        if (!executeTest(e.name, TestType.compile))
        {
            writeln(e.name, ": ", "FAILED");
            return 1;
        }
        else
        {
            writeln;
            writeln(e.name, ": ", "OK");
            if (i != centries.length-1)
                writeln("--------------------------------------------------------");
        }
    }

    writeln("========================================================");
    writeln("Starting tests that must fail...");
    writeln("========================================================");

    auto fentries = dirEntries(testsPath ~ "fail", "*.ya", SpanMode.shallow).array;
    foreach(i, e; fentries)
    {
        if (!executeTest(e.name, TestType.fail))
        {
            writeln(e.name, ": ", "FAILED");
            return 1;
        }
        else
        {
            writeln(e.name, ": ", "OK");
            if (i != fentries.length-1)
                writeln("--------------------------------------------------------");
        }
    }
    return 0;
}
