module tester;

import
    std.stdio, std.file, std.process, std.string, std.path, std.array;

enum TestType
{
    compile,
    cover,
    fail,
}

immutable string yatolPath;
immutable string testsPath;
immutable string compileCwd;
immutable string failCwd;
immutable string covCwd;

static this()
{
    enum d = __FILE_FULL_PATH__.dirName;
    yatolPath   = d.dirName ~ dirSeparator ~ "bin" ~ dirSeparator ~ "yatol";
    testsPath   = d ~ dirSeparator;
    compileCwd  = d ~ dirSeparator ~ "compile";
    failCwd     = d ~ dirSeparator ~ "fail";
    covCwd      = d.dirName;
}

bool executeTest(const(char)[] filename, TestType type)
{
    char[] argsFile = filename.stripExtension ~ ".args";
    char[] cmd = yatolPath ~ " " ~ filename ~ " ";
    cmd ~= argsFile.exists ? cast(char[]) read(filename.stripExtension ~ ".args") : "";
    string cwd = type == TestType.compile
        ? compileCwd : type == TestType.cover
        ? covCwd : failCwd;

    ProcessPipes pp = pipeShell(cmd, Redirect.stderrToStdout, null, Config.none, cwd);
    int r = pp.pid.wait();
    stdout.flush;

    final switch(type)
    {
        case TestType.compile:  return r == 0;
        case TestType.cover:    return true;
        case TestType.fail:     return r != 0;
    }
}

int main(string[] args)
{
    writeln("========================================================");
    writeln("Starting the coverage tests");
    writeln("========================================================");

    auto cov_entries = dirEntries(testsPath ~ "cover", "*.ya", SpanMode.shallow).array;
    foreach(i, e; cov_entries)
    {
        executeTest(e.name, TestType.cover);
        writeln(e.name, ": ", "OK");
        if (i != cov_entries.length-1)
            writeln("--------------------------------------------------------");
    }

    writeln("========================================================");
    writeln("Starting the tests that must compile...");
    writeln("========================================================");

    auto compile_entries = dirEntries(testsPath ~ "compile", "*.ya", SpanMode.shallow).array;
    foreach(i, e; compile_entries)
    {
        if (!executeTest(e.name, TestType.compile))
        {
            writeln(e.name, ": ", "FAILED");
            return 1;
        }
        else
        {
            writeln(e.name, ": ", "OK");
            if (i != compile_entries.length-1)
                writeln("--------------------------------------------------------");
        }
    }

    writeln("========================================================");
    writeln("Starting tests that must fail...");
    writeln("========================================================");

    auto fail_entries = dirEntries(testsPath ~ "fail", "*.ya", SpanMode.shallow).array;
    foreach(i, e; fail_entries)
    {
        if (!executeTest(e.name, TestType.fail))
        {
            writeln(e.name, ": ", "FAILED");
            return 1;
        }
        else
        {
            writeln(e.name, ": ", "OK");
            if (i != fail_entries.length-1)
                writeln("--------------------------------------------------------");
        }
    }
    return 0;
}
