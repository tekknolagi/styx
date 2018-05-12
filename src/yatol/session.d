module yatol.session;

import
    std.stdio, std.format;
import
    yatol.token;

/// Describes the type of a message
enum MessageType
{
    /// error
    error,
    /// information, e.g in verbose mode
    info,
    /// warning
    warn,
}

/**
 * Represents a compiler session, which includes the messages, utilities to
 * append them, some global options.
 */
struct Session
{
__gshared: private:

    size_t _errorsCount;
    ptrdiff_t _gag;

public:

    version(X86_64)
        byte regSize = 64;
    else version(X86)
        byte regSize = 32;
    else
        static assert(0);

    /// Indicates if verbose output is expected.
    bool verbose;
    /// Contains the users version identifiers.
    string[] userVersions;
    /// Indicates if errors happened.
    static bool hasErrors(){return _errorsCount != 0;}
    /// A callback for the messages. When not set, message are written to the standard output.
    static void function(const(char)[] filename, Position pos, MessageType type,
        const(char)[] text) messageFunc;

    /// Sets if messages are gagged.
    static void startGagging()
    {
        _gag++;
    }

    /// Sets if messages are gagged.
    static void stopGagging()
    {
        _gag--;
    }

    /// Returns: The count of error.
    static size_t errorsCount(){return _errorsCount;}

    /// Reset the count of errors to 0.
    static void resetErrorsCount(){_errorsCount = 0;}

    /**
     * Adds an error message.
     *
     * Params:
     *      filename = The file where the error is located.
     *      pos = The error position.
     *      spec = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void error(A...)(const(char)[] filename, Position pos, const(char)[] spec, A args)
    {
        if (_gag)
            return;

        _errorsCount++;
        string msg = format(spec, args);
        if (messageFunc)
            messageFunc(filename, pos,  MessageType.error, msg);
        else
            writeln(filename, '(', pos.line, ',', pos.column, "): error, ", msg);
    }

    /**
     * Adds an informational message.
     *
     * Params:
     *      filename = The file where the error is located.
     *      pos = The error position.
     *      spec = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void info(A...)(const(char)[] filename, Position pos, const(char)[] spec, A args)
    {
        if (_gag)
            return;

        string msg = format(spec, args);
        if (messageFunc)
            messageFunc(filename, pos,  MessageType.info, msg);
        else
            writeln(filename, '(', pos.line, ',', pos.column, "): information, ", msg);
    }

    /**
     * Adds an warning message.
     *
     * Params:
     *      filename = The file where the error is located.
     *      pos = The error position.
     *      spec = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void warn(A...)(const(char)[] filename, Position pos, const(char)[] spec, A args)
    {
        if (_gag)
            return;

        string msg = format(spec, args);
        if (messageFunc)
            messageFunc(filename, pos,  MessageType.warn, msg);
        else
            writeln(filename, '(', pos.line, ',', pos.column, "): warning, ", msg);
    }
}

alias session = Session;

