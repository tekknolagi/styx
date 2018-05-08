module yatol.session;

import
    std.stdio, std.format;

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

    /// Indicates if verbose output is expected.
    bool verbose;
    /// Indicates the count of errors.

    /// Contains the users version identifiers.
    string[] userVersions;

    /// Indicates if errors happened.
    static bool hasErrors(){return _errorsCount != 0;}

    /// A callback for the messages. When not set, message are written to the standard output.
    static void function(const(char)[] filename, size_t line, size_t column,
        MessageType type, const(char)[] text) messageFunc;

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
     *      line = The line where the error is located.
     *      column = The column where the error is located.
     *      spec = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void error(A...)(const(char)[] filename, size_t line, size_t column, const(char)[] spec, A args)
    {
        if (_gag)
            return;

        _errorsCount++;
        string msg = format(spec, args);
        if (messageFunc)
            messageFunc(filename, line, column,  MessageType.error, msg);
        else
            writeln(filename, '(', line, ',', column, "): error, ", msg);
    }

    /**
     * Adds an informational message.
     *
     * Params:
     *      filename = The file where the error is located.
     *      line = The line where the error is located.
     *      column = The column where the error is located.
     *      spec = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void info(A...)(const(char)[] filename, size_t line, size_t column, const(char)[] spec, A args)
    {
        if (_gag)
            return;

        string msg = format(spec, args);
        if (messageFunc)
            messageFunc(filename, line, column,  MessageType.info, msg);
        else
            writeln(filename, '(', line, ',', column, "): information, ", msg);
    }

    /**
     * Adds an warning message.
     *
     * Params:
     *      filename = The file where the error is located.
     *      line = The line where the error is located.
     *      column = The column where the error is located.
     *      spec = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void warn(A...)(const(char)[] filename, size_t line, size_t column, const(char)[] spec, A args)
    {
        if (_gag)
            return;

        string msg = format(spec, args);
        if (messageFunc)
            messageFunc(filename, line, column,  MessageType.warn, msg);
        else
            writeln(filename, '(', line, ',', column, "): warning, ", msg);
    }
}

alias session = Session;

