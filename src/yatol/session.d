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

/// A compiler message.
struct Message
{
    /// The message type.
    MessageType type;
    /// The message text.
    string text;
}

/**
 * Represents a compiler session, which includes the messages, utilities to
 * append them, some global options.
 */
struct Session
{
__gshared: private:

    Message[] messages;

public:

    /// Indicates if verbose output is expected.
    bool verbose;
    /// Indicates if errors happened.
    bool hasErrors;
    /// Contains the users version identifiers.
    string[] userVersions;

    /**
     * Adds an error message.
     *
     * Params:
     *      text = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void error(A...)(const(char)[] text, A args)
    {
        messages.length += 1;
        messages[$-1].type = MessageType.error;
        messages[$-1].text = format(text, args);
        hasErrors = true;
    }

    /**
     * Adds an informational message.
     *
     * Params:
     *      text = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void info(A...)(const(char)[] text, A args)
    {
        messages.length += 1;
        messages[$-1].type = MessageType.info;
        messages[$-1].text = format(text, args);
    }

    /**
     * Adds an warning message.
     *
     * Params:
     *      text = A format specifier.
     *      args = The variadic arguments, as expected by the specifier.
     */
    static void warn(A...)(const(char)[] text, A args)
    {
        messages.length += 1;
        messages[$-1].type = MessageType.warn;
        messages[$-1].text = format(text, args);
    }

    /// Writes the messages to the standard output.
    static void printMessages()
    {
        foreach(m; messages)
            writeln(m.text);
        messages.length = 0;
    }
}

alias session = Session;

static ~this()
{
    session.printMessages();
}

