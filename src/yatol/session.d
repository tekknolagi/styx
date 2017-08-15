module yatol.session;

/**
 *
 */
struct Session
{

public: __gshared:

    bool verbose;

    string[] messages;
    string[] userVersions;

}

alias session = Session;

