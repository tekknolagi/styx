module styx.transpilers;

import
    styx.ast;

/**
 * Interface for a source to source styx compiler.
 */
interface YatolTranspiler
{
    /// Processes a unit.
    void processUnit(UnitAstNode ast);
    /// Compiles the transpiled units.
    bool transpile();
}

