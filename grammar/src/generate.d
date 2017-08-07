module generate;

import
    pegged.grammar, pegged.peg;

void main()
{
    asModule("yatol", "src/yatol", import("peg.txt"));
}
