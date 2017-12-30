Contributing

## Issues

Issues are tracked here: https://github.com/BBasile/yatol/issues.

### General

Do not put tags (even beween square brackets), they'll be added after a review.

```
~~[BUG, PARSER] Foo doesn't work after a Bar~~
```

### Enhancements

Enhancement requests are really not expected for now, unless you're experienced enough to implement the thing yourself.

### Bugs

Bug reports should include a test case.
Fundamental problems should be detailed and explained.

## Patches and pull requests

### Style

#### Phobos style

The project follows the Phobos D style (naming, case, braces, 4 spaces, not tans, etc) to a few exceptions:

- Space before colons, for example for selective imports or inheritence list, are not required. Spaces after colon are still required, following standard punctuation rules.
- Top level imports use a single import statement by root package. Elements are indented and start on a new line. Packages are sorted from the lowest to the highest level.

```d
import
    core.foo, core.bar;
import
    std.foo, std.bar;
import
    yatol.foo, yatol.bar;
```

- Always add (or never remove) a last empty line at the end of a module. The last empty line prevents a possible annoyance when using editors that support code folding and when the last block is collapsed.

#### Coding style

- Meta programming and mixins are almost forbidden. Keep in mind that the sources, at some point, will have to be converted to bootstrap. This also allows to get a more accurate and higher unittest coverage.
- The project must compile with the latest LDC stable version, without warnings. This implies that the latest language features cannot always be used.

### Testing

Everything must be tested and covered by unittests.
Unittests must be located in the same module as the feature they cover. This makes the development easier (write->test->fix->write-test->fix...) with certain editors and when the project is setup as a library.
