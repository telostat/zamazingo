<a name="unreleased"></a>
## [Unreleased]


<a name="0.0.0.3"></a>
## [0.0.0.3] - 2022-07-21
### Chore
- **dev:** add release.sh script

### Fix
- export Zamazingo.Aeson module defs on Zamazingo module

### Pull Requests
- Merge pull request [#6](https://github.com/telostat/zamazingo/issues/6) from telostat/vst/export-aeson-module-defs


<a name="0.0.0.2"></a>
## [0.0.0.2] - 2022-07-21
### Chore
- **dev:** migrate from Stack to Nix
- **release:** 0.0.0.2

### Feat
- add commonAesonOptions, drop deriving-aeson dependency

### Pull Requests
- Merge pull request [#5](https://github.com/telostat/zamazingo/issues/5) from telostat/vst/issue-2
- Merge pull request [#4](https://github.com/telostat/zamazingo/issues/4) from telostat/vst/issue-3


<a name="0.0.0.1"></a>
## 0.0.0.1 - 2022-07-21
### Chore
- init repository
- **dev:** add Weeder configuration
- **dev:** update HLint configuration
- **dev:** integrate git-chglog
- **docs:** fix haddock warnings
- **doctest:** fix NonEmptyText doctest
- **release:** 0.0.0.1
- **test:** fix tests

### Feat
- add {To,From}JSONKey instance to Id type
- add allEq function
- add Zamazingo.Aeson module
- add Zamazingo.GroupBy module
- add Zamazingo.Bool module
- improve Zamazingo.Id module with new lookup table functions
- add Zamazingo.Id module for type-safe entity identifiers
- relax Secret constructor and accessor to underlying value
- add TextPair and related definitions to TextCodec module
- add applicative (alternative) helpers (guarded, guardedA)
- add monadic helpers (whenM, unlessM, ifM, guardM)
- add TimeZoneLabel data type and related definitions
- add Mailess API client implementation
- add EmailAddress data type
- add DateRange data type and related functions
- add Aeson instances to HttpUrl, revisit documentation
- start adding time-related utility functions (formatting on Text)
- add String, ByteString and lazy ByteString codec functions
- add HttpUrl data type
- add Secret data type
- add Host data type
- add decodeTextTH function for compile-time creation of decodable values
- add TextCodec instance to Port data type
- init codebase

### Pull Requests
- Merge pull request [#1](https://github.com/telostat/zamazingo/issues/1) from telostat/init


[Unreleased]: https://github.com/telostat/zamazingo/compare/0.0.0.3...HEAD
[0.0.0.3]: https://github.com/telostat/zamazingo/compare/0.0.0.2...0.0.0.3
[0.0.0.2]: https://github.com/telostat/zamazingo/compare/0.0.0.1...0.0.0.2
