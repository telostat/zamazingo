<a name="unreleased"></a>
## [Unreleased]


<a name="0.0.0.1"></a>
## 0.0.0.1 - 2022-07-21
### Chore
- init repository
- **dev:** add Weeder configuration
- **dev:** update HLint configuration
- **dev:** integrate git-chglog
- **docs:** fix haddock warnings
- **doctest:** fix NonEmptyText doctest
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


[Unreleased]: https://github.com/telostat/zamazingo/compare/0.0.0.1...HEAD
