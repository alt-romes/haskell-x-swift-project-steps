// The Swift Programming Language
// https://docs.swift.org/swift-book

/// A macro that produces both a value and a string containing the
/// source code that generated the value. For example,
///
///     #stringify(x + y)
///
/// produces a tuple `(x + y, "x + y")`.
@attached(peer, names: overloaded)
public macro ForeignImportHaskell() = #externalMacro(module: "HsFFIMacroMacros", type: "ForeignImportHaskellMacro")

public struct HsCallJSON {
    init() {
        fatalError("HsCallJSON should never be constructed, it is just used in the stub function signature from which we generate the actual function that calls a Haskell function.")
    }
}
