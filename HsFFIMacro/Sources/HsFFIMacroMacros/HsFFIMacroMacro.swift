import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

/// Implementation of the `stringify` macro, which takes an expression
/// of any type and produces a tuple containing the value of that expression
/// and the source code that produced the value. For example
///
///     #stringify(x + y)
///
///  will expand to
///
///     (x + y, "x + y")
public struct StringifyMacro: ExpressionMacro {
    public static func expansion(
        of node: some FreestandingMacroExpansionSyntax,
        in context: some MacroExpansionContext
    ) -> ExprSyntax {
        guard let argument = node.argumentList.first?.expression else {
            fatalError("compiler bug: the macro does not have any arguments")
        }

        return "(\(argument), \(literal: argument.description))"
    }
}

let knownCallingConvs = ["HsCallJSON"]

public struct ForeignImportHaskellMacro: PeerMacro {
    public static func expansion(of node: AttributeSyntax, providingPeersOf declaration: some DeclSyntaxProtocol, in context: some MacroExpansionContext) throws -> [DeclSyntax] {
        guard let function = declaration.as(FunctionDeclSyntax.self) else {
            throw ForeignImportHaskellError.onlyApplicableToFunction
        }
        
        let sig = function.signature,
            fname = function.name
        
        // If I wanted the calling convention used I could use a "guard let" instead of "!= nil"
        guard knownCallingConvs.firstIndex(of: sig.parameterClause.parameters.first?.type.description ?? "") != nil else {
            throw ForeignImportHaskellError.unknownCallingConv
        }
        
        guard let retType = sig.returnClause?.type else {
            throw ForeignImportHaskellError.noReturnClause
        }
        
        let args = sig.parameterClause.parameters.dropFirst().map({ param in
                guard let paraType = param.type.as(IdentifierTypeSyntax.self)?.name else { return "" }
                return "\(param.firstName)\(param.secondName ?? ""): \(paraType)" }).joined(separator: ", "),
            
            argNames = sig.parameterClause.parameters.dropFirst().map({param in
                let name = param.secondName ?? param.firstName
                return name.text.trimmingCharacters(in: .whitespacesAndNewlines)
            }),
            
            encodedArgs = argNames.map({arg in
                """
                var \(arg)_data = try hs_enc.encode(\(arg))
                let \(arg)_datalen = Int64(\(arg)_data.count)
                """
            }).joined(separator: "\n"),
        
            pointersToArgs = argNames.map({arg in
                """
                try \(arg)_data.withUnsafeMutableBytes { (\(arg)_ptr:UnsafeMutableRawBufferPointer) in
                """
            }).joined(separator: "\n"),
            
            closePointersToArgsClosures = argNames.map({_ in return "}"}).joined(separator: "\n"),
        
            fcallArgs = argNames.map({ arg in
                "\(arg)_ptr.baseAddress, \(arg)_datalen"
            }).joined(separator: ", "),
        
            fcall:ExprSyntax =
                "h\(fname)(\(raw: fcallArgs), res_ptr.baseAddress, size_ptr.baseAddress)",
            
            decodeAndReturnResult:StmtSyntax =
                """
                let new_data = Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none)
                print("Read JSON from Haskell: \\(String(bytes: new_data, encoding: .ascii) ?? "???")")
                return try hs_dec.decode(\(retType.self), from: new_data)
                """
        
        return [
        """
        func \(fname)(\(raw: args)) -> \(retType) {
          let hs_enc = JSONEncoder()
          let hs_dec = JSONDecoder()
          do {
            \(raw: encodedArgs)
            return \(raw: pointersToArgs)
                // Allocate buffer for result and allocate a pointer to an int with the initial size of the buffer
                let buf_size = 1024000
                enum HsFFIError: Error {
                    case requiredSizeIs(Int)
                }
                return try withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { size_ptr in
                    size_ptr.baseAddress?.pointee = buf_size
                    
                    do {
                        return try withUnsafeTemporaryAllocation(byteCount: buf_size, alignment: 1) { res_ptr in
                            
                            \(fcall)
                            
                            if let required_size = size_ptr.baseAddress?.pointee {
                                if required_size > buf_size {
                                    throw HsFFIError.requiredSizeIs(required_size)
                                }
                            }
        
                            \(decodeAndReturnResult)
                        }
                    } catch HsFFIError.requiredSizeIs(let required_size) {
                        print("Retrying with required size: \\(required_size)")
                        return try withUnsafeTemporaryAllocation(byteCount: required_size, alignment: 1) { res_ptr in
                            size_ptr.baseAddress?.pointee = required_size
                            
                            \(fcall)
                            \(decodeAndReturnResult)
                        }
                    }
                }
              \(raw: closePointersToArgsClosures)
          } catch {
              fatalError("Error decoding JSON marshaled from Haskell, probably: \\(error)")
          }
        }
        """
        ]
    }
}

public enum ForeignImportHaskellError: CustomStringConvertible, Error {
    case onlyApplicableToFunction
    case unknownCallingConv
    case noReturnClause

    public var description: String {
        switch self {
        case .onlyApplicableToFunction:
            "@ForeignImportHaskell can only be applied to a function."
        case .unknownCallingConv:
            "@ForeignImportHaskell can only be applied to functions whose first argument is a stub parameter whose type indicates the calling convention. Known types include: \(knownCallingConvs.joined(separator: ", "))."
        case .noReturnClause:
            "@ForeignImportHaskell can only be applied to functions with a return clause."
        }
    }
}

@main
struct HsFFIMacroPlugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        ForeignImportHaskellMacro.self
    ]
}
