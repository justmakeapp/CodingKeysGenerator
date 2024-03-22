import SwiftDiagnostics
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct CodingKeysMacro: MemberMacro {
    public static func expansion(
        of node: AttributeSyntax,
        providingMembersOf declaration: some DeclGroupSyntax,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        guard declaration.is(StructDeclSyntax.self) || declaration.is(ClassDeclSyntax.self) else {
            let error = Diagnostic(node: node._syntaxNode, message: CodingKeyMacroDiagnostic.isNotClassOrStruct("CodingKeysMacro"))
            context.diagnose(error)
            return []
        }
        let currentModifiers: [String] = declaration.modifiers.compactMap { ($0 as? DeclModifierSyntax)?.name.text }
        let accessModifier = Set(AccessModifier.allCases.map(\.rawValue)).intersection(currentModifiers)
        let codingKeyModifier = {
            if let firstModifier = accessModifier.first {
                return "\(firstModifier) "
            }
            return ""
        }()

        let cases: [String] = try declaration.memberBlock.members.compactMap { member in
            guard let variableDecl = member.decl.as(VariableDeclSyntax.self) else { return nil }
            guard let property = variableDecl.bindings.first?.pattern.as(IdentifierPatternSyntax.self)?.identifier.text
            else {
                return nil
            }
            if attributesElement(withIdentifier: "CodingKeyIgnored", in: variableDecl.attributes) != nil {
                return nil
            } else if let element = attributesElement(withIdentifier: "CodingKey", in: variableDecl.attributes) {
                guard let customKeyName = customKey(in: element) else {
                    let diagnostic = Diagnostic(node: Syntax(node), message: CodingKeysDiagnostic())
                    throw DiagnosticsError(diagnostics: [diagnostic])
                }
                return "case \(property) = \(customKeyName)"
            } else {
                let raw = property.dropBackticks()
                return property.contains("`") ? "case `\(raw)` = \"\(property)\"" : "case \(property)"
            }
        }
        guard !cases.isEmpty else { return [] }
        let casesDecl: DeclSyntax = """
        \(raw: codingKeyModifier)enum CodingKeys: String, CodingKey {
            \(raw: cases.joined(separator: "\n    "))
        }
        """
        return [casesDecl]
    }

    private static func attributesElement(
        withIdentifier macroName: String,
        in attributes: AttributeListSyntax?
    ) -> AttributeListSyntax.Element? {
        attributes?.first {
            $0.as(AttributeSyntax.self)?
                .attributeName
                .as(IdentifierTypeSyntax.self)?
                .description == macroName
        }
    }

    private static func customKey(in attributesElement: AttributeListSyntax.Element) -> ExprSyntax? {
        attributesElement
            .as(AttributeSyntax.self)?
            .arguments?
            .as(LabeledExprListSyntax.self)?
            .first?
            .expression
    }
}

public struct CustomCodingKeyMacro: PeerMacro {
    public static func expansion(
        of _: AttributeSyntax,
        providingPeersOf _: some DeclSyntaxProtocol,
        in _: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        []
    }
}

public struct CodingKeyIgnoredMacro: PeerMacro {
    public static func expansion(
        of _: AttributeSyntax,
        providingPeersOf _: some DeclSyntaxProtocol,
        in _: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        []
    }
}

struct CodingKeysDiagnostic: DiagnosticMessage {
    let message: String = "Empty argument"
    let diagnosticID: SwiftDiagnostics.MessageID = .init(domain: "CodingKeysGenerator", id: "emptyArgument")
    let severity: SwiftDiagnostics.DiagnosticSeverity = .error
}

private extension String {
    func dropBackticks() -> String {
        count > 1 && first == "`" && last == "`" ? String(dropLast().dropFirst()) : self
    }
}

enum CodingKeyMacroDiagnostic: Error, DiagnosticMessage {
    case isNotClassOrStruct(String)

    private var rawValue: String {
        switch self {
        case let .isNotClassOrStruct(macro):
            return "isNotClassOrStruct_\(macro)"
        }
    }

    // MARK: - DiagnosticMessage

    var severity: DiagnosticSeverity { .error }

    var message: String {
        switch self {
        case let .isNotClassOrStruct(macro):
            return "'@\(macro)' can only be applied to a class or struct."
        }
    }

    var diagnosticID: MessageID {
        .init(domain: "CodingKeysMacro", id: rawValue)
    }
}

public enum AccessModifier: String, CaseIterable {
    case `private`
    case `public`
    case package
    case `internal`
    case `fileprivate`
}
