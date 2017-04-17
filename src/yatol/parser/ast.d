module yatol.parser.ast;

import
    std.conv, std.algorithm.iteration, std.stdio;
import
    yatol.lexer.types;

/// USed to annotate the fields set by the semantic.
enum Semantic;

/// The AST visitor.
class AstVisitor
{
    ///
    this() {}

    /// Creates an instance and start to visit from node.
    this(UnitContainerAstNode node)
    {
        if (node)
            visit(node);
    }

    void visit(AstNode node){assert(node);node.accept(this);}
    void visit(ClassDeclarationAstNode node){assert(node);node.accept(this);}
    void visit(DeclarationAstNode node){ assert(node);node.DeclarationAstNode.accept(this);}
    void visit(FunctionDeclarationAstNode node){assert(node);node.accept(this);}
    void visit(FunctionHeaderAstNode node){assert(node);node.accept(this);}
    void visit(FunctionTypeAstNode node){assert(node);node.accept(this);}
    void visit(ImportDeclarationAstNode node){assert(node);node.accept(this);}
    void visit(LiteralAstNode node){assert(node);node.accept(this);}
    void visit(ProtectionDeclarationAstNode node){assert(node);node.accept(this);}
    void visit(ScopeDeclarationAstNode node){assert(node);node.accept(this);}
    void visit(StructDeclarationAstNode node){assert(node);node.accept(this);}
    void visit(TypeAstNode node){assert(node);node.accept(this);}
    void visit(TypedVariableListAstNode node){assert(node);node.accept(this);}
    void visit(TypeModifierAstNode node){assert(node);node.accept(this);}
    void visit(UnitAstNode node){assert(node);node.accept(this);}
    void visit(UnitContainerAstNode node){assert(node);node.accept(this);}
}

/// The base AST node.
class AstNode
{
    /// Gets visited by an AstVisitor.
    void accept(AstVisitor visitor) {}
    /// Returns: $(D true) if the node matches to a grammar rule.
    bool isGrammatic() {return false;}
    /// Returns: $(D true) if the node has no children.
    bool isTerminal() {return false;}

    @Semantic
    {
        /// Indicates if this node represents something public.
        bool isPublic;
        /// Indicates $(D true) if this node represents something private.
        bool isPrivate;
        /// Indicates $(D true) if this node represents something private.
        bool isProtected;
        /// Indicates $(D true) if this node represents something protected.
    }
}

/// LiteralAstNode
class LiteralAstNode: AstNode
{

private:

    double _asFloat;
    ulong  _asInt;
    bool _cached;

    void tryCacheValue()
    {
        if (!_cached)
        {
            try _asInt = to!ulong(literal.text());
            catch(ConvException) _asInt = 0;
            try _asFloat = to!double(literal.text());
            catch(ConvException) _asFloat = 0.0;
        }
        _cached = true;
    }

protected:

public:

    /// The token that gives the type of literal.
    Token* literalType;
    /// The token that gives the literal text.
    Token* literal;

    /// Creates a new instance with a token that gives the literal and the
    /// one that gives the type.
    this (Token* literal, Token* type)
    {
        this.literal = literal;
        this.literalType = type;
    }

    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return true;}

    /// Returns: The literal interpreted as a u8.
    ubyte asU8(){tryCacheValue(); return cast(ubyte) _asInt;}
    /// Returns: The literal interpreted as a u16.
    ushort asU16(){tryCacheValue(); return cast(ushort) _asInt;}
    /// Returns: The literal interpreted as a u32.
    uint asU32(){tryCacheValue(); return cast(uint) _asInt;}
    /// Returns: The literal interpreted as a u64.
    ulong asU64(){tryCacheValue(); return _asInt;}

    /// Returns: The literal interpreted as a s8.
    byte asS8(){tryCacheValue(); return cast(byte) _asInt;}
    /// Returns: The literal interpreted as a s16.
    short asS16(){tryCacheValue(); return cast(short) _asInt;}
    /// Returns: The literal interpreted as a s32.
    int asS32(){tryCacheValue(); return cast(int) _asInt;}
    /// Returns: The literal interpreted as a s64.
    long asS64(){tryCacheValue(); return _asInt;}

    /// Returns: The literal interpreted as a f32.
    float asF32(){tryCacheValue(); return cast(float) _asFloat;}
    /// Returns: The literal interpreted as a f64.
    double asF64(){tryCacheValue(); return _asFloat;}
}

/// FunctionType
class FunctionTypeAstNode: AstNode
{
    /// Indicates wether the function type needs a context.
    bool isStatic;
    /// The function parameters;
    TypedVariableListAstNode[] parameters;
    /// The function return
    TypeAstNode returnType;
    ///
    override void accept(AstVisitor visitor)
    {
        parameters.each!(a => visitor.visit(a));
        if (returnType)
            visitor.visit(returnType);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// FunctionDeclaration
class FunctionHeaderAstNode: AstNode
{
    /// The function name.
    Token* name;
    /// The function parameters
    TypedVariableListAstNode[] parameters;
    /// The function return
    TypeAstNode returnType;
    ///
    bool isStatic;
    ///
    override void accept(AstVisitor visitor)
    {
        parameters.each!(a => visitor.visit(a));
        if (returnType)
            visitor.visit(returnType);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// FunctionDeclaration
class FunctionDeclarationAstNode: AstNode
{
    /// The function header.
    FunctionHeaderAstNode header;
    /// Used to indicates the body kind.
    Token* firstBodyToken;
    /// The body.
    DeclarationAstNode[] declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        if (header)
            visitor.visit(header);
        declarations.each!(a => visitor.visit(a));
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
    /// Returns: $(D true) if the function is implemented.
    bool isImplemented() {return firstBodyToken.isTokLeftCurly;}
}

/// ImportDeclaration, list of prioritized imports.
class ImportDeclarationAstNode: AstNode
{
    /// The imports priority.
    LiteralAstNode priority;
    /// An array of tokens chain, each represents a unit to import.
    Token*[][] importList;
    ///
    override void accept(AstVisitor visitor) {}
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return true;}
}

/// StructDeclaration
class StructDeclarationAstNode: AstNode
{
    /// The struct name.
    Token* name;
    /// The declarations located in the struct.
    DeclarationAstNode[] declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        declarations.each!(a => visitor.visit(a));
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// ClassDeclaration
class ClassDeclarationAstNode: AstNode
{
    /// The struct name.
    Token* name;
    /// The declarations located in the class.
    DeclarationAstNode[] declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        declarations.each!(a => visitor.visit(a));
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// Scope
class ScopeDeclarationAstNode: AstNode
{
    /// The declarations located in the scope.
    DeclarationAstNode[] declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        declarations.each!(a => visitor.visit(a));
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// ProtectionAttribute
class ProtectionDeclarationAstNode: AstNode
{
    /// The token that specifies the new protection.
    Token* protection;
    ///
    override void accept(AstVisitor visitor) {}
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return true;}
}

class DeclarationAstNode: AstNode
{
    /// Assigned if this declaration is a FunctionDeclaration.
    FunctionDeclarationAstNode functionDeclaration;
    /// Assigned if this declaration is an ImportDeclarationAstNode.
    ImportDeclarationAstNode importDeclaration;
    /// Assigned if this declaration is an ProtectionOverwriteAstNode.
    ProtectionDeclarationAstNode protectionOverwrite;
    /// Assigned if this declaration is an ClassDeclarationAstNode.
    ClassDeclarationAstNode classDeclaration;
    /// Assigned if this declaration is an StructDeclarationAstNode.
    StructDeclarationAstNode structDeclaration;
    /// Assigned if this declaration is a Scope.
    ScopeDeclarationAstNode scopeDeclaration;
    ///
    override void accept(AstVisitor visitor)
    {
        if (importDeclaration)
            visitor.visit(importDeclaration);
        else if (protectionOverwrite)
            visitor.visit(protectionOverwrite);
        else if (classDeclaration)
            visitor.visit(classDeclaration);
        else if (structDeclaration)
            visitor.visit(structDeclaration);
        else if (scopeDeclaration)
            visitor.visit(scopeDeclaration);
        else if (functionDeclaration)
            visitor.visit(functionDeclaration);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return false;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// TypedVariableList
class TypedVariableListAstNode: AstNode
{
    /// The variables list.
    Token*[] variableList;
    /// The variables common type.
    TypeAstNode type;
    ///
    override void accept(AstVisitor visitor)
    {
        if (type)
            visitor.visit(type);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// Type
class TypeAstNode: AstNode
{
    /// The basic type or a qualified custom type
    Token*[] basicOrQualifiedType;
    /// If the type is a function, then assigned.
    FunctionTypeAstNode functionType;
    /// The first modifier.
    TypeModifierAstNode modifier;
    ///
    override void accept(AstVisitor visitor)
    {
        if (modifier)
            visitor.visit(modifier);
        if (functionType)
            visitor.visit(functionType);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// Describes the type modifiers.
enum ModifierKind
{
    /// modified by nested "[]"
    array,
    /// modified by consecutive "*"
    pointer,
}

/// TypeModifier
class TypeModifierAstNode: AstNode
{
    /// The modifier kind.
    ModifierKind kind;
    /// The count of modifiers.
    size_t count;
    /// Next modifications, always of a different kind.
    TypeModifierAstNode modifier;
    ///
    override void accept(AstVisitor visitor)
    {
        if (modifier)
            visitor.visit(modifier);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// Either a MainUnit or a VirtualUnit
class UnitAstNode: AstNode
{
    /// The chain of tokens used in the UnitDeclaration.
    Token*[] unitDeclaration;
    /// When the unit is virtual, this is a reference to the MainUnit.
    UnitAstNode mainUnit; //!\\ not to visit //!\\
    /// The declarations located in the unit.
    DeclarationAstNode[] declarations;
    /// Indicates if this is a VirtualUnit.
    final bool isVirtual() const {return mainUnit !is null;}
    /// Indicates if this is a MainUnit.
    final bool isMain() const {return !isVirtual;}
    ///
    override void accept(AstVisitor visitor)
    {
        declarations.each!(a => visitor.visit(a));
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}

}

/// The AST root node
class UnitContainerAstNode: AstNode
{
    /// The main unit.
    UnitAstNode mainUnit;
    /// The virtual units.
    UnitAstNode[] virtualUnits;
    ///
    override void accept(AstVisitor visitor)
    {
        if (mainUnit)
            visitor.visit(mainUnit);
        virtualUnits.each!(a => visitor.visit(a));
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

