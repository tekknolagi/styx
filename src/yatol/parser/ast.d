module yatol.parser.ast;

import
    std.conv, std.algorithm.iteration, std.stdio;
import
    yatol.lexer.types;

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
    void visit(ImportDeclarationAstNode node){assert(node);node.accept(this);}
    void visit(LiteralAstNode node){assert(node);node.accept(this);}
    void visit(ProtectionAttributeAstNode node){assert(node);node.accept(this);}
    void visit(ProtectionOverwriteAstNode node){assert(node);node.accept(this);}
    void visit(StructDeclarationAstNode node){assert(node);node.accept(this);}
    void visit(UnitAstNode node){assert(node);node.accept(this);}
    void visit(UnitContainerAstNode node){assert(node);node.accept(this);}
}

class AstNode
{
    void accept(AstVisitor visitor) {}
}

class LiteralAstNode: AstNode
{

private:

    Token* _literalType;
    Token* _literal;
    double _asFloat;
    ulong  _asInt;
    bool _cached;

    void tryCacheValue()
    {
        if (!_cached)
        {
            try _asInt = to!ulong(_literal.text());
            catch(ConvException) _asInt = 0;
            try _asFloat = to!double(_literal.text());
            catch(ConvException) _asFloat = 0.0;
        }
        _cached = true;
    }

protected:

public:

    this (Token* literal, Token* type)
    {
        _literal = literal;
        _literalType = type;
    }

    ubyte asU8(){tryCacheValue(); return cast(ubyte) _asInt;}

    ushort asU16(){tryCacheValue(); return cast(ushort) _asInt;}

    uint asU32(){tryCacheValue(); return cast(uint) _asInt;}

    ulong asU64(){tryCacheValue(); return _asInt;}

    byte asS8(){tryCacheValue(); return cast(byte) _asInt;}

    short asS16(){tryCacheValue(); return cast(short) _asInt;}

    int asS32(){tryCacheValue(); return cast(int) _asInt;}

    long asS64(){tryCacheValue(); return _asInt;}

    float asF32(){tryCacheValue(); return cast(float) _asFloat;}

    double asF64(){tryCacheValue(); return _asFloat;}
}

/// ImportDeclaration, listo fprioritized imports.
class ImportDeclarationAstNode: DeclarationAstNode
{
    /// The imports priority.
    LiteralAstNode priority;
    /// An array of tokens chain, each represents a unit to import.
    TokensList importList;
    ///
    override void accept(AstVisitor){}
}

/// ProtectionOverwrite
class ProtectionOverwriteAstNode: DeclarationAstNode
{
    /// Creates with the token that speicies the new protection.
    this(Token* protection) {this.protection = protection;}
    /// The token that speicies the new protection.
    Token* protection;
    ///
    override void accept(AstVisitor){}
}

/// StructDeclaration
class StructDeclarationAstNode: DeclarationAstNode
{
    /// The declaration;
    DeclarationAstNode[] declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        declarations.each!(a => visitor.visit(a));
    }
}

/// ClassDeclaration
class ClassDeclarationAstNode: DeclarationAstNode
{
    /// The declaration;
    DeclarationAstNode[] declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        declarations.each!(a => visitor.visit(a));
    }
}

/// ProtectionAttribute (overwrite once)
class ProtectionAttributeAstNode: AstNode
{
    /// Creates with the token that speicies the new protection.
    this(Token* protection) {this.protection = protection;}
    /// The token that speicies the new protection.
    Token* protection;
}

class DeclarationAstNode: AstNode
{
    /// Assigned if this declaration is an ImportDeclarationAstNode
    ImportDeclarationAstNode importDeclaration;
    /// Assigned if this declaration is an ProtectionOverwriteAstNode
    ProtectionOverwriteAstNode protectionOverwrite;
    /// Assigned if this declaration is an ClassDeclarationAstNode
    ClassDeclarationAstNode classDeclaration;
    /// Assigned if this declaration is an StructDeclarationAstNode
    StructDeclarationAstNode structDeclaration;
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
    }
}

/// Either a MainUnit or a VirtualUnit
class UnitAstNode: AstNode
{
    /// The chain of tokens used in the UnitDeclaration
    Tokens unitDeclaration;
    /// When the unit is virtual, this is a reference to the MainUnit
    UnitAstNode mainUnit; //!\\ not to visit //!\\
    /// The declarations
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
}

/// The AST root node
class UnitContainerAstNode: AstNode
{
    /// The main unit
    UnitAstNode mainUnit;
    /// The virtual units
    UnitAstNode[] virtualUnits;
    ///
    override void accept(AstVisitor visitor)
    {
        if (mainUnit)
            visitor.visit(mainUnit);
        virtualUnits.each!(a => visitor.visit(a));
    }
}

