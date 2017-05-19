module yatol.parser.ast;

import
    std.conv, std.algorithm.iteration, std.meta, std.stdio;
import
    yatol.lexer.types;

/// Used to annotate the fields set by the semantic.
enum Semantic;

private string astNodesClasses()
{
    import std.algorithm.sorting: sort;
    string result = "private alias AstNodesSeq = AliasSeq!(";
    foreach (member; __traits(allMembers, yatol.parser.ast))
    {
        if (member.length > 6 && member[$-7..$] == "AstNode")
            result ~= member ~ ",\n";
    }
    return result ~ ");";
}

mixin(astNodesClasses);

/// Sequence of alias that contains each AST node.
alias AstNodes = AstNodesSeq;

/**
 * Generates all the $(D visit()) overrides as a text ready to be mixed
 * in an $(D AstVisitor()) class.
 *
 * Params:
 *      statements = The statements called in each function.
 */
string genVisitMethods(string statements)
{
    string result;
    foreach (Node; AstNodes)
    {
        result ~= "override void visit(" ~ Node.stringof ~ " node)\n" ~
            "{\n    " ~ statements ~ "\n}\n";
    }
    return result;
}

/**
 * The AST visitor.
 * By default, every node is accepted.
 */
class AstVisitor
{
    void visit(AssignExpressionAstNode node){node.accept(this);}
    void visit(AstNode node){node.accept(this);}
    void visit(BinaryExpressionAstNode node){node.accept(this);}
    void visit(CallParametersAstNode node){node.accept(this);}
    void visit(CastExpressionAstNode node){node.accept(this);}
    void visit(ClassDeclarationAstNode node){node.accept(this);}
    void visit(DeclarationAstNode node){node.accept(this);}
    void visit(DeclarationOrStatementAstNode node){node.accept(this);}
    void visit(EmptyStatementAstNode node){node.accept(this);}
    void visit(ExpressionAstNode node){node.accept(this);}
    void visit(ExpressionStatementAstNode node){node.accept(this);}
    void visit(FunctionDeclarationAstNode node){node.accept(this);}
    void visit(FunctionHeaderAstNode node){node.accept(this);}
    void visit(FunctionTypeAstNode node){node.accept(this);}
    void visit(ImportDeclarationAstNode node){node.accept(this);}
    void visit(IndexExpressionAstNode node){node.accept(this);}
    void visit(InterfaceDeclarationAstNode node){node.accept(this);}
    void visit(NumberLiteralAstNode node){node.accept(this);}
    void visit(ParenExpressionAstNode node){node.accept(this);}
    void visit(ProtectionDeclarationAstNode node){node.accept(this);}
    void visit(RangeExpressionAstNode node){node.accept(this);}
    void visit(ScopeDeclarationAstNode node){node.accept(this);}
    void visit(StatementAstNode node){node.accept(this);}
    void visit(StructDeclarationAstNode node){node.accept(this);}
    void visit(TypeAstNode node){node.accept(this);}
    void visit(TypeModifierAstNode node){node.accept(this);}
    void visit(TypedVariableListAstNode node){node.accept(this);}
    void visit(UnaryExpressionAstNode node){node.accept(this);}
    void visit(UnitAstNode node){node.accept(this);}
    void visit(UnitContainerAstNode node){node.accept(this);}
}

/**
 * Base for any AST visitor that visit few nodes.
 * Only the unit container and the units are visisted by default so
 * that only the interesting $(D visit()) have to be overridden.
 */
class AstVisitorNone: AstVisitor
{
    override void visit(AssignExpressionAstNode node){}
    override void visit(AstNode node){}
    override void visit(BinaryExpressionAstNode node){}
    override void visit(CallParametersAstNode node){}
    override void visit(CastExpressionAstNode node){}
    override void visit(ClassDeclarationAstNode node){}
    override void visit(DeclarationAstNode node){}
    override void visit(DeclarationOrStatementAstNode node){}
    override void visit(EmptyStatementAstNode node){}
    override void visit(ExpressionAstNode node){}
    override void visit(ExpressionStatementAstNode node){}
    override void visit(FunctionDeclarationAstNode node){}
    override void visit(FunctionHeaderAstNode node){}
    override void visit(FunctionTypeAstNode node){}
    override void visit(ImportDeclarationAstNode node){}
    override void visit(IndexExpressionAstNode node){}
    override void visit(InterfaceDeclarationAstNode node){}
    override void visit(NumberLiteralAstNode node){}
    override void visit(ParenExpressionAstNode node){}
    override void visit(ProtectionDeclarationAstNode node){}
    override void visit(RangeExpressionAstNode node){}
    override void visit(ScopeDeclarationAstNode node){}
    override void visit(StatementAstNode node){}
    override void visit(StructDeclarationAstNode node){}
    override void visit(TypeAstNode node){}
    override void visit(TypeModifierAstNode node){}
    override void visit(TypedVariableListAstNode node){}
    override void visit(UnaryExpressionAstNode node){}
    override void visit(UnitAstNode node){node.accept(this);}
    override void visit(UnitContainerAstNode node){node.accept(this);}
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

    /// Indicates if this node represents something public.
    @Semantic bool isPublic;
    /// Indicates $(D true) if this node represents something private.
    @Semantic bool isPrivate;
    /// Indicates $(D true) if this node represents something protected.
    @Semantic bool isProtected;
}

unittest
{
    import std.traits;
    static assert(hasUDA!(AstNode.isPrivate, Semantic));
    static assert(hasUDA!(FunctionDeclarationAstNode.isPublic, Semantic));
}

/// LiteralAstNode
class NumberLiteralAstNode: AstNode
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
    DeclarationOrStatementAstNode[] declarationsOrStatements;
    ///
    override void accept(AstVisitor visitor)
    {
        if (header)
            visitor.visit(header);
        declarationsOrStatements.each!(a => visitor.visit(a));
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
    NumberLiteralAstNode priority;
    /// An array of tokens chain, each represents a unit to import.
    Token*[][] importList;
    ///
    override void accept(AstVisitor visitor)
    {
        if (priority)
            visitor.visit(priority);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
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
    /// The class name.
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

/// InterfaceDeclaration
class InterfaceDeclarationAstNode: AstNode
{
    /// The interface name.
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

/// Declaration
class DeclarationAstNode: AstNode
{
    /// Assigned if this declaration is a FunctionDeclaration.
    FunctionDeclarationAstNode functionDeclaration;
    /// Assigned if this declaration is an ImportDeclarationAstNode.
    ImportDeclarationAstNode importDeclaration;
    /// Assigned if this declaration is a ProtectionDeclarationAstNode.
    ProtectionDeclarationAstNode protectionOverwrite;
    /// Assigned if this declaration is an InterfaceDeclarationAstNode.
    InterfaceDeclarationAstNode interfaceDeclaration;
    /// Assigned if this declaration is a ClassDeclarationAstNode.
    ClassDeclarationAstNode classDeclaration;
    /// Assigned if this declaration is a StructDeclarationAstNode.
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
        else if (interfaceDeclaration)
            visitor.visit(interfaceDeclaration);
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

class CallParametersAstNode: AstNode
{
    /// The parameters
    ExpressionAstNode[] parameters;
    ///
    override void accept(AstVisitor visitor)
    {
        parameters.each!(a => visitor.visit(a));
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// UnaryExpression
class UnaryExpressionAstNode: AstNode
{
    /// the expression prefix.
    Token* prefix;
    /// the expression suffix.
    Token* suffix;
    /// the nested unary expression.
    UnaryExpressionAstNode unary;
    /// Assigned when no identifierChain.
    NumberLiteralAstNode numberLitteral;
    /// Assigned when no numberLitteral.
    Token*[] identifierChain;
    /// Assigned if this unary is a function call
    CallParametersAstNode callParameters;
    ///
    override void accept(AstVisitor visitor)
    {
        if (unary)
            visitor.visit(unary);
        if (numberLitteral)
            visitor.visit(numberLitteral);
        if (callParameters)
            visitor.visit(callParameters);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// ExpressionStatement
class ExpressionStatementAstNode: AstNode
{
    /// The expression.
    ExpressionAstNode expression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// Expression
class ExpressionAstNode: AstNode
{
    /// Assigned if this expression is an AssignExpression.
    AssignExpressionAstNode assignExpression;
    /// Assigned if this expression is a BinaryExpression.
    BinaryExpressionAstNode binaryExpression;
    /// Assigned if this expression is an IndexExpression.
    IndexExpressionAstNode indexExpression;
    /// Assigned if this expression is a RangeExpression.
    RangeExpressionAstNode rangeExpression;
    /// Assigned if this expression is a ParenExpression.
    ParenExpressionAstNode parenExpression;
    /// Assigned if this expression is an UnaryExpression
    UnaryExpressionAstNode unaryExpression;
    /// Assigned if this expression is a CastExpression
    CastExpressionAstNode castExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (assignExpression)
            visitor.visit(assignExpression);
        else if (binaryExpression)
            visitor.visit(binaryExpression);
        else if (indexExpression)
            visitor.visit(indexExpression);
        else if (rangeExpression)
            visitor.visit(rangeExpression);
        else if (parenExpression)
            visitor.visit(parenExpression);
        else if (unaryExpression)
            visitor.visit(unaryExpression);
        else if (castExpression)
            visitor.visit(castExpression);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// AssignExpression
class AssignExpressionAstNode: AstNode
{
    /// The equal LHS.
    ExpressionAstNode left;
    /// The equal RHS.
    ExpressionAstNode right;
    ///
    override void accept(AstVisitor visitor)
    {
        if (left)
            visitor.visit(left);
        if (right)
            visitor.visit(right);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// BinaryExpression
class BinaryExpressionAstNode: AstNode
{
    /// The operator.
    Token* operator;
    /// The operator LHS.
    ExpressionAstNode left;
    /// The operator RHS.
    ExpressionAstNode right;
    ///
    override void accept(AstVisitor visitor)
    {
        if (left)
            visitor.visit(left);
        if (right)
            visitor.visit(right);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// IndexExpression
class IndexExpressionAstNode: AstNode
{
    /// The expression that's indexed.
    ExpressionAstNode indexed;
    /// The expression that gives the index.
    ExpressionAstNode index;
    ///
    override void accept(AstVisitor visitor)
    {
        if (indexed)
            visitor.visit(indexed);
        if (index)
            visitor.visit(index);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// IndexExpression
class RangeExpressionAstNode: AstNode
{
    /// The expression that's indexed.
    ExpressionAstNode indexed;
    /// The expression that gives the left index.
    ExpressionAstNode left;
    /// The expression that gives the right index.
    ExpressionAstNode right;
    ///
    override void accept(AstVisitor visitor)
    {
        if (indexed)
            visitor.visit(indexed);
        if (left)
            visitor.visit(left);
        if (right)
            visitor.visit(right);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// ParenExpression
class ParenExpressionAstNode: AstNode
{
    /// The surrounded expression
    ExpressionAstNode expression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// CastExpressionAstNode
class CastExpressionAstNode: AstNode
{
    /// The reinterpreted expression
    ExpressionAstNode expression;
    /// The target type
    TypeAstNode type;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
        if (type)
            visitor.visit(type);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
}

/// EmptyStatement
class EmptyStatementAstNode: AstNode
{
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return true;}
}

/// Statement
class StatementAstNode: AstNode
{
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return false;}
    /// Returns: $(D true) if the node has no children.
    override bool isTerminal() {return false;}
    /// Assigned if this statement is an EmptyStatementAstNode.
    EmptyStatementAstNode emptyStatement;
    /// Assigned if this statement is an Expression.
    ExpressionStatementAstNode expression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (emptyStatement)
            visitor.visit(emptyStatement);
        else if (expression)
            visitor.visit(expression);
    }
}

/// DeclarationOrStatement
class DeclarationOrStatementAstNode: AstNode
{
    /// Assigned if this is a declaration
    DeclarationAstNode declaration;
    /// Assigned if this is a statement
    StatementAstNode statement;
    ///
    override void accept(AstVisitor visitor)
    {
        if (declaration)
            visitor.visit(declaration);
        else if (statement)
            visitor.visit(statement);
    }
    /// Returns: $(D true) if the node matches to a grammar rule.
    override bool isGrammatic() {return true;}
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

