module yatol.parser.ast;

import
    std.conv, std.algorithm.iteration, std.meta, std.stdio;
import
    yatol.lexer.types;

/// Used to annotate the fields set by the semantic.
enum Semantic;

private string astNodesClasses()
{
    string result = "private alias AstNodesSeq = AliasSeq!(";
    mixin("alias m = " ~ __MODULE__ ~ ";");
    foreach (member; __traits(allMembers, m))
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
    void visit(AkaDeclarationAstNode node){node.accept(this);}
    void visit(AssignExpressionAstNode node){node.accept(this);}
    void visit(AstNode node){node.accept(this);}
    void visit(BinaryExpressionAstNode node){node.accept(this);}
    void visit(BlockStatementAstNode node){node.accept(this);}
    void visit(BreakStatementAstNode node){node.accept(this);}
    void visit(CallParametersAstNode node){node.accept(this);}
    void visit(ClassDeclarationAstNode node){node.accept(this);}
    void visit(ContinueStatementAstNode node){node.accept(this);}
    void visit(DeclarationAstNode node){node.accept(this);}
    void visit(DeclarationOrStatementAstNode node){node.accept(this);}
    void visit(DotExpressionAstNode node){node.accept(this);}
    void visit(EmptyStatementAstNode node){node.accept(this);}
    void visit(EnumDeclarationAstNode node){node.accept(this);}
    void visit(EnumMemberAstNode node){node.accept(this);}
    void visit(ExpressionAstNode node){node.accept(this);}
    void visit(ExpressionStatementAstNode node){node.accept(this);}
    void visit(ForeachStatementAstNode node){node.accept(this);}
    void visit(FunctionDeclarationAstNode node){node.accept(this);}
    void visit(FunctionHeaderAstNode node){node.accept(this);}
    void visit(FunctionParameterGroupAstNode node){node.accept(this);}
    void visit(FunctionTypeAstNode node){node.accept(this);}
    void visit(IdentifierChainsAstNode node){node.accept(this);}
    void visit(IfConditionVariableAstNode node){node.accept(this);}
    void visit(IfElseStatementAstNode node){node.accept(this);}
    void visit(ImportDeclarationAstNode node){node.accept(this);}
    void visit(IndexExpressionAstNode node){node.accept(this);}
    void visit(InterfaceDeclarationAstNode node){node.accept(this);}
    void visit(NumberLiteralAstNode node){node.accept(this);}
    void visit(ParenExpressionAstNode node){node.accept(this);}
    void visit(PostfixExpressionAstNode node){node.accept(this);}
    void visit(ProtectionDeclarationAstNode node){node.accept(this);}
    void visit(RangeExpressionAstNode node){node.accept(this);}
    void visit(ReturnStatementAstNode node){node.accept(this);}
    void visit(SingleStatementOrBlockAstNode node){node.accept(this);}
    void visit(StatementAstNode node){node.accept(this);}
    void visit(StructDeclarationAstNode node){node.accept(this);}
    void visit(Token* token){}
    void visit(TypeAstNode node){node.accept(this);}
    void visit(TypeModifierAstNode node){node.accept(this);}
    void visit(UnaryExpressionAstNode node){node.accept(this);}
    void visit(UnitAstNode node){node.accept(this);}
    void visit(UnitContainerAstNode node){node.accept(this);}
    void visit(VariableDeclarationAstNode node){node.accept(this);}
    void visit(VariableDeclarationItemAstNode node){node.accept(this);}
    void visit(WhileStatementAstNode node){node.accept(this);}
}

/**
 * Base for any AST visitor that visit few nodes.
 * Only the unit container and the units are visisted by default so
 * that only the interesting $(D visit()) have to be overridden.
 */
class AstVisitorNone: AstVisitor
{
    override void visit(AkaDeclarationAstNode node){}
    override void visit(AssignExpressionAstNode node){}
    override void visit(AstNode node){}
    override void visit(BinaryExpressionAstNode node){}
    override void visit(BlockStatementAstNode node){}
    override void visit(BreakStatementAstNode node){}
    override void visit(CallParametersAstNode node){}
    override void visit(ClassDeclarationAstNode node){}
    override void visit(ContinueStatementAstNode node){}
    override void visit(DeclarationAstNode node){}
    override void visit(DeclarationOrStatementAstNode node){}
    override void visit(DotExpressionAstNode node){}
    override void visit(EmptyStatementAstNode node){}
    override void visit(EnumDeclarationAstNode node){}
    override void visit(EnumMemberAstNode node){}
    override void visit(ExpressionAstNode node){}
    override void visit(ExpressionStatementAstNode node){}
    override void visit(ForeachStatementAstNode node){}
    override void visit(FunctionDeclarationAstNode node){}
    override void visit(FunctionHeaderAstNode node){}
    override void visit(FunctionParameterGroupAstNode node){}
    override void visit(FunctionTypeAstNode node){}
    override void visit(IdentifierChainsAstNode node){}
    override void visit(IfConditionVariableAstNode node){}
    override void visit(IfElseStatementAstNode  node){}
    override void visit(ImportDeclarationAstNode node){}
    override void visit(IndexExpressionAstNode node){}
    override void visit(InterfaceDeclarationAstNode node){}
    override void visit(NumberLiteralAstNode node){}
    override void visit(ParenExpressionAstNode node){}
    override void visit(PostfixExpressionAstNode node){}
    override void visit(ProtectionDeclarationAstNode node){}
    override void visit(RangeExpressionAstNode node){}
    override void visit(ReturnStatementAstNode node){}
    override void visit(SingleStatementOrBlockAstNode node){}
    override void visit(StatementAstNode node){}
    override void visit(StructDeclarationAstNode node){}
    override void visit(TypeAstNode node){}
    override void visit(TypeModifierAstNode node){}
    override void visit(UnaryExpressionAstNode node){}
    override void visit(UnitAstNode node){node.accept(this);}
    override void visit(UnitContainerAstNode node){node.accept(this);}
    override void visit(VariableDeclarationAstNode node){}
    override void visit(VariableDeclarationItemAstNode node){}
    override void visit(WhileStatementAstNode node){}
}

/// The base AST node.
class AstNode
{
    /// Information about the position
    Position position;
    /// Gets visited by an AstVisitor.
    void accept(AstVisitor visitor) {}
    /// Set to $(D true) if this node represents something public.
    @Semantic bool isPublic;
    /// Set to $(D true) if this node represents something private.
    @Semantic bool isPrivate;
    /// Set $(D true) if this node represents something protected.
    @Semantic bool isProtected;
}


/**
 * Returns: $(D true) if the node passed as parameter matches to a grammar rule
 * and $(D false) if it's a helper node used to propagate fields by inheritance.
 */
enum isGrammatic(T) = T.stringof[$-7..$] == "AstNode";

unittest
{
    import std.traits;
    static assert(hasUDA!(AstNode.isPrivate, Semantic));
    static assert(hasUDA!(FunctionDeclarationAstNode.isPublic, Semantic));
    static assert(isGrammatic!AstNode);
    static assert(!isGrammatic!FlowControlBaseNode);
}

/// IdentifierChains
final class IdentifierChainsAstNode: AstNode
{
    /// The chain of identifiers.
    Token*[][] chains;
}

/// LiteralAstNode
final class NumberLiteralAstNode: AstNode
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

public:

    /// The token that gives the type of literal.
    Token* literalType;
    /// The token that gives the literal text.
    Token* literal;
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
final class FunctionTypeAstNode: AstNode
{
    /// Indicates wether the function type needs a context.
    bool isStatic;
    /// The function parameters;
    FunctionParameterGroupAstNode[] parameters;
    /// The function return
    TypeAstNode returnType;
    ///
    override void accept(AstVisitor visitor)
    {
        parameters.each!(a => visitor.visit(a));
        if (returnType)
            visitor.visit(returnType);
    }
}

/// FunctionDeclaration
final class FunctionHeaderAstNode: AstNode
{
    /// The function name.
    Token* name;
    /// The function parameters
    FunctionParameterGroupAstNode[] parameters;
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
}

/// FunctionDeclaration
final class FunctionDeclarationAstNode: AstNode
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
}

/// ImportDeclaration, list of prioritized imports.
final class ImportDeclarationAstNode: AstNode
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
}

/// StructDeclaration
final class StructDeclarationAstNode: AstNode
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
}

/// EnumMember
final class EnumMemberAstNode: AstNode
{
    /// The enum member name.
    Token* identifier;
    /// The member value.
    ExpressionAstNode value;
    ///
    override void accept(AstVisitor visitor)
    {
        if (value)
            visitor.visit(value);
    }
}

/// EnumDeclaration
final class EnumDeclarationAstNode: AstNode
{
    /// The enum name.
    Token* name;
    /// The enum type
    TypeAstNode type;
    /// The enum members.
    EnumMemberAstNode[] members;
    ///
    override void accept(AstVisitor visitor)
    {
        if (type)
            visitor.visit(type);
        members.each!(a => visitor.visit(a));
    }
}

/// ClassDeclaration
final class ClassDeclarationAstNode: AstNode
{
    /// The class name.
    Token* name;
    /// The inheritance list.
    IdentifierChainsAstNode inheritanceList;
    /// The declarations located in the class.
    DeclarationAstNode[] declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        if (inheritanceList)
            visitor.visit(inheritanceList);
        declarations.each!(a => visitor.visit(a));
    }
}

/// InterfaceDeclaration
final class InterfaceDeclarationAstNode: AstNode
{
    /// The interface name.
    Token* name;
    /// The inheritance list.
    IdentifierChainsAstNode inheritanceList;
    /// The declarations located in the class.
    DeclarationAstNode[] declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        if (inheritanceList)
            visitor.visit(inheritanceList);
        declarations.each!(a => visitor.visit(a));
    }
}

/// ProtectionAttribute
final class ProtectionDeclarationAstNode: AstNode
{
    /// The token that specifies the new protection.
    Token* protection;
    ///
    override void accept(AstVisitor visitor) {}
}

/// VariableDeclarationItem
final class VariableDeclarationItemAstNode: AstNode
{
    /// The expression that gives trhe initial value;
    ExpressionAstNode initiliazer;
    /// The variable name.
    Token* name;
    ///
    override void accept(AstVisitor visitor)
    {
        if (initiliazer)
            visitor.visit(initiliazer);
    }
}

/// VariableDeclaration
final class VariableDeclarationAstNode: AstNode
{
    /// Indicates if the variables in the list are static.
    bool isStatic;
    /// Indicates if the variables in the list are constants.
    bool isConst;
    /// The type of the variables in the list.
    TypeAstNode type;
    /// The list of variables.
    VariableDeclarationItemAstNode[] list;
    ///
    override void accept(AstVisitor visitor)
    {
        if (type)
            visitor.visit(type);
        list.each!(a => visitor.visit(a));
    }
}

/// AkaDeclaration
final class AkaDeclarationAstNode: AstNode
{
    /// The target name.
    Token* name;
    /// The source type or the source symbol.
    TypeAstNode type;
    ///
    override void accept(AstVisitor visitor)
    {
        if (type)
            visitor.visit(type);
    }
}

/// Declaration
final class DeclarationAstNode: AstNode
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
    /// Assigned if this declaration is an EnumDeclarationAstNode.
    EnumDeclarationAstNode enumDeclaration;
    /// Assigned if this declaration is a Scope.
    BlockStatementAstNode declarationBlock;
    /// Assigned if this declaration is a VariableDeclaration.
    VariableDeclarationAstNode variableDeclaration;
    /// Assigned if this declaration is an akaDeclaration.
    AkaDeclarationAstNode akaDeclaration;
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
        else if (enumDeclaration)
            visitor.visit(enumDeclaration);
        else if (declarationBlock)
            visitor.visit(declarationBlock);
        else if (functionDeclaration)
            visitor.visit(functionDeclaration);
        else if (variableDeclaration)
            visitor.visit(variableDeclaration);
        else if (akaDeclaration)
            visitor.visit(akaDeclaration);
    }
}

/// CallParameters
final class CallParametersAstNode: AstNode
{
    /// The parameters
    ExpressionAstNode[] parameters;
    ///
    override void accept(AstVisitor visitor)
    {
        parameters.each!(a => visitor.visit(a));
    }
}

/// PostfixExpressionAst
final class PostfixExpressionAstNode: AstNode
{
    /// Assigned if this postfix is a ++/--.
    Token* plusplusOrMinusMinus;
    /// Assigned if this poststix is an index.
    IndexExpressionAstNode indexExpression;
    /// Assigned if this poststix is a range.
    RangeExpressionAstNode rangeExpression;
    /// Assigned if this postfix is a call.
    CallParametersAstNode callParameters;
    /// Assigned if this postfix is a cast.
    TypeAstNode castToType;
    ///
    override void accept(AstVisitor visitor)
    {
        if (indexExpression)
            visitor.visit(indexExpression);
        else if (rangeExpression)
            visitor.visit(rangeExpression);
        else if (callParameters)
            visitor.visit(callParameters);
        else if (castToType)
            visitor.visit(castToType);
    }
}

/// UnaryExpression
final class UnaryExpressionAstNode: AstNode
{
    /// the expression prefix.
    Token* prefix;
    /// the expression postfixes.
    PostfixExpressionAstNode[] postfixes;
    /// the nested unary expression.
    UnaryExpressionAstNode unary;
    /// Assigned when no identifierChain.
    NumberLiteralAstNode numberLitteral;
    /// Assigned when this unary is a ParenExpression.
    ParenExpressionAstNode parenExpression;
    /// Assigned for a simplified / full super call.
    Token* super_;
    /// Assigned when no numberLitteral / no value keyword / following super (.)?
    Token*[] identifierChain;
    /// Assigned when the unary is a value that's a keyword
    Token* valueKeyword;
    ///
    override void accept(AstVisitor visitor)
    {
        if (unary)
            visitor.visit(unary);
        else if (numberLitteral)
            visitor.visit(numberLitteral);
        else if (parenExpression)
            visitor.visit(parenExpression);

        postfixes.each!(a => visitor.visit(a));
    }
}

/// ExpressionStatement
final class ExpressionStatementAstNode: AstNode
{
    /// The expression.
    AssignExpressionAstNode assignExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (assignExpression)
            visitor.visit(assignExpression);
    }
}

/// ForeachStatement
final class ForeachStatementAstNode: AstNode
{
    /// The variable.
    VariableDeclarationAstNode variable;
    /// The expression that give the enumerable.
    ExpressionAstNode enumerable;
    /// The single statement or block.
    SingleStatementOrBlockAstNode singleStatementOrBlock;
    ///
    override void accept(AstVisitor visitor)
    {
        if (variable)
            visitor.visit(variable);
        if (enumerable)
            visitor.visit(enumerable);
        if (singleStatementOrBlock)
            visitor.visit(singleStatementOrBlock);
    }
}

/// WhileStatement
final class WhileStatementAstNode: AstNode
{
    /// The condition
    ExpressionAstNode condition;
    /// The single statement or block.
    SingleStatementOrBlockAstNode singleStatementOrBlock;
    ///
    override void accept(AstVisitor visitor)
    {
        if (condition)
            visitor.visit(condition);
        if (singleStatementOrBlock)
            visitor.visit(singleStatementOrBlock);
    }
}

/// IfConditionVariable
final class IfConditionVariableAstNode: AstNode
{
    /// Indicates wether the variable is const.
    bool isConst;
    /// The type of the variables in the list.
    TypeAstNode type;
    /// The variable and its initializer.
    VariableDeclarationItemAstNode variable;
    ///
    override void accept(AstVisitor visitor)
    {
        if (type)
            visitor.visit(type);
        if (variable)
            visitor.visit(variable);
    }
}

/// SingleStatementOrBlock
final class SingleStatementOrBlockAstNode: AstNode
{
    /// The single statement.
    DeclarationOrStatementAstNode singleStatement;
    /// The block.
    BlockStatementAstNode block;
    ///
    override void accept(AstVisitor visitor)
    {
        if (singleStatement)
            visitor.visit(singleStatement);
        else if (block)
            visitor.visit(block);
    }
}

/// IfElseStatement
final class IfElseStatementAstNode: AstNode
{
    /// The condition when it's an expression.
    ExpressionAstNode condition;
    /// The consition when it's a new scoped variable.
    IfConditionVariableAstNode ifVariable;
    /// The single statement or block when condition is true.
    SingleStatementOrBlockAstNode trueStatementOrBlock;
    /// The single statement or block when condition is false.
    SingleStatementOrBlockAstNode falseStatementOrBlock;
    ///
    override void accept(AstVisitor visitor)
    {
        if (condition)
            visitor.visit(condition);
        else if (ifVariable)
            visitor.visit(ifVariable);
        if (trueStatementOrBlock)
            visitor.visit(trueStatementOrBlock);
        if (falseStatementOrBlock)
            visitor.visit(falseStatementOrBlock);
    }
}

/// Expression
final class ExpressionAstNode: AstNode
{
    /// Assigned if this expression is a BinaryExpression.
    BinaryExpressionAstNode binaryExpression;
    /// Assigned if this expression is a DotExpression.
    DotExpressionAstNode dotExpression;
    /// Assigned if this expression is an UnaryExpression.
    UnaryExpressionAstNode unaryExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (binaryExpression)
            visitor.visit(binaryExpression);
        else if (dotExpression)
            visitor.visit(dotExpression);
        else if (unaryExpression)
            visitor.visit(unaryExpression);
    }
}

/// AssignExpression
final class AssignExpressionAstNode: AstNode
{
    /// The equal LHS.
    ExpressionAstNode left;
    ///The assignation operator
    Token* operator;
    /// The equal RHS.
    AssignExpressionAstNode right;
    ///
    override void accept(AstVisitor visitor)
    {
        if (left)
            visitor.visit(left);
        if (right)
            visitor.visit(right);
    }
}

/// BinaryExpression
final class BinaryExpressionAstNode: AstNode
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
}

/// DotExpression
final class DotExpressionAstNode : AstNode
{
    /// Assigned if there's an expression before the dot.
    ExpressionAstNode left;
    /// The dot RHS.
    ExpressionAstNode right;
    ///
    override void accept(AstVisitor visitor)
    {
        if (left)
            visitor.visit(left);
        if (right)
            visitor.visit(right);
    }
}

/// IndexExpression
final class IndexExpressionAstNode: AstNode
{
    /// The expression that gives the index.
    ExpressionAstNode index;
    ///
    override void accept(AstVisitor visitor)
    {
        if (index)
            visitor.visit(index);
    }
}

/// IndexExpression
final class RangeExpressionAstNode: AstNode
{
    /// The expression that gives the left index.
    ExpressionAstNode left;
    /// The expression that gives the right index.
    ExpressionAstNode right;
    ///
    override void accept(AstVisitor visitor)
    {
        if (left)
            visitor.visit(left);
        if (right)
            visitor.visit(right);
    }
}

/// ParenExpression
final class ParenExpressionAstNode: AstNode
{
    /// the expression prefix.
    Token* prefix;
    /// The surrounded expression
    ExpressionAstNode expression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
    }
}

/// EmptyStatement
final class EmptyStatementAstNode: AstNode {}

/// BlockStatement
final class BlockStatementAstNode: AstNode
{
    /// Declarations or statement located in the block.
    DeclarationOrStatementAstNode[] declarationsOrStatements;
    ///
    override void accept(AstVisitor visitor)
    {
        declarationsOrStatements.each!(a => visitor.visit(a));
    }
}

/// ReturnStatement
class FlowControlBaseNode: AstNode
{
    /// The expression that gives the return or executed before the break/continue
    AssignExpressionAstNode expression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
    }
}

/// ReturnStatement
final class ReturnStatementAstNode: FlowControlBaseNode {}

/// ContinueStatement
final class ContinueStatementAstNode: FlowControlBaseNode {}

/// BreakStatement
final class BreakStatementAstNode: FlowControlBaseNode
{
    /// The token that indicates the label to go to.
    Token* label;
}

/// Statement
final class StatementAstNode: AstNode
{
    /// Assigned if this statement is an EmptyStatementAstNode.
    EmptyStatementAstNode emptyStatement;
    /// Assigned if this statement is an Expression.
    ExpressionStatementAstNode expression;
    /// Assigned if this statement is a ReturnStatement.
    ReturnStatementAstNode returnStatement;
    /// Assigned if this statement is a BreakStatement.
    BreakStatementAstNode breakStatement;
    /// Assigned if this statement is a ContinueStatement.
    ContinueStatementAstNode continueStatement;
    /// Assigned if this is a block statement.
    BlockStatementAstNode block;
    /// Assigned if this statement is an IfElseStatement.
    IfElseStatementAstNode ifElseStatement;
    /// Assigned if this statement is a WhileStatement.
    WhileStatementAstNode whileStatement;
    /// Assigned if this statement is a ForeachStatement.
    ForeachStatementAstNode foreachStatement;
    ///
    override void accept(AstVisitor visitor)
    {
        if (emptyStatement)
            visitor.visit(emptyStatement);
        else if (expression)
            visitor.visit(expression);
        else if (returnStatement)
            visitor.visit(returnStatement);
        else if (breakStatement)
            visitor.visit(breakStatement);
        else if (continueStatement)
            visitor.visit(continueStatement);
        else if (block)
            visitor.visit(block);
        else if (ifElseStatement)
            visitor.visit(ifElseStatement);
        else if (whileStatement)
            visitor.visit(whileStatement);
        else if (foreachStatement)
            visitor.visit(foreachStatement);
    }
}

/// DeclarationOrStatement
final class DeclarationOrStatementAstNode: AstNode
{
    /// Assigned if this is a declaration.
    DeclarationAstNode declaration;
    /// Assigned if this is a statement.
    StatementAstNode statement;
    ///
    override void accept(AstVisitor visitor)
    {
        if (declaration)
            visitor.visit(declaration);
        else if (statement)
            visitor.visit(statement);
    }
}

/// TypedVariableList
final class FunctionParameterGroupAstNode: AstNode
{
    /// Indicates if the variables are passed by reference.
    bool isVar;
    /// Indicates if the variables are read-only
    bool isConst;
    /// The variables list.
    Token*[] variableList;
    /// The type of group.
    TypeAstNode type;
    ///
    override void accept(AstVisitor visitor)
    {
        if (type)
            visitor.visit(type);
    }
}

/// Type
final class TypeAstNode: AstNode
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
}

/// Describes the type modifiers.
enum ModifierKind
{
    none,
    /// modified by "[]"
    arrayDynDim,
    /// modified by a static array dimension. "[<staticDimension>]"
    arrayStatDim,
    /// modified by "*"
    pointer,
}

/// TypeModifier
final class TypeModifierAstNode: AstNode
{
    /// The modifier kind.
    ModifierKind kind;
    /// The expression that gives the dimension when $(D kind) is set to $(D arrayStatDim).
    ExpressionAstNode staticDimension;
    /// Assigned if there are more modifiers.
    TypeModifierAstNode modifier;
    ///
    override void accept(AstVisitor visitor)
    {
        if (staticDimension)
            visitor.visit(staticDimension);
        if (modifier)
            visitor.visit(modifier);
    }
}

/// Either a MainUnit or a VirtualUnit
final class UnitAstNode: AstNode
{
    /// The chain of tokens used in the UnitDeclaration.
    Token*[] unitDeclaration;
    /// When the unit is virtual, this is a reference to the MainUnit.
    UnitAstNode mainUnit; //!\\ not to visit //!\\
    /// The declarations located in the unit.
    DeclarationAstNode[] declarations;
    /// Indicates if this is a VirtualUnit.
    bool isVirtual() const {return mainUnit !is null;}
    /// Indicates if this is a MainUnit.
    bool isMain() const {return !isVirtual;}
    ///
    override void accept(AstVisitor visitor)
    {
        declarations.each!(a => visitor.visit(a));
    }
}

/// The AST root node
final class UnitContainerAstNode: AstNode
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
}

