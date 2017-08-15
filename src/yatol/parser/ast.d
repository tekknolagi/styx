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
    void visit(AtAttributeAstNode node){node.accept(this);}
    void visit(BinaryExpressionAstNode node){node.accept(this);}
    void visit(BlockStatementAstNode node){node.accept(this);}
    void visit(BreakStatementAstNode node){node.accept(this);}
    void visit(CallParametersAstNode node){node.accept(this);}
    void visit(ClassDeclarationAstNode node){node.accept(this);}
    void visit(ContinueStatementAstNode node){node.accept(this);}
    void visit(DeclarationAstNode node){node.accept(this);}
    void visit(DeclarationOrStatementAstNode node){node.accept(this);}
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
    void visit(InitializerAstNode node){node.accept(this);}
    void visit(InterfaceDeclarationAstNode node){node.accept(this);}
    void visit(OnExceptionInstanceAstNode node){node.accept(this);}
    void visit(OnExceptionStatementAstNode node){node.accept(this);}
    void visit(OnMatchStatementAstNode node){node.accept(this);}
    void visit(ParenExpressionAstNode node){node.accept(this);}
    void visit(PostfixExpressionAstNode node){node.accept(this);}
    void visit(PrimaryExpressionAstNode node){node.accept(this);}
    void visit(ProtectionDeclarationAstNode node){node.accept(this);}
    void visit(ReturnStatementAstNode node){node.accept(this);}
    void visit(SingleOrRangeExpressionAstNode node){node.accept(this);}
    void visit(SingleStatementOrBlockAstNode node){node.accept(this);}
    void visit(SliceExpressionAstNode node){node.accept(this);}
    void visit(StatementAstNode node){node.accept(this);}
    void visit(StructDeclarationAstNode node){node.accept(this);}
    void visit(SwitchStatementAstNode node){node.accept(this);}
    void visit(ThrowStatementAstNode node){node.accept(this);}
    void visit(Token* token){}
    void visit(TryOnFinallyStatementAstNode node){node.accept(this);}
    void visit(TypeAstNode node){node.accept(this);}
    void visit(TypeModifierAstNode node){node.accept(this);}
    void visit(UnaryExpressionAstNode node){node.accept(this);}
    void visit(UnitAstNode node){node.accept(this);}
    void visit(UnitContainerAstNode node){node.accept(this);}
    void visit(VariableDeclarationAstNode node){node.accept(this);}
    void visit(VariableDeclarationItemAstNode node){node.accept(this);}
    void visit(VersionBlockDeclarationAstNode node){node.accept(this);}
    void visit(VersionAndExpressionAstNode node){node.accept(this);}
    void visit(VersionOrExpressionAstNode node){node.accept(this);}
    void visit(VersionParenExpressionAstNode node){node.accept(this);}
    void visit(VersionPrimaryExpressionAstNode node){node.accept(this);}
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
    override void visit(AtAttributeAstNode node){}
    override void visit(BinaryExpressionAstNode node){}
    override void visit(BlockStatementAstNode node){}
    override void visit(BreakStatementAstNode node){}
    override void visit(CallParametersAstNode node){}
    override void visit(ClassDeclarationAstNode node){}
    override void visit(ContinueStatementAstNode node){}
    override void visit(DeclarationAstNode node){}
    override void visit(DeclarationOrStatementAstNode node){}
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
    override void visit(InitializerAstNode node){}
    override void visit(InterfaceDeclarationAstNode node){}
    override void visit(OnExceptionInstanceAstNode node){}
    override void visit(OnExceptionStatementAstNode node){}
    override void visit(OnMatchStatementAstNode node){}
    override void visit(ParenExpressionAstNode node){}
    override void visit(PostfixExpressionAstNode node){}
    override void visit(PrimaryExpressionAstNode node){}
    override void visit(ProtectionDeclarationAstNode node){}
    override void visit(ReturnStatementAstNode node){}
    override void visit(SingleOrRangeExpressionAstNode node){}
    override void visit(SingleStatementOrBlockAstNode node){}
    override void visit(SliceExpressionAstNode node){}
    override void visit(StatementAstNode node){}
    override void visit(StructDeclarationAstNode node){}
    override void visit(SwitchStatementAstNode node){}
    override void visit(ThrowStatementAstNode node){}
    override void visit(TryOnFinallyStatementAstNode node){}
    override void visit(TypeAstNode node){}
    override void visit(TypeModifierAstNode node){}
    override void visit(UnaryExpressionAstNode node){}
    override void visit(UnitAstNode node){node.accept(this);}
    override void visit(UnitContainerAstNode node){node.accept(this);}
    override void visit(VariableDeclarationAstNode node){}
    override void visit(VariableDeclarationItemAstNode node){}
    override void visit(VersionBlockDeclarationAstNode node){}
    override void visit(VersionAndExpressionAstNode node){}
    override void visit(VersionOrExpressionAstNode node){}
    override void visit(VersionParenExpressionAstNode node){}
    override void visit(VersionPrimaryExpressionAstNode node){}
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
    /// Set to a non null value if this node has a matching type.
    //@Semantic TypeAstNode type;
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
    /// The function attributes.
    AtAttributeAstNode[] attributes;
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
        attributes.each!(a => visitor.visit(a));
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
    Token* priority;
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

/// SingleOrRangeExpression
final class SingleOrRangeExpressionAstNode: AstNode
{
    /// The expression or the left expression of the range.
    ExpressionAstNode singleOrLeftExpression;
    /// The right expression of the range.
    ExpressionAstNode rightExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (singleOrLeftExpression)
            visitor.visit(singleOrLeftExpression);
        if (rightExpression)
            visitor.visit(rightExpression);
    }
}

/// OnMatchStatement
final class OnMatchStatementAstNode: AstNode
{
    /// The expressions that match.
    SingleOrRangeExpressionAstNode[] onMatchExpressions;
    /// Single Statement or block.
    SingleStatementOrBlockAstNode singleStatementOrBlock;
    ///
    override void accept(AstVisitor visitor)
    {
        onMatchExpressions.each!(a => visitor.visit(a));
        if (singleStatementOrBlock)
            visitor.visit(singleStatementOrBlock);
    }
}

/// SwitchStatement
final class SwitchStatementAstNode: AstNode
{
    /// The expression to match.
    ExpressionAstNode expression;
    /// The matches.
    OnMatchStatementAstNode[] onMatchStatements;
    /// The fallback for unmatched cases.
    SingleStatementOrBlockAstNode elseStatement;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
        onMatchStatements.each!(a => visitor.visit(a));
        if (elseStatement)
            visitor.visit(elseStatement);
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

/// PrimaryExpression
final class PrimaryExpressionAstNode: AstNode
{
    /// Either an identifier, "super", a value keyword or a literal.
    Token* identifierOrKeywordOrLiteral;
    /// Assigned when the primary is an array literal.
    InitializerAstNode arrayLiteral;
    /// Assigned when no identifierOrKeywordOrLiteral.
    ExpressionAstNode parenExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (arrayLiteral)
            visitor.visit(arrayLiteral);
        else if (parenExpression)
            visitor.visit(parenExpression);
    }
}

/// ProtectionDeclaration
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
    InitializerAstNode initializer;
    /// The variable name.
    Token* name;
    ///
    override void accept(AstVisitor visitor)
    {
        if (initializer)
            visitor.visit(initializer);
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

/// AtAttribute
final class AtAttributeAstNode: AstNode
{
    /// Either an identifier or a keyword
    Token* identifierOrKeyword;
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
    /// Assigned if this declaration is an AkaDeclaration.
    AkaDeclarationAstNode akaDeclaration;
    /// Assigned if this declaration is an VersionBlockDeclaration.
    VersionBlockDeclarationAstNode versionBlockDeclaration;
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
        else if (versionBlockDeclaration)
            visitor.visit(versionBlockDeclaration);
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
    SliceExpressionAstNode sliceExpression;
    /// Assigned if this postfix is a call.
    CallParametersAstNode callParameters;
    /// Assigned if this postfix is a cast.
    TypeAstNode castToType;
    /// Assigned if this postfix is an optional or dotted Primary
    Token* dotOrOptAccess;
    /// Assigned for dotted and optional primary
    PrimaryExpressionAstNode primary;
    ///
    override void accept(AstVisitor visitor)
    {
        if (indexExpression)
            visitor.visit(indexExpression);
        else if (sliceExpression)
            visitor.visit(sliceExpression);
        else if (callParameters)
            visitor.visit(callParameters);
        else if (castToType)
            visitor.visit(castToType);
        else if (primary)
            visitor.visit(primary);
    }
}

/// UnaryExpression
final class UnaryExpressionAstNode: AstNode
{
    /// The expression prefix when there's a nest.
    Token* prefix;
    /// The nested unary expression.
    UnaryExpressionAstNode unary;
    /// The primary expression when there's no nest.
    PrimaryExpressionAstNode primary;
    /// The primary postfixes.
    PostfixExpressionAstNode[] postfixes;
    ///
    override void accept(AstVisitor visitor)
    {
        if (unary)
        {
            visitor.visit(unary);
        }
        else if (primary)
        {
            visitor.visit(primary);
            postfixes.each!(a => visitor.visit(a));
        }
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
    SingleOrRangeExpressionAstNode singleOrRangeExpression;
    /// The single statement or block.
    SingleStatementOrBlockAstNode singleStatementOrBlock;
    ///
    override void accept(AstVisitor visitor)
    {
        if (variable)
            visitor.visit(variable);
        if (singleOrRangeExpression)
            visitor.visit(singleOrRangeExpression);
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
    /// Assigned if this expression is an UnaryExpression.
    UnaryExpressionAstNode unaryExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (binaryExpression)
            visitor.visit(binaryExpression);
        else if (unaryExpression)
            visitor.visit(unaryExpression);
    }
}

/// AssignExpression
final class AssignExpressionAstNode: AstNode
{
    /// The equal LHS.
    ExpressionAstNode left;
    ///The assignement operator
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

/// Initializer
final class InitializerAstNode: AstNode
{
    /// When no arrayInitiliazer the expression that initializes.
    ExpressionAstNode singleInitializer;
    /// Initializes an array.
    InitializerAstNode[] arrayInitializerElements;
    ///
    override void accept(AstVisitor visitor)
    {
        if (singleInitializer)
            visitor.visit(singleInitializer);
        else
            arrayInitializerElements.each!(a => visitor.visit(a));
    }
}

/// SliceExpression
final class SliceExpressionAstNode: AstNode
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
    /// Assigned if this statement is a SwitchStatement.
    SwitchStatementAstNode switchStatement;
    /// Assigned if this statement is a TryStatement.
    TryOnFinallyStatementAstNode tryOnFinallyStatement;
    /// Assigned if this statement is a ThrowStatement.
    ThrowStatementAstNode throwStatement;
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
        else if (switchStatement)
            visitor.visit(switchStatement);
        else if (tryOnFinallyStatement)
            visitor.visit(tryOnFinallyStatement);
        else if (throwStatement)
            visitor.visit(throwStatement);
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

/// ThrowStatement
final class ThrowStatementAstNode: AstNode
{
    /// The unary expression that represents an instance construction
    UnaryExpressionAstNode unary;
    ///
    override void accept(AstVisitor visitor)
    {
        if (unary)
            visitor.visit(unary);
    }
}

/// TryStatement
final class TryOnFinallyStatementAstNode: AstNode
{
    /// The statement to try.
    SingleStatementOrBlockAstNode triedStatementOrBlock;
    /// The Exceptions handlers.
    OnExceptionStatementAstNode[] exceptionStatements;
    /// The final statement.
    SingleStatementOrBlockAstNode finalStatementOrBlock;
    ///
    override void accept(AstVisitor visitor)
    {
        if (triedStatementOrBlock)
            visitor.visit(triedStatementOrBlock);
        exceptionStatements.each!(a => visitor.visit(a));
        if (finalStatementOrBlock)
            visitor.visit(finalStatementOrBlock);
    }
}

/// OnExceptionInstance
final class OnExceptionInstanceAstNode: AstNode
{
    /// The exception type.
    TypeAstNode exceptionType;
    /// The instance identifier.
    Token* identifier;
    ///
    override void accept(AstVisitor visitor)
    {
        visitor.visit(exceptionType);
    }
}

/// OnExceptionStatement
final class OnExceptionStatementAstNode: AstNode
{
    /// The list of exceptions for this case.
    OnExceptionInstanceAstNode[] exceptionsInstances;
    /// The statement or block for these exceptions.
    SingleStatementOrBlockAstNode exceptionsStatementorBlock;
    ///
    override void accept(AstVisitor visitor)
    {
        exceptionsInstances.each!(a => visitor.visit(a));
        if (exceptionsStatementorBlock)
            visitor.visit(exceptionsStatementorBlock);
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

/// VersionBlockDeclaration
final class VersionBlockDeclarationAstNode: AstNode
{
    /// Defines the version(s) for the block or single statement.
    VersionParenExpressionAstNode versionExpression;
    /// The block or single statement when the versionExpression is verified.
    SingleStatementOrBlockAstNode trueDeclarationOrBlock;
    /// The block or single statement when the versionExpression is not verified.
    SingleStatementOrBlockAstNode falseDeclarationOrBlock;
    ///
    override void accept(AstVisitor visitor)
    {
        if (versionExpression)
            visitor.visit(versionExpression);
        if (trueDeclarationOrBlock)
            visitor.visit(trueDeclarationOrBlock);
        if (falseDeclarationOrBlock)
            visitor.visit(falseDeclarationOrBlock);
    }
    /// Indicates wether $(D trueDeclarationOrBlock) or $(D falseDeclarationOrBlock) is the valid branch.
    @Semantic bool isTrue;
}

/// VersionParenExpression
final class VersionParenExpressionAstNode: AstNode
{
    /// The expression between parens.
    VersionOrExpressionAstNode expression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
    }
}

/// VersionOrExpression
final class VersionOrExpressionAstNode: AstNode
{
    /// The LHS or the andExpression when no rightExpression.
    VersionAndExpressionAstNode leftExpression;
    /// The RHS, when assigned there's a pipe operator.
    VersionOrExpressionAstNode rightExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (leftExpression)
            visitor.visit(leftExpression);
        if (rightExpression)
            visitor.visit(rightExpression);
    }
}

/// VersionAndExpression
final class VersionAndExpressionAstNode: AstNode
{
    /// The LHS or the primary when no rightExpression.
    VersionPrimaryExpressionAstNode leftExpression;
    /// The RHS, when assigned there's an ampersand operator.
    VersionAndExpressionAstNode rightExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (leftExpression)
            visitor.visit(leftExpression);
        if (rightExpression)
            visitor.visit(rightExpression);
    }
}

/// VersionPrimaryExpression
final class VersionPrimaryExpressionAstNode: AstNode
{
    /// Assigned when the primary expression is an identifier.
    Token* identifier;
    /// Assigned when the primary expression is a paren expression.
    VersionParenExpressionAstNode parenExpression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (parenExpression)
            visitor.visit(parenExpression);
    }
    /// Returns: If $(D identifier) is set and defined $(D true), otherwise $(D false).
    @Semantic bool isDefined;
}

