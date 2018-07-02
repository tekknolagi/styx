/**
 * Contains the AST node classes and a visitor class.
 */
module styx.ast;

import
    std.conv, std.algorithm.iteration, std.meta, std.stdio;
import
    styx.token, styx.symbol;

/// Used to annotate the fields set by the semantic.
enum Semantic;

private string astNodesClasses()
{
    string result = "private alias AstNodesSeq = AliasSeq!(";
    mixin("alias m = " ~ __MODULE__ ~ ";");
    foreach (member; __traits(allMembers, m))
    {
        static if (member.length > 6 && member[$-7..$] == "AstNode")
            result ~= member ~ ",\n";
    }
    return result ~ ");";
}

unittest
{
    string s = astNodesClasses();
    string g = genVisitMethods("");
}

mixin(astNodesClasses);

/// Sequence of aliases that contains each AST node.
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
    void visit(AssertStatementAstNode node){node.accept(this);}
    void visit(AssignExpressionAstNode node){node.accept(this);}
    void visit(AstNode node){node.accept(this);}
    void visit(AtAttributeAstNode node){node.accept(this);}
    void visit(BinaryExpressionAstNode node){node.accept(this);}
    void visit(BlockStatementAstNode node){node.accept(this);}
    void visit(BreakStatementAstNode node){node.accept(this);}
    void visit(CallParametersAstNode node){node.accept(this);}
    void visit(ClassDeclarationAstNode node){node.accept(this);}
    void visit(ContinueStatementAstNode node){node.accept(this);}
    void visit(CompilerEchoAstNode node){node.accept(this);}
    void visit(DeclarationAstNode node){node.accept(this);}
    void visit(DeclarationsAstNode node){node.accept(this);}
    void visit(DeclarationOrStatementAstNode node){node.accept(this);}
    void visit(DeclarationsOrStatementsAstNode node){node.accept(this);}
    void visit(EchoParameterAstNode node){node.accept(this);}
    void visit(EmptyStatementAstNode node){node.accept(this);}
    void visit(EnumDeclarationAstNode node){node.accept(this);}
    void visit(EnumMemberAstNode node){node.accept(this);}
    void visit(ExpressionAstNode node){node.accept(this);}
    void visit(ExpressionStatementAstNode node){node.accept(this);}
    void visit(ForeachStatementAstNode node){node.accept(this);}
    void visit(ForeachVariableDeclarationAstNode node){node.accept(this);}
    void visit(FunctionDeclarationAstNode node){node.accept(this);}
    void visit(FunctionParameterGroupAstNode node){node.accept(this);}
    void visit(GotoStatementAstNode node){node.accept(this);}
    void visit(IdentifierChainAstNode node){node.accept(this);}
    void visit(IfConditionVariableAstNode node){node.accept(this);}
    void visit(IfElseStatementAstNode node){node.accept(this);}
    void visit(ImportDeclarationAstNode node){node.accept(this);}
    void visit(IndexExpressionAstNode node){node.accept(this);}
    void visit(InitializerAstNode node){node.accept(this);}
    void visit(InterfaceDeclarationAstNode node){node.accept(this);}
    void visit(LabelDeclarationstAstNode node){node.accept(this);}
    void visit(OnExceptionInstanceAstNode node){node.accept(this);}
    void visit(OnExceptionStatementAstNode node){node.accept(this);}
    void visit(OnMatchStatementAstNode node){node.accept(this);}
    void visit(PostfixExpressionAstNode node){node.accept(this);}
    void visit(PrimaryExpressionAstNode node){node.accept(this);}
    void visit(ProtectionDeclarationAstNode node){node.accept(this);}
    void visit(ReturnStatementAstNode node){node.accept(this);}
    void visit(SingleOrRangeExpressionAstNode node){node.accept(this);}
    void visit(SliceExpressionAstNode node){node.accept(this);}
    void visit(StatementAstNode node){node.accept(this);}
    void visit(StructDeclarationAstNode node){node.accept(this);}
    void visit(SwitchStatementAstNode node){node.accept(this);}
    void visit(TemplateDeclarationAstNode node){node.accept(this);}
    void visit(TemplateInstanceAstNode node){node.accept(this);}
    void visit(TemplateParametersAstNode node){node.accept(this);}
    void visit(ThrowStatementAstNode node){node.accept(this);}
    void visit(Token* token){}
    void visit(TryOnFinallyStatementAstNode node){node.accept(this);}
    void visit(TypeAstNode node){node.accept(this);}
    void visit(TypeIdentifierPartAstNode node){node.accept(this);}
    void visit(TypeModifierAstNode node){node.accept(this);}
    void visit(UnaryExpressionAstNode node){node.accept(this);}
    void visit(UnionDeclarationAstNode node){node.accept(this);}
    void visit(UnitAstNode node){node.accept(this);}
    void visit(VariableDeclarationAstNode node){node.accept(this);}
    void visit(VariableDeclarationItemAstNode node){node.accept(this);}
    void visit(VersionBlockDeclarationAstNode node){node.accept(this);}
    void visit(VersionBlockStatementAstNode node){node.accept(this);}
    void visit(VersionAndExpressionAstNode node){node.accept(this);}
    void visit(VersionOrExpressionAstNode node){node.accept(this);}
    void visit(VersionParenExpressionAstNode node){node.accept(this);}
    void visit(VersionPrimaryExpressionAstNode node){node.accept(this);}
    void visit(WhileStatementAstNode node){node.accept(this);}
}

/// The base AST node.
class AstNode
{
    /// Information about the startPos
    Position startPos;
    /// Gets visited by an AstVisitor.
    void accept(AstVisitor) {}

    /// Set to $(D true) if this node represents something public.
    @Semantic bool isPublic;
    /// Set $(D true) if this node represents something protected.
    @Semantic bool isProtected;
    /// Set to $(D true) if this node represents something private.
    @Semantic bool isPrivate;
    ///  Set to $(D true) if this node represents something strictly private.
    @Semantic bool isStrict;

    /// Associated Symbol
    @Semantic Symbol symbol;
    /// Associated Scope
    @Semantic Scope scope_;
    /// The unit where the node is declared.
    @Semantic Object unit;
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

/// AssertStatement
final class AssertStatementAstNode: AstNode
{
    /// The expression to verify.
    ExpressionAstNode expression;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
    }
}

/// CompilerEcho
final class CompilerEchoAstNode: AstNode
{
    /// The communication command.
    Token* command;
    /// The command arguments.
    EchoParameterAstNode[] parameters;
    ///
    override void accept(AstVisitor visitor)
    {
        parameters.each!(a => visitor.visit(a));
    }
}

/// EchoParaemterAstNode
final class EchoParameterAstNode: AstNode
{
    /// The parameter as an expression.
    ExpressionAstNode expression;
    /// The parameter as a type.
    TypeAstNode type;
    /// The parameter as a keyowrd
    TokenType keyword;
    ///
    override void accept(AstVisitor visitor)
    {
        if (expression)
            visitor.visit(expression);
        else if (type)
            visitor.visit(type);
    }
}

/// IdentifierChain
final class IdentifierChainAstNode: AstNode
{
    /// The identifiers.
    Token*[] chain;
}

/// FunctionDeclaration
final class FunctionDeclarationAstNode: AttributedDeclaration
{
    /// Body ending position.
    Position stopPos;
    /// The function attributes.
    AtAttributeAstNode[] attributes;
    /// The function name.
    Token* name;
    /// The template parameters.
    TemplateParametersAstNode templateParameters;
    /// The function parameters
    FunctionParameterGroupAstNode[] parameters;
    /// The function return
    TypeAstNode returnType;
    ///
    bool isStatic;
    /// Used to indicates the body kind.
    Token* firstBodyToken;
    /// The body.
    DeclarationsOrStatementsAstNode declarationsOrStatements;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        if (templateParameters)
            visitor.visit(templateParameters);
        parameters.each!(a => visitor.visit(a));
        if (returnType)
            visitor.visit(returnType);
        if (declarationsOrStatements)
            visitor.visit(declarationsOrStatements);
    }
}

/// ImportDeclaration, list of prioritized imports.
final class ImportDeclarationAstNode: AttributedDeclaration
{
    /// The imports priority.
    Token* priority;
    /// A list of fully qualified units.
    IdentifierChainAstNode[] importList;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        if (priority)
            visitor.visit(priority);
        importList.each!(a => visitor.visit(a));
    }
}

/// StructDeclaration
final class StructDeclarationAstNode: AttributedDeclaration
{
    /// Body ending position.
    Position stopPos;
    /// The struct name.
    Token* name;
    /// The template parameters.
    TemplateParametersAstNode templateParameters;
    ///
    IdentifierChainAstNode[] duckTypeList;
    /// The declarations located in the struct.
    DeclarationsAstNode declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        duckTypeList.each!(a => visitor.visit(a));
        if (declarations)
            visitor.visit(declarations);
    }
}

/// UnionDeclaration
final class UnionDeclarationAstNode: AttributedDeclaration
{
    /// Body ending position.
    Position stopPos;
    /// The union name.
    Token* name;
    /// The template parameters.
    TemplateParametersAstNode templateParameters;
    /// The declarations located in the union.
    DeclarationsAstNode declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        if (declarations)
            visitor.visit(declarations);
    }
}

/// TemplateDeclaration.
final class TemplateDeclarationAstNode: AttributedDeclaration
{
    /// Body ending position.
    Position stopPos;
    /// The template name.
    Token* name;
    /// The template parameters.
    TemplateParametersAstNode templateParameters;
    /// The templatized declarations.
    DeclarationsAstNode declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        if (templateParameters)
            visitor.visit(templateParameters);
        if (declarations)
            visitor.visit(declarations);
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
    DeclarationOrStatementAstNode declarationOrStatement;
    ///
    override void accept(AstVisitor visitor)
    {
        onMatchExpressions.each!(a => visitor.visit(a));
        if (declarationOrStatement)
            visitor.visit(declarationOrStatement);
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
    DeclarationOrStatementAstNode elseStatement;
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
    Token* name;
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
final class EnumDeclarationAstNode: AttributedDeclaration
{
    /// Body ending position.
    Position stopPos;
    /// The enum name.
    Token* name;
    /// The enum type
    TypeAstNode type;
    /// The enum members.
    EnumMemberAstNode[] members;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        if (type)
            visitor.visit(type);
        members.each!(a => visitor.visit(a));
    }
}

/// ClassDeclaration
final class ClassDeclarationAstNode: AttributedDeclaration
{
    /// Body ending position.
    Position stopPos;
    /// The class name.
    Token* name;
    /// The template parameters.
    TemplateParametersAstNode templateParameters;
    /// The inheritance list.
    IdentifierChainAstNode[] inheritanceList;
    /// The declarations located in the class.
    DeclarationsAstNode declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        inheritanceList.each!(a => visitor.visit(a));
        if (declarations)
            visitor.visit(declarations);
    }
}

/// InterfaceDeclaration
final class InterfaceDeclarationAstNode: AttributedDeclaration
{
    /// Body ending position.
    Position stopPos;
    /// The interface name.
    Token* name;
    /// The template parameters.
    TemplateParametersAstNode templateParameters;
    /// The inheritance list.
    IdentifierChainAstNode[] inheritanceList;
    /// The declarations located in the class.
    DeclarationsAstNode declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        inheritanceList.each!(a => visitor.visit(a));
        if (declarations)
            visitor.visit(declarations);
    }
}

/// PrimaryExpression
final class PrimaryExpressionAstNode: AstNode
{
    /// Either an identifier, "super", a value keyword or a literal.
    Token* identifierOrKeywordOrLiteral;
    /// Specialization of the function called.
    TemplateInstanceAstNode templateInstance;
    /// Assigned when the primary is an array literal.
    InitializerAstNode arrayLiteral;
    /// Assigned when no identifierOrKeywordOrLiteral.
    ExpressionAstNode parenExpression;
    /// Assigned when the primary is a compiletime communication.
    CompilerEchoAstNode compilerEcho;
    ///
    override void accept(AstVisitor visitor)
    {
        if (arrayLiteral)
            visitor.visit(arrayLiteral);
        if (templateInstance)
            visitor.visit(templateInstance);
        else if (compilerEcho)
            visitor.visit(compilerEcho);
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
final class VariableDeclarationAstNode: AttributedDeclaration
{
    /// Indicates if the variables in the list are static.
    bool isStatic;
    /// Indicates if the variables storage class.
    Token* storageClass;
    /// The type of the variables in the list.
    TypeAstNode type;
    /// The list of variables.
    VariableDeclarationItemAstNode[] list;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        if (type)
            visitor.visit(type);
        list.each!(a => visitor.visit(a));
    }
}

/// AkaDeclaration
final class AkaDeclarationAstNode: AttributedDeclaration
{
    /// The target name.
    Token* name;
    /// The source type or the source symbol.
    TypeAstNode type;
    ///
    override void accept(AstVisitor visitor)
    {
        visitAtAttributes(visitor);
        if (type)
            visitor.visit(type);
    }
}

/// AtAttribute
final class AtAttributeAstNode: AstNode
{
    /// The attribute identifier.
    Token* identifierOrKeyword;
    ///
    PrimaryExpressionAstNode[] parameters;
    ///
    override void accept(AstVisitor visitor)
    {
        parameters.each!(a => visitor.visit(a));
    }
}

///
class AttributedDeclaration: AstNode
{
    /// The attributes attached to the final declaration type.
    AtAttributeAstNode[] atAttributes;
    ///
    void visitAtAttributes(AstVisitor visitor)
    {
        atAttributes.each!(a => visitor.visit(a));
    }
}

/// Enumerates the possible declarations.
enum DeclarationKind: ubyte
{
    dkNone,
    dkFunction,
    dkImport,
    dkProtection,
    dkInterface,
    dkClass,
    dkStruct,
    dkEnum,
    dkVariable,
    dkAka,
    dkVersion,
    dkUnion,
    dkTemplate,
    dkLabel,
}

/// Declaration
final class DeclarationAstNode: AstNode
{
    ///
    static union Declaration
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
        /// Assigned if this declaration is a VariableDeclaration.
        VariableDeclarationAstNode variableDeclaration;
        /// Assigned if this declaration is an AkaDeclaration.
        AkaDeclarationAstNode akaDeclaration;
        /// Assigned if this declaration is an VersionBlockDeclaration.
        VersionBlockDeclarationAstNode versionBlockDeclaration;
        /// Assigned if this declaration is an UnionDeclaration
        UnionDeclarationAstNode unionDeclaration;
        /// Assigned if this declaration is a TemplateDeclaration
        TemplateDeclarationAstNode templateDeclaration;
        /// Assigned if this declaration is a LabelDeclaration
        LabelDeclarationstAstNode labelDeclaration;
    }
    ///
    Declaration declaration;
    ///
    DeclarationKind declarationKind;
    ///
    override void accept(AstVisitor visitor)
    {
        with (DeclarationKind) final switch (declarationKind)
        {
        case dkFunction: visitor.visit(declaration.functionDeclaration); break;
        case dkImport: visitor.visit(declaration.importDeclaration); break;
        case dkProtection: visitor.visit(declaration.protectionOverwrite); break;
        case dkInterface: visitor.visit(declaration.interfaceDeclaration); break;
        case dkClass: visitor.visit(declaration.classDeclaration); break;
        case dkStruct: visitor.visit(declaration.structDeclaration); break;
        case dkEnum: visitor.visit(declaration.enumDeclaration); break;
        case dkVariable: visitor.visit(declaration.variableDeclaration); break;
        case dkAka: visitor.visit(declaration.akaDeclaration); break;
        case dkVersion: visitor.visit(declaration.versionBlockDeclaration); break;
        case dkUnion: visitor.visit(declaration.unionDeclaration); break;
        case dkTemplate: visitor.visit(declaration.templateDeclaration); break;
        case dkLabel: visitor.visit(declaration.labelDeclaration); break;
        case dkNone: assert(false);
        }
    }
}

/// Declarations
final class DeclarationsAstNode: AstNode
{
    /// The declarations
    DeclarationAstNode[] items;
    ///
    override void accept(AstVisitor visitor)
    {
        items.each!(a => visitor.visit(a));
    }
}

/// DeclarationsOrStatements
final class DeclarationsOrStatementsAstNode: AstNode
{
    /// The declarations
    DeclarationOrStatementAstNode[] items;
    ///
    override void accept(AstVisitor visitor)
    {
        items.each!(a => visitor.visit(a));
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

/// PostfixExpression
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
    /// The variables.
    ForeachVariableDeclarationAstNode[] variables;
    /// The expression that give the enumerable.
    SingleOrRangeExpressionAstNode singleOrRangeExpression;
    /// The single statement or block.
    DeclarationOrStatementAstNode declarationOrStatement;
    ///
    override void accept(AstVisitor visitor)
    {
        variables.each!(a => visitor.visit(a));
        if (singleOrRangeExpression)
            visitor.visit(singleOrRangeExpression);
        if (declarationOrStatement)
            visitor.visit(declarationOrStatement);
    }
}

/// ForeachVariableDeclaration
final class ForeachVariableDeclarationAstNode: AstNode
{
    /// Indicates if the variable is const or var.
    bool isConst;
    /// The variable type.
    TypeAstNode type;
    /// The variable name.
    Token* identifier;
    ///
    override void accept(AstVisitor visitor)
    {
        if (type)
            visitor.visit(type);
    }
}

/// WhileStatement
final class WhileStatementAstNode: AstNode
{
    /// The condition
    ExpressionAstNode condition;
    /// The single statement or block.
    DeclarationOrStatementAstNode declarationOrStatement;
    ///
    override void accept(AstVisitor visitor)
    {
        if (condition)
            visitor.visit(condition);
        if (declarationOrStatement)
            visitor.visit(declarationOrStatement);
    }
}

/// IfConditionVariable
final class IfConditionVariableAstNode: AstNode
{
    /// Indicates wether the variable is const or var.
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

/// IfElseStatement
final class IfElseStatementAstNode: AstNode
{
    /// The condition when it's an expression.
    ExpressionAstNode condition;
    /// The consition when it's a new scoped variable.
    IfConditionVariableAstNode ifVariable;
    /// The single statement or block when condition is true.
    DeclarationOrStatementAstNode trueDeclarationOrStatement;
    /// The single statement or block when condition is false.
    DeclarationOrStatementAstNode falseDeclarationOrStatement;
    ///
    override void accept(AstVisitor visitor)
    {
        if (condition)
            visitor.visit(condition);
        else if (ifVariable)
            visitor.visit(ifVariable);
        if (trueDeclarationOrStatement)
            visitor.visit(trueDeclarationOrStatement);
        if (falseDeclarationOrStatement)
            visitor.visit(falseDeclarationOrStatement);
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

/// EmptyStatement
final class EmptyStatementAstNode: AstNode {}

/// BlockStatement
final class BlockStatementAstNode: AstNode
{
    /// Ending position
    Position stopPos;
    /// Declarations or statement located in the block.
    DeclarationsOrStatementsAstNode declarationsOrStatements;
    ///
    override void accept(AstVisitor visitor)
    {
        if (declarationsOrStatements)
            visitor.visit(declarationsOrStatements);
    }
}

/// Ancestor for BreakStatement, ContinueStatement and ReturnStatement
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
final class ContinueStatementAstNode: FlowControlBaseNode
{
    /// The token that indicates the label to go to.
    Token* label;
}

/// LabelDeclaration
final class LabelDeclarationstAstNode: AstNode
{
    /// The token that identifies the label.
    Token* identifier;
}

/// BreakStatement
final class BreakStatementAstNode: FlowControlBaseNode
{
    /// The token that indicates the label to go to.
    Token* label;
}

/// GotoStatement
final class GotoStatementAstNode: FlowControlBaseNode
{
    /// The token that indicates the label to go to.
    Token* label;
}

/// Enumerates the possible statements
enum StatementKind: ubyte
{
    skNone,
    skEmpty,
    skExpression,
    skReturn,
    skBreak,
    skContinue,
    skBlock,
    skIfElse,
    skWhile,
    skForeach,
    skSwitch,
    skTryOnFinally,
    skThrow,
    skVersion,
    skAssert,
    skGoto,
}

/// Statement
final class StatementAstNode: AstNode
{
    ///
    static union Statement
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
        /// Assigned if this statement is a VersionBlockStatement.
        VersionBlockStatementAstNode versionBlockStatement;
        /// Assigned if this statement is an AssertStatement.
        AssertStatementAstNode assertStatement;
        /// Assigned if this statement is a GotoStatement;
        GotoStatementAstNode gotoStatement;
    }
    ///
    Statement statement;
    /// Indicates the statement kind.
    StatementKind statementKind;
    ///
    override void accept(AstVisitor visitor)
    {
        with(StatementKind) final switch(statementKind)
        {
        case skEmpty: visitor.visit(statement.emptyStatement); break;
        case skExpression: visitor.visit(statement.expression); break;
        case skReturn: visitor.visit(statement.returnStatement); break;
        case skBreak: visitor.visit(statement.breakStatement); break;
        case skContinue: visitor.visit(statement.continueStatement); break;
        case skBlock: visitor.visit(statement.block); break;
        case skIfElse: visitor.visit(statement.ifElseStatement); break;
        case skWhile: visitor.visit(statement.whileStatement); break;
        case skForeach: visitor.visit(statement.foreachStatement); break;
        case skSwitch: visitor.visit(statement.switchStatement); break;
        case skTryOnFinally: visitor.visit(statement.tryOnFinallyStatement); break;
        case skThrow: visitor.visit(statement.throwStatement); break;
        case skVersion: visitor.visit(statement.versionBlockStatement); break;
        case skAssert: visitor.visit(statement.assertStatement); break;
        case skGoto: visitor.visit(statement.gotoStatement); break;
        case skNone: assert(false);
        }
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

/// FunctionParameterGroup
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

/// TryOnFinallyStatement
final class TryOnFinallyStatementAstNode: AstNode
{
    /// The statement to try.
    DeclarationOrStatementAstNode triedDeclarationOrStatement;
    /// The Exceptions handlers.
    OnExceptionStatementAstNode[] exceptionDeclarationsOrStatements;
    /// The final statement.
    DeclarationOrStatementAstNode finalDeclarationOrStatement;
    ///
    override void accept(AstVisitor visitor)
    {
        if (triedDeclarationOrStatement)
            visitor.visit(triedDeclarationOrStatement);
        exceptionDeclarationsOrStatements.each!(a => visitor.visit(a));
        if (finalDeclarationOrStatement)
            visitor.visit(finalDeclarationOrStatement);

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
        if (exceptionType)
            visitor.visit(exceptionType);
    }
}

/// OnExceptionStatement
final class OnExceptionStatementAstNode: AstNode
{
    /// The list of exceptions for this case.
    OnExceptionInstanceAstNode[] exceptionsInstances;
    /// The statement or block for these exceptions.
    DeclarationOrStatementAstNode exceptionsDeclarationOrStatement;
    ///
    override void accept(AstVisitor visitor)
    {
        exceptionsInstances.each!(a => visitor.visit(a));
        if (exceptionsDeclarationOrStatement)
            visitor.visit(exceptionsDeclarationOrStatement);
    }
}

/// Type
final class TypeAstNode: AstNode
{
    /// A basic or auto type.
    Token* autoOrBasicType;
    /// If the type is a function, then assigned.
    FunctionDeclarationAstNode functionType;
    /// The type identifier parts.
    TypeIdentifierPartAstNode typeIdentifierPart;
    /// The first modifier.
    TypeModifierAstNode modifier;
    ///
    override void accept(AstVisitor visitor)
    {
        if (autoOrBasicType)
            visitor.visit(autoOrBasicType);
        else if (functionType)
            visitor.visit(functionType);
        else if (typeIdentifierPart)
            visitor.visit(typeIdentifierPart);
        if (modifier)
            visitor.visit(modifier);
    }
}

class TypeIdentifierPartAstNode: AstNode
{
    /// The next part of the type identifier
    TypeIdentifierPartAstNode nextPart;
    /// The part identifier or template name
    Token* identifier;
    /// The part template instance
    TemplateInstanceAstNode templateInstance;
    ///
    override void accept(AstVisitor visitor)
    {
        if (identifier)
            visitor.visit(identifier);
        if (templateInstance)
            visitor.visit(templateInstance);
        if (nextPart)
            visitor.visit(nextPart);
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
    Token*[] identifiers;
    /// The declarations located in the unit.
    DeclarationsAstNode declarations;
    ///
    override void accept(AstVisitor visitor)
    {
        if (declarations)
            visitor.visit(declarations);
    }
}

/// TemplateParameters
final class TemplateParametersAstNode: AstNode
{
    /// The parameters identifers.
    Token*[] parameters;
    ///
    override void accept(AstVisitor visitor)
    {
        parameters.each!(a => visitor.visit(a));
    }
}

/// TemplateSpecialization
final class TemplateInstanceAstNode: AstNode
{
    /// The parameters identifers.
    TypeAstNode[] types;
    ///
    override void accept(AstVisitor visitor)
    {
        types.each!(a => visitor.visit(a));
    }
}

/// VersionBlockDeclaration
final class VersionBlockDeclarationAstNode: AstNode
{
    //note: other "compile time conditions" can be put here, like in D, debug{} or static if(){}

    /// Expressions allowing to select the true or false declarations.
    VersionParenExpressionAstNode versionExpression;
    /// The declarations when the versionExpression is verified.
    DeclarationsAstNode trueDeclarations;
    /// The declarations when the versionExpression is not verified.
    DeclarationsAstNode falseDeclarations;
    ///
    override void accept(AstVisitor visitor)
    {
        if (versionExpression)
            visitor.visit(versionExpression);
        if (trueDeclarations)
            visitor.visit(trueDeclarations);
        if (falseDeclarations)
            visitor.visit(falseDeclarations);
    }
    /// Indicates which declarations are valid.
    @Semantic bool isTrue;
}

/// VersionBlockStatement
final class VersionBlockStatementAstNode: AstNode
{
    //note: other "compile time conditions" can be put here, like in D, debug{} or static if(){}

    /// Expressions allowing to select the true or false declarations or statements.
    VersionParenExpressionAstNode versionExpression;
    /// The declarations or statements when the versionExpression is verified.
    DeclarationsOrStatementsAstNode trueDeclarationsOrStatements;
    /// The declarations or statements when the versionExpression is not verified.
    DeclarationsOrStatementsAstNode falseDeclarationsOrStatements;
    ///
    override void accept(AstVisitor visitor)
    {
        if (versionExpression)
            visitor.visit(versionExpression);
        if (trueDeclarationsOrStatements)
            visitor.visit(trueDeclarationsOrStatements);
        if (falseDeclarationsOrStatements)
            visitor.visit(falseDeclarationsOrStatements);
    }
    /// Indicates which declarations or statements are valid.
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
    /// Assigned when the primary is negated.
    Token* not;
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

unittest
{
    AstVisitor av1 = new AstVisitor;
    foreach (T; AstNodes)
        static if (!is(T == DeclarationAstNode) && !is(T == StatementAstNode))
    {
        T node = new T;
        av1.visit(node);
    }
}

