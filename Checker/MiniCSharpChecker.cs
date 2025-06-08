using System.Xml;
using System;
using System.Collections.Generic;
using System.Linq;
using System.IO; 
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using generated;

namespace Compiladores.Checker
{
    public class MiniCSharpChecker : MiniCSharpParserBaseVisitor<Type>
    {
        private readonly TablaSimbolos _symbolTable;
        private readonly CompilationManager _compilationManager;
        private readonly string _currentFilePath;

        public List<string> ErrorMessages { get; } = new List<string>();
        private MethodSymbol _currentMethod = null;
        private VarSymbol _lastDesignatorVarSymbol = null;
        private bool _isDesignatorLValue = false;
        
        public Dictionary<IParseTree, Type> ExpressionTypes { get; } = new Dictionary<IParseTree, Type>();

        public TablaSimbolos SymbolTable => _symbolTable;
        public string CurrentFilePath => _currentFilePath; 

        public MiniCSharpChecker(CompilationManager manager, TablaSimbolos symbolTable, string filePath)
        {
            _compilationManager = manager ?? throw new ArgumentNullException(nameof(manager));
            _symbolTable = symbolTable ?? new TablaSimbolos();
            _currentFilePath = filePath;
        }

        private Type StoreAndReturnType(IParseTree node, Type type)
        {
            if (node != null && type != null)
            {
                ExpressionTypes[node] = type;
            }
            return type;
        }

        private void AddError(string message, IParseTree context)
        {
            string positionInfo = "";
            string nearText = "";

            if (context != null)
            {
                nearText = context.GetText(); 

                if (context is ParserRuleContext prc && prc.Start != null)
                {
                    positionInfo = $" (Line {prc.Start.Line}, Col {prc.Start.Column + 1})";
                }
                else if (context is ITerminalNode terminalNode && terminalNode.Symbol != null)
                {
                    positionInfo = $" (Line {terminalNode.Symbol.Line}, Col {terminalNode.Symbol.Column + 1})";
                }
            }
            else
            {
                nearText = "unknown location";
            }
            string errorMessage = $"SEMANTIC ERROR{positionInfo}: {message} (near \"{nearText}\")";
            if (!ErrorMessages.Contains(errorMessage)) 
            {
                ErrorMessages.Add(errorMessage);
            }
        }
        
         public override Type VisitProgram(MiniCSharpParser.ProgramContext context)
        {
            Scope initialGlobalScope = _symbolTable.GetGlobalScope();
            int initialGlobalLevel = _symbolTable.GetGlobalScopeLevel();
            _symbolTable.SetCurrentScopeTo(initialGlobalScope, initialGlobalLevel);

            if (context.usingDirective() != null)
            {
                foreach (var usingDecl in context.usingDirective())
                {
                    Visit(usingDecl); 
                }
            }

            string programClassName = context.IDENT().GetText();
            var classMembersScope = new Scope(_symbolTable.GetGlobalScope()); 
            var classType = new ClassType(programClassName, classMembersScope); 
            var classSymbol = new ClassSymbol(programClassName, classType);
            classSymbol.Decl = context; 

            if (!_symbolTable.GetGlobalScope().TryInsert(classSymbol))
            {
                Symbol existing = _symbolTable.GetGlobalScope().FindCurrent(programClassName);
                if (existing != null) {
                     AddError($"Program class name '{programClassName}' conflicts with an imported type or another global definition.", context.IDENT());
                } else {
                    AddError($"Program class '{programClassName}' could not be defined in global scope.", context.IDENT());
                }
                return StoreAndReturnType(context, Type.Error);
            }

            Scope outerScopeBackup = _symbolTable.CurrentScope; 
            int outerLevelBackup = _symbolTable.CurrentLevel;   
            _symbolTable.SetCurrentScopeTo(classMembersScope, outerLevelBackup + 1); 

            foreach (var decl in context.varDecl()) Visit(decl);
            foreach (var class_decl in context.classDecl()) Visit(class_decl);
            foreach (var method_decl in context.methodDecl()) Visit(method_decl);

            _symbolTable.SetCurrentScopeTo(outerScopeBackup, outerLevelBackup);
            
            return StoreAndReturnType(context, Type.Void);
        }

        public override Type VisitClassDecl(MiniCSharpParser.ClassDeclContext context)
        {
            string className = context.IDENT().GetText();
            if (_symbolTable.SearchCurrentScope(className) != null)
            {
                AddError($"Class or identifier '{className}' is already declared in this scope.", context.IDENT());
                return StoreAndReturnType(context, Type.Error);
            }

            var classMembersScope = new Scope(_symbolTable.CurrentScope);
            var classType = new ClassType(className, classMembersScope);
            var classSymbol = new ClassSymbol(className, classType);
            classSymbol.Decl = context;

            if (!_symbolTable.Insert(classSymbol))
            {
                AddError($"Failed to insert class symbol '{className}'.", context.IDENT());
                return StoreAndReturnType(context, Type.Error);
            }
            
            Scope outerScopeBackup = _symbolTable.CurrentScope;
            int outerLevelBackup = _symbolTable.CurrentLevel;
            _symbolTable.SetCurrentScopeTo(classMembersScope, outerLevelBackup + 1);
            _currentMethod = null;
            foreach (var varDecl in context.varDecl()) Visit(varDecl);
            _symbolTable.SetCurrentScopeTo(outerScopeBackup, outerLevelBackup);
            
            return StoreAndReturnType(context, classType);
        }


        public override Type VisitType(MiniCSharpParser.TypeContext context)
        {
            string typeName = context.IDENT().GetText();
            Symbol typeSymbol = _symbolTable.Search(typeName); 

            if (typeSymbol == null || !(typeSymbol is TypeDefSymbol || typeSymbol is ClassSymbol))
            {
                AddError($"Type '{typeName}' not defined.", context.IDENT());
                return StoreAndReturnType(context, Type.Error);
            }
            Type baseType = typeSymbol.Type;
            if (context.LBRACK() != null && context.RBRACK() != null) 
            {
                if (!(baseType.Kind == TypeKind.Int || baseType.Kind == TypeKind.Char || baseType.Kind == TypeKind.Double))
                {
                    AddError($"Arrays can only be of type 'int', 'char', or 'double', not '{baseType.Name}'.", context);
                    return StoreAndReturnType(context, Type.Error);
                }
                return StoreAndReturnType(context, new ArrayType(baseType));
            }
            return StoreAndReturnType(context, baseType);
        }

        public override Type VisitVarDecl(MiniCSharpParser.VarDeclContext context)
        {
            Type varType = Visit(context.type()); 
            
            if (varType.Kind == TypeKind.Error) return Type.Error; 
    
            if (varType.Kind == TypeKind.Void)
            {
                AddError("Variables cannot be of type 'void'.", context.type());
                return Type.Error; 
            }

            foreach (var identNode in context.IDENT())
            {
                string varName = identNode.GetText();
                if (_symbolTable.SearchCurrentScope(varName) != null)
                {
                    AddError($"Identifier '{varName}' already declared in this scope.", identNode);
                }
                else
                {
                    var varSymbol = new VarSymbol(varName, varType);
                    varSymbol.Decl = context; 
                    _symbolTable.Insert(varSymbol);
                }
            }
            return Type.Void; 
        }

       public override Type VisitMethodDecl(MiniCSharpParser.MethodDeclContext context)
        {
            string methodName = context.IDENT().GetText();
            Type returnType = (context.type() != null) ? Visit(context.type()) : Type.Void;

            if (returnType.Kind == TypeKind.Error) return Type.Error;

            if (_symbolTable.SearchCurrentScope(methodName) != null)
            {
                AddError($"Identifier '{methodName}' already declared in this class scope.", context.IDENT());
                return Type.Error;
            }

            MethodSymbol methodSymbol = new MethodSymbol(methodName, returnType);
            methodSymbol.Decl = context;

            var previousMethod = _currentMethod;
            _currentMethod = methodSymbol;
            
            Scope classScope = _symbolTable.CurrentScope;
            _symbolTable.OpenScope(context);

            if (context.formPars() != null) 
            { 
                Visit(context.formPars()); 
            }

            foreach(var param in _currentMethod.Parameters)
            {
                if(!_symbolTable.Insert(param))
                { 
                    AddError($"Duplicate parameter name '{param.Name}' in method '{methodName}'.", param.Decl ?? (IParseTree)context.formPars() ?? context.IDENT());
                }
            }
            
            Visit(context.block()); 
            
            _symbolTable.CloseScope(); 
            
            _currentMethod = previousMethod;
            
            if (!classScope.TryInsert(methodSymbol))
            {
                AddError($"Could not insert method symbol '{methodName}' into class scope.", context.IDENT());
            }
            return Type.Void;
        }

        public override Type VisitFormPars(MiniCSharpParser.FormParsContext context)
        {
            if (_currentMethod == null)
            {
                AddError("Formal parameters processed outside of a method context.", context);
                return Type.Error;
            }
            var types = context.type();
            var idents = context.IDENT();
            for (int i = 0; i < idents.Length; i++)
            {
                Type paramType = Visit(types[i]);
                if (paramType.Kind == TypeKind.Error) continue;
                if (paramType.Kind == TypeKind.Void)
                { AddError($"Method parameters cannot be of type 'void'. Found for '{idents[i].GetText()}'.", types[i]); continue; }
                string paramName = idents[i].GetText();
                if (_currentMethod.Parameters.Any(p => p.Name == paramName))
                { AddError($"Duplicate parameter name '{paramName}' in method '{_currentMethod.Name}'.", idents[i]); continue; }
                VarSymbol paramSymbol = new VarSymbol(paramName, paramType) { Decl = idents[i].Payload is IToken ? context : idents[i].Parent as ParserRuleContext ?? types[i] };
                if (idents[i].Payload is IToken identTokenPayload) { 
                    paramSymbol.Decl = new TerminalNodeImpl(identTokenPayload).Parent as ParserRuleContext ?? types[i];
                } else { 
                    paramSymbol.Decl = types[i];
                }
                _currentMethod.Parameters.Add(paramSymbol);
            }
            return Type.Void;
        }

        public override Type VisitNumberFactor(MiniCSharpParser.NumberFactorContext context)
        {
            Type type;
            if (context.number().INTCONST() != null) type = Type.Int;
            else if (context.number().DOUBLECONST() != null) type = Type.Double;
            else { AddError("Unknown number format.", context); type = Type.Error; }
            return StoreAndReturnType(context, type);
        }

        public override Type VisitCharFactor(MiniCSharpParser.CharFactorContext context) => StoreAndReturnType(context, Type.Char);
        public override Type VisitStringFactor(MiniCSharpParser.StringFactorContext context) => StoreAndReturnType(context, Type.String);
        public override Type VisitBoolFactor(MiniCSharpParser.BoolFactorContext context) => StoreAndReturnType(context, Type.Bool);
        public override Type VisitParenFactor(MiniCSharpParser.ParenFactorContext context)
        {
            Type exprType = Visit(context.expr());
            return StoreAndReturnType(context, exprType);
        }

         public override Type VisitDesignator(MiniCSharpParser.DesignatorContext context)
        {
            _lastDesignatorVarSymbol = null;
            _isDesignatorLValue = false;
            string baseName = context.IDENT(0).GetText();
            Symbol symbol = _symbolTable.Search(baseName);

            if (symbol == null)
            {
                AddError($"Identifier '{baseName}' not declared.", context.IDENT(0));
                return StoreAndReturnType(context, Type.Error);
            }
            Type currentType = symbol.Type;
            if (symbol.Kind == SymbolKind.Variable || symbol.Kind == SymbolKind.Constant)
            {
                 if (symbol is VarSymbol vs) _lastDesignatorVarSymbol = vs;
                 _isDesignatorLValue = (symbol.Kind != SymbolKind.Constant); 
            }

            int identIndex = 0; 
            for (int i = 1; i < context.ChildCount; i++) 
            {
                var child = context.GetChild(i);
                bool nextIsLValueCandidate = false;
                if (child is ITerminalNode terminalNode && terminalNode.Symbol.Type == MiniCSharpLexer.DOT)
                {
                    identIndex++;
                    if (i + 1 >= context.ChildCount || !(context.GetChild(i + 1) is ITerminalNode memberIdentNode && memberIdentNode.Symbol.Type == MiniCSharpLexer.IDENT))
                    { AddError($"Expected identifier after '.'.", child); return StoreAndReturnType(context, Type.Error); }
                    string memberName = memberIdentNode.GetText();
                    if (currentType.Kind != TypeKind.Class)
                    { AddError($"'{_symbolTable.Search(baseName)?.Name ?? baseName}' is not a class and cannot have members.", context.IDENT(identIndex-1)); return StoreAndReturnType(context, Type.Error); }
                    ClassType classType = (ClassType)currentType;
                    Symbol memberSymbol = classType.Members.Find(memberName);
                    if (memberSymbol == null)
                    { AddError($"Class '{classType.Name}' does not have a member named '{memberName}'.", memberIdentNode); return StoreAndReturnType(context, Type.Error); }
                    currentType = memberSymbol.Type;
                    if (memberSymbol.Kind == SymbolKind.Variable || memberSymbol.Kind == SymbolKind.Constant)
                    { if (memberSymbol is VarSymbol mvs) _lastDesignatorVarSymbol = mvs; nextIsLValueCandidate = (memberSymbol.Kind != SymbolKind.Constant); }
                    i++; 
                }
                else if (child is ITerminalNode tNodeLbrack && tNodeLbrack.Symbol.Type == MiniCSharpLexer.LBRACK)
                {
                    if (i + 2 >= context.ChildCount || !(context.GetChild(i + 1) is MiniCSharpParser.ExprContext) || !(context.GetChild(i + 2) is ITerminalNode tNodeRbrack && tNodeRbrack.Symbol.Type == MiniCSharpLexer.RBRACK))
                    { AddError("Invalid array access syntax.", child); return StoreAndReturnType(context, Type.Error); }
                    if (currentType.Kind != TypeKind.Array)
                    { AddError($"Identifier '{_symbolTable.Search(baseName)?.Name ?? baseName}' is not an array.", context.IDENT(identIndex)); return StoreAndReturnType(context, Type.Error); }
                    Type indexType = Visit(context.expr(context.expr().ToList().IndexOf((MiniCSharpParser.ExprContext)context.GetChild(i+1))));
                    if (indexType.Kind != TypeKind.Int) { AddError("Array index must be an integer.", context.GetChild(i+1)); }
                    currentType = ((ArrayType)currentType).ElementType;
                    _lastDesignatorVarSymbol = null; 
                    nextIsLValueCandidate = true; 
                    i += 2; 
                }
                _isDesignatorLValue = nextIsLValueCandidate;
            }
            return StoreAndReturnType(context, currentType);
        }


        public override Type VisitDesignatorFactor(MiniCSharpParser.DesignatorFactorContext context)
        {
            if (context.LPAREN() == null)
            {
                Type designatorType = Visit(context.designator());
                Symbol baseDesignatorSymbol = _symbolTable.Search(context.designator().IDENT(0).GetText());
                if (baseDesignatorSymbol is MethodSymbol && context.designator().ChildCount == 1)
                {
                    AddError($"Method name '{context.designator().GetText()}' used as a value without calling it.", context.designator());
                    return StoreAndReturnType(context, Type.Error);
                }
                return StoreAndReturnType(context, designatorType);
            }
            else 
            {
                var designatorCtx = context.designator();
                Symbol potentialMethodSymbol = null;
                string methodName = "";

                if (designatorCtx.DOT().Length > 0)
                {
                    string objectName = designatorCtx.IDENT(0).GetText();
                    Symbol objectSymbol = _symbolTable.Search(objectName);

                    if (objectSymbol == null)
                    {
                        AddError($"Object '{objectName}' not declared.", designatorCtx.IDENT(0));
                        return StoreAndReturnType(context, Type.Error);
                    }
                    
                    if (objectSymbol.Type is ClassType classType)
                    {
                        methodName = designatorCtx.IDENT().Last().GetText();
                        potentialMethodSymbol = classType.Members.Find(methodName);
                    }
                    else
                    {
                        AddError($"Identifier '{objectSymbol.Name}' is not a class or object instance, cannot call a method on it.", designatorCtx);
                        return StoreAndReturnType(context, Type.Error);
                    }
                }
                else 
                {
                    methodName = designatorCtx.IDENT(0).GetText();
                    potentialMethodSymbol = _symbolTable.Search(methodName);
                }
                
                if (potentialMethodSymbol == null || !(potentialMethodSymbol is MethodSymbol methodSymbol))
                {
                    AddError($"'{methodName}' is not a method or not declared in the given context.", context.designator());
                    return StoreAndReturnType(context, Type.Error);
                }

                List<Type> actualParamTypes = new List<Type>();
                if (context.actPars() != null)
                {
                    foreach (var exprCtx in context.actPars().expr())
                    {
                        actualParamTypes.Add(Visit(exprCtx));
                    }
                }

                if (methodSymbol.Parameters.Count != actualParamTypes.Count)
                {
                    IParseTree errorContext = context.actPars() ?? (IParseTree)context.LPAREN();
                    AddError($"Method '{methodSymbol.Name}' expects {methodSymbol.Parameters.Count} arguments, but got {actualParamTypes.Count}.", errorContext);
                    return StoreAndReturnType(context, Type.Error);
                }

                for (int i = 0; i < methodSymbol.Parameters.Count; i++)
                {
                    if (actualParamTypes[i].Kind == TypeKind.Error) continue;
                    if (!AreTypesCompatible(methodSymbol.Parameters[i].Type, actualParamTypes[i]))
                    {
                        AddError($"Type mismatch for argument {i + 1} of method '{methodSymbol.Name}'. Expected '{methodSymbol.Parameters[i].Type}', got '{actualParamTypes[i]}'.", context.actPars().expr(i));
                    }
                }

                return StoreAndReturnType(context, methodSymbol.Type);
            }
        }

        private bool AreTypesCompatible(Type formalType, Type actualType)
        {
            if (formalType.Kind == TypeKind.Error || actualType.Kind == TypeKind.Error) return true;
            if (formalType.Kind == actualType.Kind)
            {
                if (formalType.Kind == TypeKind.Array && actualType.Kind == TypeKind.Array)
                {
                    return AreTypesCompatible(((ArrayType)formalType).ElementType, ((ArrayType)actualType).ElementType);
                }
                if (formalType.Kind == TypeKind.Class && actualType.Kind == TypeKind.Class)
                {
                    return formalType.Name == actualType.Name;
                }
                return true;
            }
            if ((formalType.Kind == TypeKind.Class || formalType.Kind == TypeKind.Array) && actualType.Kind == TypeKind.Null) return true;
            if (formalType.Kind == TypeKind.Double && actualType.Kind == TypeKind.Int) return true;

            return false;
        }


        public override Type VisitDesignatorStatement(MiniCSharpParser.DesignatorStatementContext context)
        {
            Type designatorType = Visit(context.designator());
            VarSymbol targetVar = _lastDesignatorVarSymbol; 
            bool isLValue = _isDesignatorLValue;         

            if (designatorType.Kind == TypeKind.Error) return Type.Error;

            if (context.ASSIGN() != null) 
            {
                if (!isLValue) { AddError("The left-hand side of an assignment must be a variable, field, or array element.", context.designator()); return Type.Error; }
                if (targetVar != null && targetVar.Kind == SymbolKind.Constant) { AddError($"Cannot assign to '{targetVar.Name}' because it is a constant.", context.designator()); }
                var rhsExprCtx = context.expr(); 
                if (rhsExprCtx == null) { AddError("Missing expression on the right-hand side of assignment.", context.ASSIGN()); return Type.Error; }
                Type rhsType = Visit(rhsExprCtx); 
                if (rhsType.Kind == TypeKind.Error) return Type.Error;
                
                if (designatorType.Kind == TypeKind.Array) { /* ... */ }
                if (!AreTypesCompatible(designatorType, rhsType)) { AddError($"Cannot assign type '{rhsType}' to '{designatorType}'.", context.expr()); }
            }
            else if (context.LPAREN() != null) { /* ... */ }
            else if (context.INC() != null || context.DEC() != null) 
            {
                if (!isLValue) { AddError("The operand of an increment or decrement operator must be a variable, field, or array element.", context.designator()); }
                if (!(designatorType.Kind == TypeKind.Int || designatorType.Kind == TypeKind.Double)) { AddError($"Operator '{(context.INC() != null ? "++" : "--")}' cannot be applied to operand of type '{designatorType}'.", context.designator()); }
            }
            return Type.Void;
        }

        public override Type VisitIfStatement(MiniCSharpParser.IfStatementContext context)
        {
            Type conditionType = Visit(context.condition());
            if (conditionType.Kind != TypeKind.Error && conditionType.Kind != TypeKind.Bool)
            { AddError("If condition must be of type 'bool'.", context.condition()); }
            Visit(context.statement(0));
            if (context.ELSE() != null) { Visit(context.statement(1)); }
            return Type.Void;
        }

        public override Type VisitWriteStatement(MiniCSharpParser.WriteStatementContext context)
        {
            Type exprType = Visit(context.expr());
            if (exprType.Kind == TypeKind.Error) return Type.Error;
            if (!(exprType.Kind == TypeKind.Int || exprType.Kind == TypeKind.Double || exprType.Kind == TypeKind.Char || exprType.Kind == TypeKind.Bool || exprType.Kind == TypeKind.String))
            { AddError($"Cannot write expression of type '{exprType}'.", context.expr()); }
            if (context.number() != null)
            {
                Type numberType = Visit(context.number());
                if (numberType.Kind != TypeKind.Int) { AddError("Format specifier for 'write' (if present) must be an integer.", context.number()); }
            }
            return Type.Void;
        }

        public override Type VisitReturnStatement(MiniCSharpParser.ReturnStatementContext context)
        {
            if (_currentMethod == null) { AddError("Return statement found outside of a method.", context); return Type.Error; }
            if (context.expr() != null)
            {
                Type returnExprType = Visit(context.expr());
                if (returnExprType.Kind == TypeKind.Error) return Type.Error;
                if (_currentMethod.Type.Kind == TypeKind.Void) { AddError($"Method '{_currentMethod.Name}' is void and cannot return a value.", context.expr()); }
                else if (!AreTypesCompatible(_currentMethod.Type, returnExprType)) { AddError($"Cannot return type '{returnExprType}' from method '{_currentMethod.Name}' expecting '{_currentMethod.Type}'.", context.expr());}
            }
            else { if (_currentMethod.Type.Kind != TypeKind.Void) { AddError($"Method '{_currentMethod.Name}' expects a return value of type '{_currentMethod.Type}'.", context); } }
            return Type.Void;
        }
        
        public override Type VisitBlock(MiniCSharpParser.BlockContext context)
        {
            _symbolTable.OpenScope(context);

            if (context.children != null) {
                foreach (var child in context.children.OfType<IParseTree>())
                {
                    if (child is MiniCSharpParser.VarDeclContext varDeclCtx)
                    {
                        Visit(varDeclCtx);
                    }
                    else if (child is MiniCSharpParser.StatementContext stmtCtx)
                    {
                        Visit(stmtCtx);
                    }
                }
            }

            _symbolTable.CloseScope(); 
            return Type.Void;
        }

        public override Type VisitExpr(MiniCSharpParser.ExprContext context)
        {
            Type currentType = Visit(context.term(0));

            if (context.MINUS() != null && currentType.Kind != TypeKind.Error)
            {
                if (!(currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double))
                { AddError($"Unary minus operator cannot be applied to type '{currentType}'.", context.term(0)); return StoreAndReturnType(context, Type.Error); }
            }

            // *** INICIO DEL NUEVO CÓDIGO PARA CASTEO ***
            if (context.cast() != null)
            {
                Type targetType = Visit(context.cast()); // Esto obtiene el tipo de destino, ej. 'int'
                if (!IsCastCompatible(currentType, targetType))
                {
                    AddError($"Cannot cast from '{currentType}' to '{targetType}'.", context.cast());
                    currentType = Type.Error;
                }
                else
                {
                    // El tipo de la expresión después del casteo es el tipo de destino
                    currentType = targetType; 
                }
            }
            // *** FIN DEL NUEVO CÓDIGO PARA CASTEO ***

            for (int i = 0; i < context.addop().Length; i++)
            {
                if (currentType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);
                string op = context.addop(i).GetText();
                Type rightType = Visit(context.term(i + 1));
                if (rightType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);

                if (currentType.Kind == TypeKind.Int && rightType.Kind == TypeKind.Int) { currentType = Type.Int; }
                else if ((currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double) && (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double)) { currentType = Type.Double; }
                else if (op == "+" && currentType.Kind == TypeKind.String && rightType.Kind == TypeKind.String) { currentType = Type.String; }
                else if (op == "+" && currentType.Kind == TypeKind.String && (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double || rightType.Kind == TypeKind.Char || rightType.Kind == TypeKind.Bool)) { currentType = Type.String; }
                else if (op == "+" && (currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double || currentType.Kind == TypeKind.Char || currentType.Kind == TypeKind.Bool) && rightType.Kind == TypeKind.String) { currentType = Type.String; }
                else { AddError($"Operator '{op}' cannot be applied to operands of type '{currentType}' and '{rightType}'.", context.addop(i)); return StoreAndReturnType(context, Type.Error); }
            }
            return StoreAndReturnType(context, currentType);
        }

        public override Type VisitTerm(MiniCSharpParser.TermContext context)
        {
            Type currentType = Visit(context.factor(0));
            for (int i = 0; i < context.mulop().Length; i++)
            {
                if (currentType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);
                string op = context.mulop(i).GetText();
                Type rightType = Visit(context.factor(i + 1));
                if (rightType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);

                if (currentType.Kind == TypeKind.Int && rightType.Kind == TypeKind.Int) { currentType = Type.Int; }
                else if ((currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double) && (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double))
                {
                    if (op == "%" && (currentType.Kind == TypeKind.Double || rightType.Kind == TypeKind.Double))
                    { AddError("Operator '%' is not typically defined for double operands in this context.", context.mulop(i)); return StoreAndReturnType(context, Type.Error); }
                    currentType = Type.Double;
                }
                else { AddError($"Operator '{op}' cannot be applied to operands of type '{currentType}' and '{rightType}'.", context.mulop(i)); return StoreAndReturnType(context, Type.Error); }
            }
            return StoreAndReturnType(context, currentType);
        }

        public override Type VisitCondFact(MiniCSharpParser.CondFactContext context)
        {
            Type leftType = Visit(context.expr(0));
            Type rightType = Visit(context.expr(1));

            if (leftType.Kind == TypeKind.Error || rightType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);
            var relopNode = context.relop(); bool compatible = false;
            if (relopNode.EQUAL() != null || relopNode.NOTEQUAL() != null) { compatible = AreTypesCompatibleForEquality(leftType, rightType); }
            else { if ((leftType.Kind == TypeKind.Int || leftType.Kind == TypeKind.Double || leftType.Kind == TypeKind.Char) && (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double || rightType.Kind == TypeKind.Char)) { compatible = true; } }
            if (!compatible) { AddError($"Cannot compare types '{leftType}' and '{rightType}' with operator '{relopNode.GetText()}'.", context.relop()); return StoreAndReturnType(context, Type.Error); }
            return StoreAndReturnType(context, Type.Bool);
        }

        public override Type VisitSwitchStatement(MiniCSharpParser.SwitchStatementContext context)
        {
            Type switchExprType = Visit(context.expr());
            if (switchExprType.Kind == TypeKind.Error) return Type.Error;

            if (switchExprType.Kind != TypeKind.Int && switchExprType.Kind != TypeKind.Char)
            {
                AddError($"Switch expression must be of type 'int' or 'char', not '{switchExprType}'.", context.expr());
            }

            HashSet<string> caseValuesTexts = new HashSet<string>();
            bool defaultFound = false;

            if (context.switchCase() != null)
            {
                foreach (var caseCtx in context.switchCase())
                {
                    Type caseConstantType = Visit(caseCtx.constant());
                    if (caseConstantType.Kind != TypeKind.Error)
                    {
                        if (switchExprType.Kind != TypeKind.Error && !AreTypesCompatible(switchExprType, caseConstantType))
                        {
                             AddError($"Case constant type '{caseConstantType}' is not compatible with switch expression type '{switchExprType}'.", caseCtx.constant());
                        }
                        string caseValText = caseCtx.constant().GetText();
                        if (!caseValuesTexts.Add(caseValText))
                        {
                            AddError($"Duplicate case value: {caseValText}.", caseCtx.constant());
                        }
                    }
                    foreach (var stmt in caseCtx.statement())
                    {
                        Visit(stmt);
                    }
                }
            }

            if (context.defaultCase() != null)
            {
                Visit(context.defaultCase());
            }
            
            return Type.Void;
        }

        public override Type VisitSwitchCase(MiniCSharpParser.SwitchCaseContext context)
        {
            if (context.statement() != null)
            {
                foreach (var stmt in context.statement())
                {
                    Visit(stmt);
                }
            }
            return Type.Void;
        }

        public override Type VisitDefaultCase(MiniCSharpParser.DefaultCaseContext context)
        {
            if (context.statement() != null)
            {
                foreach (var stmt in context.statement())
                {
                    Visit(stmt);
                }
            }
            return Type.Void;
        }

        public override Type VisitConstant(MiniCSharpParser.ConstantContext context)
        {
            Type type;
            if (context.number() != null) { type = Visit(context.number()); }
            else if (context.CHARCONST() != null) { type = Type.Char; }
            else { AddError("Unknown constant type.", context); type = Type.Error;}
            return StoreAndReturnType(context, type);
        }

        public override Type VisitActPars(MiniCSharpParser.ActParsContext context)
        {
            foreach (var expr in context.expr())
            {
                Visit(expr);
            }
            return Type.Void;
        }

        public override Type VisitNumber(MiniCSharpParser.NumberContext context)
        {
            Type type;
            if (context.INTCONST() != null) { type = Type.Int; }
            else if (context.DOUBLECONST() != null) { type = Type.Double; }
            else { AddError("Unknown number literal.", context); type = Type.Error; }
            return StoreAndReturnType(context, type);
        }
        
        public override Type Visit(IParseTree tree)
        {
            if (tree == null) {
                return Type.Error; 
            }
            return base.Visit(tree);
        }

        public override Type VisitChildren(IRuleNode node)
        {
            Type result = Type.Void;
            int n = node.ChildCount;
            for (int i = 0; i < n; i++) {
                if (!ShouldVisitNextChild(node, result)) {
                    break;
                }
                IParseTree c = node.GetChild(i);
                Type childResult = Visit(c);
                result = AggregateResult(result, childResult);
            }
            return result;
        }

        public override Type VisitTerminal(ITerminalNode node)
        {
            return base.VisitTerminal(node);
        }

        public override Type VisitErrorNode(IErrorNode node) { AddError("Parser error node encountered during semantic check.", node); return Type.Error; }


        public override Type VisitUsingDirective(MiniCSharpParser.UsingDirectiveContext context)
        {
            if (context.qualifiedIdent() != null)
            {
                string moduleName = context.qualifiedIdent().GetText();

                var importedModule = _compilationManager.GetOrCompileModule(moduleName, this);

                if (importedModule != null)
                {
                    ClassSymbol importedClassSymbol = importedModule.Item1;

                    Scope globalScope = _symbolTable.GetGlobalScope();

                    if (globalScope.FindCurrent(moduleName) != null)
                    {
                        Symbol existing = globalScope.FindCurrent(moduleName);
                        if (!(existing is ClassSymbol && existing.Type.Name == importedClassSymbol.Name))
                        {
                            AddError($"Using directive for '{moduleName}' conflicts with an existing global definition.", context.qualifiedIdent());
                        }
                    }
                    else
                    {
                        ClassSymbol symbolForCurrentTable = new ClassSymbol(importedClassSymbol.Name, (ClassType)importedClassSymbol.Type);
                        symbolForCurrentTable.Level = _symbolTable.GetGlobalScopeLevel();
                        symbolForCurrentTable.Decl = context;

                        if (!globalScope.TryInsert(symbolForCurrentTable))
                        {
                            AddError($"Failed to make type '{moduleName}' available via using directive.", context.qualifiedIdent());
                        }
                    }
                }
            }
            return Type.Void;
        }


        public override Type VisitQualifiedIdent(MiniCSharpParser.QualifiedIdentContext context)
        {
            return Type.Void;
        }


        private bool AreTypesCompatibleForEquality(Type t1, Type t2)
        {
            if (t1.Kind == TypeKind.Error || t2.Kind == TypeKind.Error) return true;
            if (t1.Kind == t2.Kind)
            {
                if (t1.Kind == TypeKind.Class) return t1.Name == t2.Name;
                return true;
            }
            if ((t1.Kind == TypeKind.Class || t1.Kind == TypeKind.Array || t1.Kind == TypeKind.String) && t2.Kind == TypeKind.Null) return true;
            if (t1.Kind == TypeKind.Null && (t2.Kind == TypeKind.Class || t2.Kind == TypeKind.Array || t2.Kind == TypeKind.String)) return true;
            if ((t1.Kind == TypeKind.Int && t2.Kind == TypeKind.Double) || (t1.Kind == TypeKind.Double && t2.Kind == TypeKind.Int)) return true;

            return false;
        }

        public override Type VisitCondition(MiniCSharpParser.ConditionContext context)
        {
            Type currentType = Visit(context.condTerm(0));
            for (int i = 0; i < context.OR().Length; i++)
            {
                if (currentType.Kind != TypeKind.Error && currentType.Kind != TypeKind.Bool) { AddError("Left operand of '||' must be of type bool.", context.condTerm(i)); }
                Type rightType = Visit(context.condTerm(i + 1));
                if (rightType.Kind != TypeKind.Error && rightType.Kind != TypeKind.Bool) { AddError("Right operand of '||' must be of type bool.", context.condTerm(i + 1)); }
                if (currentType.Kind == TypeKind.Bool && rightType.Kind == TypeKind.Bool) { currentType = Type.Bool; }
                else { currentType = Type.Error; }
            }
            return StoreAndReturnType(context, currentType);
        }

        public override Type VisitCondTerm(MiniCSharpParser.CondTermContext context)
        {
            Type currentType = Visit(context.condFact(0));
            for (int i = 0; i < context.AND().Length; i++)
            {
                if (currentType.Kind != TypeKind.Error && currentType.Kind != TypeKind.Bool) { AddError("Left operand of '&&' must be of type bool.", context.condFact(i)); }
                Type rightType = Visit(context.condFact(i + 1));
                if (rightType.Kind != TypeKind.Error && rightType.Kind != TypeKind.Bool) { AddError("Right operand of '&&' must be of type bool.", context.condFact(i + 1)); }
                if (currentType.Kind == TypeKind.Bool && rightType.Kind == TypeKind.Bool) { currentType = Type.Bool; }
                else { currentType = Type.Error; }
            }
            return StoreAndReturnType(context, currentType);
        }

        public override Type VisitNewFactor(MiniCSharpParser.NewFactorContext context)
        {
            string typeName = context.IDENT().GetText();
            Symbol typeSymbol = _symbolTable.Search(typeName);
            if (typeSymbol == null)
            { AddError($"Type '{typeName}' not found for 'new' operator.", context.IDENT()); return StoreAndReturnType(context, Type.Error); }

            if (context.LBRACK() != null) 
            {
                if (!(typeSymbol.Type.Kind == TypeKind.Int || typeSymbol.Type.Kind == TypeKind.Char || typeSymbol.Type.Kind == TypeKind.Double))
                { AddError($"Can only create arrays of 'int', 'char', or 'double', not '{typeName}'.", context.IDENT()); return StoreAndReturnType(context, Type.Error); }
                Type exprType = Visit(context.expr());
                if(exprType.Kind != TypeKind.Int) { AddError("Array size specifier must be an integer.", context.expr()); }
                return StoreAndReturnType(context, new ArrayType(typeSymbol.Type));
            }
            else 
            {
                if (!(typeSymbol is ClassSymbol))
                { AddError($"'{typeName}' is not a class type and cannot be instantiated with 'new'.", context.IDENT()); return StoreAndReturnType(context, Type.Error); }
                return StoreAndReturnType(context, typeSymbol.Type); 
            }
        }
        
        public override Type VisitReadStatement(MiniCSharpParser.ReadStatementContext context)
        {
            Type designatorType = Visit(context.designator());
            if (designatorType.Kind == TypeKind.Error) return Type.Error;

            if(!_isDesignatorLValue)
            {
                AddError("Operand for 'read' must be an assignable variable (L-Value).", context.designator());
            }

            if (!(designatorType.Kind == TypeKind.Int || designatorType.Kind == TypeKind.Double || designatorType.Kind == TypeKind.Char))
            {
                AddError($"Can only 'read' into 'int', 'double', or 'char' variables, not '{designatorType}'.", context.designator());
            }
            return Type.Void;
        }


        public override Type VisitForStatement(MiniCSharpParser.ForStatementContext context)
        {
            _symbolTable.OpenScope(); 

            if (context.expr() != null) Visit(context.expr()); 
            
            if (context.condition() != null)
            {
                Type condType = Visit(context.condition());
                if (condType.Kind != TypeKind.Error && condType.Kind != TypeKind.Bool)
                {
                    AddError("For loop condition must be of type 'bool'.", context.condition());
                }
            }

            var statements = context.statement();
            if (statements != null && statements.Length > 0)
            {
                if (statements.Length > 1)
                {
                    Visit(statements[0]);
                }
                Visit(statements.Last());
            } else if (statements != null && statements.Length == 1) {
                Visit(statements[0]);
            }

            _symbolTable.CloseScope();
            return Type.Void;
        }


        public override Type VisitWhileStatement(MiniCSharpParser.WhileStatementContext context)
        {
            Type conditionType = Visit(context.condition());
            if (conditionType.Kind != TypeKind.Error && conditionType.Kind != TypeKind.Bool)
            {
                AddError("While loop condition must be of type 'bool'.", context.condition());
            }
            Visit(context.statement());
            return Type.Void;
        }

        public override Type VisitBreakStatement(MiniCSharpParser.BreakStatementContext context)
        {
            return Type.Void;
        }


        public override Type VisitEmptyStatement(MiniCSharpParser.EmptyStatementContext context)
        {
            return Type.Void;
        }

        public override Type VisitBlockStatement(MiniCSharpParser.BlockStatementContext context)
        {
            return Visit(context.block());
        }
        
        // *** MÉTODO MODIFICADO PARA CASTING ***
        public override Type VisitCast(MiniCSharpParser.CastContext context)
        {
            // Este método simplemente visita y devuelve el tipo que se encuentra dentro de los paréntesis del casteo
            Type targetType = Visit(context.type());
            return StoreAndReturnType(context, targetType);
        }
         
        // *** NUEVO MÉTODO HELPER PARA VALIDAR CASTING ***
        private bool IsCastCompatible(Type source, Type target)
        {
            // Permite conversiones entre cualquier tipo numérico (int, double, char)
            bool isSourceNumeric = source.Kind == TypeKind.Int || source.Kind == TypeKind.Double || source.Kind == TypeKind.Char;
            bool isTargetNumeric = target.Kind == TypeKind.Int || target.Kind == TypeKind.Double || target.Kind == TypeKind.Char;

            if (isSourceNumeric && isTargetNumeric)
            {
                return true;
            }

            // Aquí se podrían agregar más reglas en el futuro (ej. casteo de clases)
            return false;
        }
    }
}