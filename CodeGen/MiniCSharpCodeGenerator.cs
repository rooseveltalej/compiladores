using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime.Tree;
using Compiladores.Checker;
using Compiladores; 
using System.Globalization;
using generated;

namespace Compiladores.CodeGen
{
    public class MiniCSharpCodeGenerator : MiniCSharpParserBaseVisitor<object>
    {
        private readonly TablaSimbolos _symbolTable; 
        private readonly string _assemblyNameBase; 
        private MiniCSharpParser.ProgramContext _programContext; 
        private MethodSymbol _currentGeneratingMethodSymbol = null; 

        private AssemblyBuilder _assemblyBuilder; 
        private ModuleBuilder _moduleBuilder; 
        private TypeBuilder _currentTypeBuilder; 
        private MethodBuilder _currentMethodBuilder; 
        private ILGenerator _ilGenerator; 

        private Dictionary<Symbol, FieldBuilder> _fieldBuilders = new Dictionary<Symbol, FieldBuilder>(); 
        private Dictionary<Symbol, MethodBuilder> _methodBuilders = new Dictionary<Symbol, MethodBuilder>(); 
        private Dictionary<Symbol, LocalBuilder> _localBuilders = new Dictionary<Symbol, LocalBuilder>(); 
        private Dictionary<ClassSymbol, TypeBuilder> _definedTypes = new Dictionary<ClassSymbol, TypeBuilder>(); 

        private Scope _currentCodeGenScope; 
        
        private Stack<System.Reflection.Emit.Label> _breakLabelStack = new Stack<System.Reflection.Emit.Label>();

        public Dictionary<IParseTree, Compiladores.Checker.Type> ExpressionTypes { get; }
        private readonly CompilationManager _compilationManager; 


        public MiniCSharpCodeGenerator(CompilationManager manager, TablaSimbolos symbolTable, string sourceFilePath, string assemblyName, Dictionary<IParseTree, Compiladores.Checker.Type> expressionTypes)
        {
            _compilationManager = manager;
            _symbolTable = symbolTable;
            _assemblyNameBase = assemblyName;
            ExpressionTypes = expressionTypes;
        }


        public System.Type GenerateAssemblyAndGetMainType(MiniCSharpParser.ProgramContext programContext)
        {
            _programContext = programContext;
            AssemblyName aName = new AssemblyName(_assemblyNameBase);
            _assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(aName, AssemblyBuilderAccess.RunAndCollect);
            _moduleBuilder = _assemblyBuilder.DefineDynamicModule(aName.Name);
            Visit(programContext); 

            foreach (var tbEntry in _definedTypes.Values)
            {
                if (!tbEntry.IsCreated())
                {
                    tbEntry.CreateTypeInfo();
                }
            }
            
            string mainClassName = programContext.IDENT().GetText();
            ClassSymbol mainClassSymbol = _symbolTable.SearchGlobal(mainClassName) as ClassSymbol;
            if (mainClassSymbol != null && _definedTypes.TryGetValue(mainClassSymbol, out TypeBuilder mainTypeBuilder))
            {
                if (!mainTypeBuilder.IsCreated()) 
                {
                    mainTypeBuilder.CreateTypeInfo(); 
                }
                return mainTypeBuilder.AsType(); 
            }
            Console.Error.WriteLine("CodeGen Error: No se pudo obtener el TypeBuilder final para la clase principal.");
            return null;
        }

        private System.Type ResolveNetType(Compiladores.Checker.Type miniCSharpType)
        {
            if (miniCSharpType == null) throw new ArgumentNullException(nameof(miniCSharpType));
            switch (miniCSharpType.Kind)
            {
                case TypeKind.Int: return typeof(int);
                case TypeKind.Double: return typeof(double);
                case TypeKind.Char: return typeof(char);
                case TypeKind.Bool: return typeof(bool);
                case TypeKind.String: return typeof(string);
                case TypeKind.Void: return typeof(void);
                case TypeKind.Null: return typeof(object);
                case TypeKind.Array:
                    var arrayType = (ArrayType)miniCSharpType;
                    System.Type elementType = ResolveNetType(arrayType.ElementType);
                    if (elementType is TypeBuilder elementTb && !elementTb.IsCreated())
                    {
                        elementTb.CreateTypeInfo();
                    }
                    return elementType.MakeArrayType();
                case TypeKind.Class:
                    Symbol classDefSymbol = _symbolTable.SearchGlobal(miniCSharpType.Name) ?? _currentCodeGenScope?.Find(miniCSharpType.Name);

                    if (classDefSymbol is ClassSymbol userClassSymbol && _definedTypes.TryGetValue(userClassSymbol, out TypeBuilder tb))
                    {
                         return tb;
                    }

                    if (_compilationManager != null && _compilationManager._compiledModulesCache.TryGetValue(miniCSharpType.Name, out var compiledModule))
                    {
                        return compiledModule.Item2;
                    }

                    if (miniCSharpType.Name == "Console") return typeof(System.Console);
                    if (miniCSharpType.Name == "MiniCSharpRuntimeHelpers") return typeof(Compiladores.MiniCSharpRuntimeHelpers);

                    Console.Error.WriteLine($"Advertencia (ResolveNetType): No se pudo resolver el tipo .NET para la clase '{miniCSharpType.Name}'. Usando 'object'.");
                    return typeof(object);
                default:
                    Console.Error.WriteLine($"Error Crítico (ResolveNetType): Tipo no soportado: {miniCSharpType.Name}");
                    return typeof(object);
            }
        }


        public override object VisitProgram(MiniCSharpParser.ProgramContext context)
        {
            _symbolTable.SetCurrentScopeTo(_symbolTable.GetGlobalScope(), _symbolTable.GetGlobalScopeLevel()); 
            _currentCodeGenScope = _symbolTable.GetGlobalScope(); 
            string className = context.IDENT().GetText(); 
            ClassSymbol progClassSymbol = _symbolTable.SearchGlobal(className) as ClassSymbol; 
            if (progClassSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No ClassSymbol para '{className}'."); return null; } 
            
            _currentTypeBuilder = _moduleBuilder.DefineType(className, TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.AutoLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit, typeof(object)); 
            if (!_definedTypes.TryAdd(progClassSymbol, _currentTypeBuilder)) { _definedTypes[progClassSymbol] = _currentTypeBuilder; } 
            
            Scope classScope = (progClassSymbol.Type as ClassType)?.Members; 
            if (classScope == null) { Console.Error.WriteLine($"CodeGen Error: ClassSymbol '{className}' no tiene Members."); return null; } 
            
            Scope outerScope = _currentCodeGenScope; 
            _currentCodeGenScope = classScope; 
            
            foreach (var varDeclContext in context.varDecl()) Visit(varDeclContext); 
            foreach (var classDeclContext in context.classDecl()) Visit(classDeclContext);  
            foreach (var methodDeclContext in context.methodDecl()) Visit(methodDeclContext); 

            _currentCodeGenScope = outerScope; 
            
            return null; 
        }
        
        public override object VisitClassDecl(MiniCSharpParser.ClassDeclContext context)
        {
            string className = context.IDENT().GetText(); 
            ClassSymbol classSymbol = _currentCodeGenScope.FindCurrent(className) as ClassSymbol; 
            if (classSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No ClassSymbol para clase anidada '{className}' en scope '{_currentTypeBuilder?.Name}'."); return null; } 
            
            TypeBuilder outerTypeBuilder = _currentTypeBuilder; 
            _currentTypeBuilder = _moduleBuilder.DefineType(className, TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.AutoLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit | TypeAttributes.Sealed, typeof(object)); 
            if (!_definedTypes.TryAdd(classSymbol, _currentTypeBuilder)) { _definedTypes[classSymbol] = _currentTypeBuilder; } 
            
            Scope outerScope = _currentCodeGenScope; 
            _currentCodeGenScope = (classSymbol.Type as ClassType)?.Members; 
            if (_currentCodeGenScope == null) { Console.Error.WriteLine($"CodeGen Error: ClassSymbol anidado '{className}' no tiene Members."); _currentTypeBuilder = outerTypeBuilder; return null; } 
            
            foreach (var varDeclContext in context.varDecl()) Visit(varDeclContext); 

            _currentCodeGenScope = outerScope; 
            
            // *** INICIO DE LA CORRECCIÓN ***
            // "Horneamos" el tipo anidado para que esté disponible para ser instanciado.
            _currentTypeBuilder.CreateTypeInfo();
            // *** FIN DE LA CORRECCIÓN ***

            _currentTypeBuilder = outerTypeBuilder; 
            return null; 
        }

        public override object VisitVarDecl(MiniCSharpParser.VarDeclContext context)
        {
            Compiladores.Checker.Type varMiniCSharpType = (Compiladores.Checker.Type)Visit(context.type()); 
            if (varMiniCSharpType == Compiladores.Checker.Type.Error) 
            {
                return null; 
            }
            System.Type varNetType = ResolveNetType(varMiniCSharpType); 

            foreach (var identNode in context.IDENT()) 
            {
                string varName = identNode.GetText(); 
                VarSymbol varSymbol = _currentCodeGenScope.FindCurrent(varName) as VarSymbol; 

                if (varSymbol == null) 
                {
                    continue; 
                }

                if (_currentMethodBuilder != null) 
                {
                    if (_ilGenerator == null) { return null; }
                    LocalBuilder lb = _ilGenerator.DeclareLocal(varNetType); 
                    _localBuilders[varSymbol] = lb; 
                }
                else 
                {
                    FieldAttributes attributes = FieldAttributes.Public; 
                    if (_currentTypeBuilder != null && _programContext != null && _currentTypeBuilder.Name == _programContext.IDENT().GetText()) 
                    {
                        attributes |= FieldAttributes.Static; 
                    }
                    FieldBuilder fb = _currentTypeBuilder.DefineField(varName, varNetType, attributes); 
                    _fieldBuilders[varSymbol] = fb; 
                }
            }
            return null; 
        }
        
        public override object VisitType(MiniCSharpParser.TypeContext context)
        {
            string typeName = context.IDENT().GetText();
            Symbol typeDefiningSymbol = _symbolTable.Search(typeName);
            if (typeDefiningSymbol == null || !(typeDefiningSymbol is TypeDefSymbol || typeDefiningSymbol is ClassSymbol))
            { return Compiladores.Checker.Type.Error; }
            
            Compiladores.Checker.Type baseType = typeDefiningSymbol.Type;
            if (context.LBRACK() != null && context.RBRACK() != null) { return new ArrayType(baseType); } 
            return baseType;
        }

        public override object VisitMethodDecl(MiniCSharpParser.MethodDeclContext context)
        {
            string methodName = context.IDENT().GetText();
            MethodSymbol methodSymbol = _currentCodeGenScope.FindCurrent(methodName) as MethodSymbol;

            if (methodSymbol == null) {
                return null;
            }

            MethodSymbol previousGeneratingMethodSymbol = _currentGeneratingMethodSymbol;
            MethodBuilder previousMethodBuilder = _currentMethodBuilder;
            ILGenerator previousILGenerator = _ilGenerator;
            
            _currentGeneratingMethodSymbol = methodSymbol;

            System.Type returnType = ResolveNetType(methodSymbol.Type);
            List<System.Type> paramTypesList = methodSymbol.Parameters.Select(p => ResolveNetType(p.Type)).ToList();
            System.Type[] paramTypes = paramTypesList.ToArray();

            MethodAttributes attributes = MethodAttributes.Public; 

            if (_currentTypeBuilder != null && _programContext != null &&
                _currentTypeBuilder.Name == _programContext.IDENT().GetText())
            {
                attributes |= MethodAttributes.Static;
            }

            _currentMethodBuilder = _currentTypeBuilder.DefineMethod(methodName, attributes, returnType, paramTypes);
            _methodBuilders[methodSymbol] = _currentMethodBuilder;

            for (int i = 0; i < methodSymbol.Parameters.Count; i++)
            {
                _currentMethodBuilder.DefineParameter(i + 1, ParameterAttributes.None, methodSymbol.Parameters[i].Name);
            }
            
            _ilGenerator = _currentMethodBuilder.GetILGenerator();
            
            Scope classScopeForCodeGen = _currentCodeGenScope; 
            int originalLevelInSymbolTable = _symbolTable.CurrentLevel;

            Scope methodScopeFromChecker = _symbolTable.GetScopeForNode(context); 

            if (methodScopeFromChecker == null) {
                _ilGenerator = previousILGenerator;
                _currentMethodBuilder = previousMethodBuilder;
                _currentGeneratingMethodSymbol = previousGeneratingMethodSymbol;
                return null;
            }
            
            _currentCodeGenScope = methodScopeFromChecker;
            int methodScopeLevelInSymbolTable = originalLevelInSymbolTable + 1; 
            if (methodSymbol.Parameters.Any()) {
                methodScopeLevelInSymbolTable = methodSymbol.Parameters.First().Level;
            } 
             _symbolTable.SetCurrentScopeTo(methodScopeFromChecker, methodScopeLevelInSymbolTable); 
            
            Visit(context.block()); 
            
            _symbolTable.SetCurrentScopeTo(classScopeForCodeGen, originalLevelInSymbolTable);
            _currentCodeGenScope = classScopeForCodeGen;

            bool isVoidMethod = (returnType == typeof(void));
            var statementsInBlock = context.block().statement();
            bool isBlockEmpty = statementsInBlock == null || !statementsInBlock.Any();
            var lastStatementNodeInBlock = statementsInBlock?.LastOrDefault();
            bool lastStatementIsReturn = lastStatementNodeInBlock != null && 
                                         lastStatementNodeInBlock.GetChild(0) is MiniCSharpParser.ReturnStatementContext;

            if (isVoidMethod && (isBlockEmpty || !lastStatementIsReturn) )
            {
                if (_ilGenerator == _currentMethodBuilder.GetILGenerator()) {
                   _ilGenerator.Emit(OpCodes.Ret);
                }
            }
            
            _currentGeneratingMethodSymbol = previousGeneratingMethodSymbol;
            _ilGenerator = previousILGenerator;
            _currentMethodBuilder = previousMethodBuilder;
            return null;
        }

        public override object VisitBlock(MiniCSharpParser.BlockContext context)
        {
            Scope scopeBeforeBlock = _currentCodeGenScope;

            Scope blockCheckerScope = _symbolTable.GetScopeForNode(context); 
            if (blockCheckerScope != null) {
                _currentCodeGenScope = blockCheckerScope;
            }

            if (context.children != null) {
                foreach (var item in context.children) {
                    if (item is MiniCSharpParser.VarDeclContext varCtx)
                    {
                        VisitVarDecl(varCtx);
                    }
                    else if (item is MiniCSharpParser.StatementContext stmtCtx)
                    {
                        Visit(stmtCtx);
                    }
                }
            }

            _currentCodeGenScope = scopeBeforeBlock;
            return null;
        }

        public override object VisitNumberFactor(MiniCSharpParser.NumberFactorContext context)
        {
            if (_ilGenerator == null) { return null; }
            if (context.number().INTCONST() != null)
            { _ilGenerator.Emit(OpCodes.Ldc_I4, int.Parse(context.number().INTCONST().GetText())); }
            else if (context.number().DOUBLECONST() != null)
            { _ilGenerator.Emit(OpCodes.Ldc_R8, double.Parse(context.number().DOUBLECONST().GetText(), System.Globalization.CultureInfo.InvariantCulture)); }
            ExpressionTypes[context] = context.number().INTCONST() != null ? Compiladores.Checker.Type.Int : Compiladores.Checker.Type.Double;
            return null;
        }
        public override object VisitCharFactor(MiniCSharpParser.CharFactorContext context)
        {
            if (_ilGenerator == null) { return null; }
            string text = context.CHARCONST().GetText(); char val = text.Length >= 3 ? text[1] : '\0';
            _ilGenerator.Emit(OpCodes.Ldc_I4, (int)val); 
            ExpressionTypes[context] = Compiladores.Checker.Type.Char;
            return null;
        }
        public override object VisitStringFactor(MiniCSharpParser.StringFactorContext context)
        {
            if (_ilGenerator == null) { return null; }
            string text = context.STRINGCONST().GetText(); string val = text.Substring(1, text.Length - 2);
            _ilGenerator.Emit(OpCodes.Ldstr, val); 
            ExpressionTypes[context] = Compiladores.Checker.Type.String;
            return null;
        }
        public override object VisitBoolFactor(MiniCSharpParser.BoolFactorContext context)
        {
            if (_ilGenerator == null) { return null; }
            _ilGenerator.Emit(context.TRUE() != null ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0); 
            ExpressionTypes[context] = Compiladores.Checker.Type.Bool;
            return null;
        }
        
        public override object VisitWriteStatement(MiniCSharpParser.WriteStatementContext context)
        {
            if (_ilGenerator == null) { return null; }
            
            var exprNode = context.expr();
            Visit(exprNode); 

            Compiladores.Checker.Type exprMiniCSharpType = GetExpressionType(exprNode); 
            
            if (exprMiniCSharpType == Compiladores.Checker.Type.Error && exprNode.GetText().Contains("len(")) {
                 exprMiniCSharpType = Compiladores.Checker.Type.Int;
            }

            System.Type netTypeForResolvedExpr = ResolveNetType(exprMiniCSharpType); 
            MethodInfo writeLineMethod = null;
            bool needsBoxing = false;

            writeLineMethod = typeof(System.Console).GetMethod("WriteLine", new System.Type[] { netTypeForResolvedExpr });
            
            if (writeLineMethod == null) 
            {
                writeLineMethod = typeof(System.Console).GetMethod("WriteLine", new System.Type[] { typeof(object) });
                System.Type originalNetType = ResolveNetType(exprMiniCSharpType); 
                if (originalNetType.IsValueType && originalNetType != typeof(void)) { 
                    needsBoxing = true;
                }
            }
            else 
            {
                 if (netTypeForResolvedExpr == typeof(object) && 
                     exprMiniCSharpType != Compiladores.Checker.Type.String && 
                     exprMiniCSharpType != Compiladores.Checker.Type.Null &&  
                     ResolveNetType(exprMiniCSharpType).IsValueType) 
                 {
                    needsBoxing = true;
                 }
            }

            if (writeLineMethod != null)
            {
                if (needsBoxing) {
                     System.Type typeOnStackForBoxing = ResolveNetType(GetExpressionType(exprNode)); 
                     if(typeOnStackForBoxing != typeof(object) && typeOnStackForBoxing.IsValueType) 
                     {
                        _ilGenerator.Emit(OpCodes.Box, typeOnStackForBoxing);
                     }
                }
                _ilGenerator.Emit(OpCodes.Call, writeLineMethod);
            }
            else
            {
                if (exprMiniCSharpType != Compiladores.Checker.Type.Void && _ilGenerator.ILOffset > 0) 
                {
                    _ilGenerator.Emit(OpCodes.Pop);
                }
            }
            return null;
        }
        
        private Compiladores.Checker.Type GetExpressionType(IParseTree treeNode)
        {
            if (treeNode == null)
            {
                return Compiladores.Checker.Type.Error;
            }

            if (ExpressionTypes.TryGetValue(treeNode, out Compiladores.Checker.Type type))
            {
                if (type == null)
                {
                    return Compiladores.Checker.Type.Error;
                }
                return type;
            }
            
            if (treeNode is MiniCSharpParser.ExprContext exprCtx && exprCtx.term().Length == 1 && exprCtx.addop().Length == 0 && exprCtx.MINUS() == null && exprCtx.cast() == null)
            {
                return GetExpressionType(exprCtx.term(0));
            }
            if (treeNode is MiniCSharpParser.TermContext termCtx && termCtx.factor().Length == 1 && termCtx.mulop().Length == 0)
            {
                return GetExpressionType(termCtx.factor(0));
            }
            if (treeNode is MiniCSharpParser.DesignatorFactorContext dfCtx) 
            {
                 if (dfCtx.LPAREN() == null)
                    return GetExpressionType(dfCtx.designator());
            }
             if (treeNode is MiniCSharpParser.DesignatorContext desCtx)
            {
                string baseName = desCtx.IDENT(0).GetText();
                Symbol sym = _currentCodeGenScope?.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
                if (sym != null && sym.Type != null)
                {
                    return sym.Type;
                }
            }
            
            return Compiladores.Checker.Type.Error;
        }

        public override object VisitReturnStatement(MiniCSharpParser.ReturnStatementContext context)
        {
            if (_ilGenerator == null) { return null; }
            if (context.expr() != null) { Visit(context.expr()); }
            _ilGenerator.Emit(OpCodes.Ret);
            return null;
        }
        
        private void EmitLoadDesignator(MiniCSharpParser.DesignatorContext designatorCtx, out Compiladores.Checker.Type finalType)
        {
            finalType = Compiladores.Checker.Type.Error;
            if (designatorCtx.DOT().Length > 0)
            {
                string objectName = designatorCtx.IDENT(0).GetText();
                string fieldName = designatorCtx.IDENT(1).GetText();

                VarSymbol objectSymbol = (_currentCodeGenScope.Find(objectName) ?? _symbolTable.SearchGlobal(objectName)) as VarSymbol;
                if (objectSymbol == null) return;
                
                EmitLoadVariable(objectSymbol); 

                if (objectSymbol.Type is ClassType classType)
                {
                    VarSymbol fieldSymbol = classType.Members.Find(fieldName) as VarSymbol;
                    if (fieldSymbol != null && _fieldBuilders.TryGetValue(fieldSymbol, out FieldBuilder fb))
                    {
                        _ilGenerator.Emit(OpCodes.Ldfld, fb);
                        finalType = fieldSymbol.Type;
                    }
                }
            }
        }
        
        private void EmitLoadVariable(VarSymbol varSymbol)
        {
            if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lb))
            {
                _ilGenerator.Emit(OpCodes.Ldloc, lb);
            }
            else if (_currentGeneratingMethodSymbol != null && _currentGeneratingMethodSymbol.Parameters.Contains(varSymbol))
            {
                int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(varSymbol);
                _ilGenerator.Emit(OpCodes.Ldarg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
            }
            else if (_fieldBuilders.TryGetValue(varSymbol, out FieldBuilder fb))
            {
                if (!fb.IsStatic)
                {
                    _ilGenerator.Emit(OpCodes.Ldarg_0);
                }
                _ilGenerator.Emit(fb.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fb);
            }
        }

        public override object VisitDesignatorFactor(MiniCSharpParser.DesignatorFactorContext context)
        {
            if (_ilGenerator == null) { return null; }

            var designatorNode = context.designator();

            if (context.LPAREN() != null)
            {
                MethodSymbol calledMethodSym = ResolveDesignatorToCallableSymbol(designatorNode);
                Compiladores.Checker.Type methodReturnType = calledMethodSym?.Type ?? Compiladores.Checker.Type.Error;
                HandleMethodCall(designatorNode, context.actPars());
                ExpressionTypes[context] = methodReturnType;
            }
            else
            {
                Compiladores.Checker.Type designatorValueType = GetExpressionType(designatorNode);
                ExpressionTypes[context] = designatorValueType;

                string baseName = designatorNode.IDENT(0).GetText();
                Symbol symbol = _currentCodeGenScope.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);

                if (symbol is VarSymbol varSymbol)
                {
                    if (designatorNode.DOT().Length > 0)
                    {
                        EmitLoadDesignator(designatorNode, out designatorValueType);
                        ExpressionTypes[context] = designatorValueType;
                    }
                    else if (designatorNode.LBRACK().Length > 0)
                    {
                         EmitLoadArrayAndIndex(varSymbol, designatorNode.expr(0));
                        _ilGenerator.Emit(GetLdElemOpCode(((ArrayType)varSymbol.Type).ElementType));
                    }
                    else
                    {
                        EmitLoadVariable(varSymbol);
                    }
                }
            }
            return null;
        }

        public override object VisitDesignatorStatement(MiniCSharpParser.DesignatorStatementContext context)
        {
            if (_ilGenerator == null) { return null; }

            var designatorNode = context.designator();

            if (context.ASSIGN() != null) 
            {
                Compiladores.Checker.Type lhsType = GetExpressionType(designatorNode);
                
                if (designatorNode.DOT().Length > 0)
                {
                    string objectName = designatorNode.IDENT(0).GetText();
                    VarSymbol objectSymbol = (_currentCodeGenScope.Find(objectName) ?? _symbolTable.SearchGlobal(objectName)) as VarSymbol;
                    if (objectSymbol != null) EmitLoadVariable(objectSymbol);
                }
                else if (designatorNode.LBRACK().Length > 0)
                {
                     string baseArrayName = designatorNode.IDENT(0).GetText();
                     Symbol baseArraySymbol = _currentCodeGenScope.Find(baseArrayName) ?? _symbolTable.SearchGlobal(baseArrayName);
                     if (baseArraySymbol is VarSymbol arrayVarSymbol)
                     {
                        EmitLoadArrayAndIndex(arrayVarSymbol, designatorNode.expr(0));
                     }
                }

                Visit(context.expr()); 
                Compiladores.Checker.Type rhsType = GetExpressionType(context.expr());
                
                if (lhsType == Compiladores.Checker.Type.Double && rhsType == Compiladores.Checker.Type.Int)
                {
                    _ilGenerator.Emit(OpCodes.Conv_R8);
                }

                EmitStoreToDesignator(designatorNode, lhsType); 
            }
            else if (context.INC() != null || context.DEC() != null) 
            {
                 string baseIdentName = designatorNode.IDENT(0).GetText();
                Symbol symbol = _currentCodeGenScope.Find(baseIdentName); 
                bool isArrayElement = designatorNode.LBRACK().Length > 0;
                bool isField = false;
                FieldBuilder fieldForIncDec = null;

                if (symbol == null && !isArrayElement && _currentTypeBuilder != null) {
                    Scope classScope = GetCurrentClassScopeForFieldAccess(baseIdentName);
                    if (classScope != null) {
                         symbol = classScope.FindCurrent(baseIdentName);
                         if (symbol is VarSymbol vs_inc_dec && _fieldBuilders.TryGetValue(vs_inc_dec, out fieldForIncDec)) {
                             isField = true;
                         } else { symbol = null; }
                    }
                } else if (symbol is VarSymbol vs_direct_field && _fieldBuilders.TryGetValue(vs_direct_field, out fieldForIncDec)) {
                    isField = true; 
                }

                if (symbol == null && !isArrayElement) { 
                    return null;
                }

                VarSymbol varSymbol_inc_dec = null;
                Compiladores.Checker.Type targetType; 

                if (isArrayElement) {
                    Symbol arrayBaseSymbol = _currentCodeGenScope.Find(baseIdentName);
                     if (arrayBaseSymbol == null && _currentTypeBuilder != null) {
                        Scope classScope = GetCurrentClassScopeForFieldAccess(baseIdentName);
                         if (classScope != null) arrayBaseSymbol = classScope.FindCurrent(baseIdentName);
                     }
                    if (!(arrayBaseSymbol is VarSymbol arrayVarSym) || !(arrayVarSym.Type is ArrayType arrayTypeObj)) {
                        return null;
                    }
                    varSymbol_inc_dec = arrayVarSym; 
                    targetType = arrayTypeObj.ElementType; 
                } else {
                    varSymbol_inc_dec = symbol as VarSymbol;
                    if (varSymbol_inc_dec == null) {
                         return null;
                    }
                    targetType = varSymbol_inc_dec.Type;
                }
                
                if (targetType != Compiladores.Checker.Type.Int && targetType != Compiladores.Checker.Type.Double) {
                     return null;
                }
                
                LocalBuilder tempArrayIndexLocal = null; 
                LocalBuilder tempArrayRefLocal = null; 

                if (isArrayElement)
                {
                    EmitLoadArrayAndIndex(varSymbol_inc_dec, designatorNode.expr(0)); 
                    
                    tempArrayIndexLocal = _ilGenerator.DeclareLocal(typeof(int));
                    _ilGenerator.Emit(OpCodes.Dup); 
                    _ilGenerator.Emit(OpCodes.Stloc, tempArrayIndexLocal); 

                    LocalBuilder tempIndexForLd = _ilGenerator.DeclareLocal(typeof(int));
                    _ilGenerator.Emit(OpCodes.Stloc, tempIndexForLd); 
                    tempArrayRefLocal = _ilGenerator.DeclareLocal(ResolveNetType(varSymbol_inc_dec.Type));
                    _ilGenerator.Emit(OpCodes.Dup); 
                    _ilGenerator.Emit(OpCodes.Stloc, tempArrayRefLocal); 
                    
                    _ilGenerator.Emit(OpCodes.Ldloc, tempIndexForLd); 
                    _ilGenerator.Emit(GetLdElemOpCode(targetType)); 
                }
                else if (isField) 
                {
                    if (!fieldForIncDec.IsStatic) 
                    {
                        _ilGenerator.Emit(OpCodes.Ldarg_0); 
                        _ilGenerator.Emit(OpCodes.Dup);     
                    }
                    _ilGenerator.Emit(fieldForIncDec.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fieldForIncDec); 
                }
                else if (_localBuilders.TryGetValue(varSymbol_inc_dec, out LocalBuilder lbIncDec)) 
                {
                    _ilGenerator.Emit(OpCodes.Ldloc, lbIncDec);
                }
                else if (_currentGeneratingMethodSymbol != null &&
                         _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == varSymbol_inc_dec.Name && p.Level == varSymbol_inc_dec.Level) is VarSymbol paramSymbol_inc_dec) 
                {
                    int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol_inc_dec);
                    _ilGenerator.Emit(OpCodes.Ldarg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
                } 

                _ilGenerator.Emit(OpCodes.Ldc_I4_1);
                if (targetType == Compiladores.Checker.Type.Double)
                {
                    _ilGenerator.Emit(OpCodes.Conv_R8); 
                }

                if (context.INC() != null) _ilGenerator.Emit(OpCodes.Add);
                else _ilGenerator.Emit(OpCodes.Sub);
                
                if (isArrayElement)
                {
                    LocalBuilder tempNewValue = _ilGenerator.DeclareLocal(ResolveNetType(targetType));
                    _ilGenerator.Emit(OpCodes.Stloc, tempNewValue); 

                    _ilGenerator.Emit(OpCodes.Ldloc, tempArrayRefLocal); 
                    _ilGenerator.Emit(OpCodes.Ldloc, tempArrayIndexLocal); 
                    _ilGenerator.Emit(OpCodes.Ldloc, tempNewValue); 
                    _ilGenerator.Emit(GetStElemOpCode(targetType));
                }
                else if (isField) 
                {
                    _ilGenerator.Emit(fieldForIncDec.IsStatic ? OpCodes.Stsfld : OpCodes.Stfld, fieldForIncDec);
                }
                else if (_localBuilders.ContainsKey(varSymbol_inc_dec))
                {
                    _ilGenerator.Emit(OpCodes.Stloc, _localBuilders[varSymbol_inc_dec]);
                }
                else if (_currentGeneratingMethodSymbol != null &&
                         _currentGeneratingMethodSymbol.Parameters.Any(p => p.Name == varSymbol_inc_dec.Name && p.Level == varSymbol_inc_dec.Level) )
                {
                    VarSymbol paramSymbol_for_store = _currentGeneratingMethodSymbol.Parameters.First(p => p.Name == varSymbol_inc_dec.Name && p.Level == varSymbol_inc_dec.Level);
                    int paramIndex_for_store = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol_for_store);
                    _ilGenerator.Emit(OpCodes.Starg, (short)(_currentMethodBuilder.IsStatic ? paramIndex_for_store : paramIndex_for_store + 1));
                }
            }
            else if (context.LPAREN() != null) 
            {
                HandleMethodCall(designatorNode, context.actPars());
                Symbol calledSymbol = ResolveDesignatorToCallableSymbol(designatorNode);
                if (calledSymbol is MethodSymbol calledMethodSym)
                {
                    if (calledMethodSym.Type != Compiladores.Checker.Type.Void)
                    {
                        _ilGenerator.Emit(OpCodes.Pop);
                    }
                } else {
                     var callExpressionType = GetExpressionType(context.designator().Parent); 
                     if (callExpressionType != null && callExpressionType != Compiladores.Checker.Type.Void && callExpressionType != Compiladores.Checker.Type.Error) {
                        _ilGenerator.Emit(OpCodes.Pop);
                     } 
                }
            }
            return null;
        }

        private void EmitLoadArrayAndIndex(VarSymbol arrayVarSymbol, MiniCSharpParser.ExprContext indexExprContext)
        {
            if (_localBuilders.TryGetValue(arrayVarSymbol, out LocalBuilder lb))
            {
                _ilGenerator.Emit(OpCodes.Ldloc, lb);
            }
            else if (_currentGeneratingMethodSymbol != null &&
                     _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == arrayVarSymbol.Name && p.Level == arrayVarSymbol.Level) is VarSymbol paramSymbol)
            {
                int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                _ilGenerator.Emit(OpCodes.Ldarg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
            }
            else if (_fieldBuilders.TryGetValue(arrayVarSymbol, out FieldBuilder fb))
            {
                if (!fb.IsStatic) _ilGenerator.Emit(OpCodes.Ldarg_0); 
                _ilGenerator.Emit(fb.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fb);
            }
            else
            {
                return;
            }
            Visit(indexExprContext); 
        }
        
        private Scope GetCurrentClassScopeForFieldAccess(string fieldNameHint) {
            if (_currentTypeBuilder == null) return null;
            Symbol classSym = null;
            if (_currentTypeBuilder.IsNested && _currentTypeBuilder.DeclaringType != null) {
                Symbol outerClassSym = _symbolTable.SearchGlobal(_currentTypeBuilder.DeclaringType.Name);
                if (outerClassSym is ClassSymbol ocs && ocs.Type is ClassType oct) {
                    classSym = oct.Members.FindCurrent(_currentTypeBuilder.Name);
                }
            } else { 
                classSym = _symbolTable.SearchGlobal(_currentTypeBuilder.Name);
            }

            if (classSym is ClassSymbol cs && cs.Type is ClassType ct) {
                return ct.Members;
            }
            if (_programContext != null && _currentTypeBuilder.Name == _programContext.IDENT().GetText()){
                var progClassSymbol = _symbolTable.SearchGlobal(_programContext.IDENT().GetText()) as ClassSymbol;
                if (progClassSymbol != null && progClassSymbol.Type is ClassType pct) {
                    return pct.Members;
                }
            }
    
            return null;
        }
        
        private void EmitStoreToDesignator(MiniCSharpParser.DesignatorContext designatorCtx, Compiladores.Checker.Type valueTypeOnStack)
        {
            if (_ilGenerator == null) { return; }
            if (designatorCtx == null) { return; }
            
            string baseName = designatorCtx.IDENT(0).GetText();
            
            if (designatorCtx.DOT().Length > 0)
            {
                string fieldName = designatorCtx.IDENT(1).GetText();
                Symbol objectSymbol = _currentCodeGenScope.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
                if (objectSymbol is VarSymbol varObj && varObj.Type is ClassType classType)
                {
                    Symbol fieldSymbol = classType.Members.Find(fieldName);
                    if (fieldSymbol is VarSymbol varField && _fieldBuilders.TryGetValue(varField, out FieldBuilder fb))
                    {
                        _ilGenerator.Emit(OpCodes.Stfld, fb);
                    }
                }
            }
            else if (designatorCtx.LBRACK().Length > 0) 
            {
                Symbol arrayBaseSymbol = _currentCodeGenScope.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
                if (arrayBaseSymbol is VarSymbol vs && vs.Type is ArrayType arrayT)
                {
                    _ilGenerator.Emit(GetStElemOpCode(arrayT.ElementType));
                }
            }
            else 
            {
                Symbol symbol = _currentCodeGenScope.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
                if (symbol is VarSymbol varSymbol)
                {
                    if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lb))
                    {
                        _ilGenerator.Emit(OpCodes.Stloc, lb);
                    }
                    else if (_currentGeneratingMethodSymbol != null && _currentGeneratingMethodSymbol.Parameters.Contains(varSymbol))
                    {
                        int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(varSymbol);
                        _ilGenerator.Emit(OpCodes.Starg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
                    }
                    else if (_fieldBuilders.TryGetValue(varSymbol, out FieldBuilder fb))
                    {
                        if (!fb.IsStatic)
                        {
                            LocalBuilder tempVal = _ilGenerator.DeclareLocal(ResolveNetType(valueTypeOnStack));
                            _ilGenerator.Emit(OpCodes.Stloc, tempVal);      
                            _ilGenerator.Emit(OpCodes.Ldarg_0);             
                            _ilGenerator.Emit(OpCodes.Ldloc, tempVal);      
                            _ilGenerator.Emit(OpCodes.Stfld, fb);
                        }
                        else
                        {
                           _ilGenerator.Emit(OpCodes.Stsfld, fb);
                        }
                    }
                }
            }
        }

        private OpCode GetLdElemOpCode(Compiladores.Checker.Type elementType)
        {
            System.Type netElementType = ResolveNetType(elementType);
            if (netElementType == typeof(int)) return OpCodes.Ldelem_I4;
            if (netElementType == typeof(double)) return OpCodes.Ldelem_R8;
            if (netElementType == typeof(char)) return OpCodes.Ldelem_U2; 
            if (netElementType == typeof(bool)) return OpCodes.Ldelem_I1; 
            if (!netElementType.IsValueType || netElementType.IsGenericParameter) return OpCodes.Ldelem_Ref; 

            return OpCodes.Ldelem_Ref; 
        }

        private OpCode GetStElemOpCode(Compiladores.Checker.Type elementType)
        {
            System.Type netElementType = ResolveNetType(elementType);
            if (netElementType == typeof(int)) return OpCodes.Stelem_I4;
            if (netElementType == typeof(double)) return OpCodes.Stelem_R8;
            if (netElementType == typeof(char)) return OpCodes.Stelem_I2; 
            if (netElementType == typeof(bool)) return OpCodes.Stelem_I1; 
            if (!netElementType.IsValueType || netElementType.IsGenericParameter) return OpCodes.Stelem_Ref;

            return OpCodes.Stelem_Ref;
        }

        private void HandleMethodCall(MiniCSharpParser.DesignatorContext designatorCtx, MiniCSharpParser.ActParsContext actParsCtx)
        {
            MethodSymbol resolvedMethodSymbol = ResolveDesignatorToCallableSymbol(designatorCtx);
            MethodBase methodToCall = null;

            if (resolvedMethodSymbol != null && resolvedMethodSymbol.Name == "len")
            {
                if (_ilGenerator == null)
                {
                    return;
                }
                if (actParsCtx == null || actParsCtx.expr().Length != 1)
                {
                    if (actParsCtx != null) foreach (var argExpr_len in actParsCtx.expr()) if (GetExpressionType(argExpr_len) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    _ilGenerator.Emit(OpCodes.Ldc_I4_M1);
                    return;
                }

                var arrayExpr_len = actParsCtx.expr(0);
                Visit(arrayExpr_len);

                Compiladores.Checker.Type arrayExprType_len = GetExpressionType(arrayExpr_len);
                if (arrayExprType_len.Kind != TypeKind.Array)
                {
                    if (arrayExprType_len != Compiladores.Checker.Type.Void && arrayExprType_len != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    _ilGenerator.Emit(OpCodes.Ldc_I4_M1);
                    return;
                }

                _ilGenerator.Emit(OpCodes.Ldlen);
                _ilGenerator.Emit(OpCodes.Conv_I4);
                return;
            }
            else if (resolvedMethodSymbol != null && resolvedMethodSymbol.Name == "add")
            {
                if (_ilGenerator == null) { return; }
                if (actParsCtx == null || actParsCtx.expr().Length != 2)
                {
                    if (actParsCtx != null) foreach (var argExpr_add in actParsCtx.expr()) if (GetExpressionType(argExpr_add) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }

                var arrayArgExprCtx = actParsCtx.expr(0);
                var elementArgExprCtx = actParsCtx.expr(1);

                MiniCSharpParser.DesignatorContext arrayDesignatorCtx = null;
                if (arrayArgExprCtx.term(0)?.factor(0) is MiniCSharpParser.DesignatorFactorContext dfCtx)
                {
                    arrayDesignatorCtx = dfCtx.designator();
                }
                if (arrayDesignatorCtx == null)
                {
                    Visit(arrayArgExprCtx); if (GetExpressionType(arrayArgExprCtx) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    Visit(elementArgExprCtx); if (GetExpressionType(elementArgExprCtx) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }

                Visit(arrayArgExprCtx);
                Compiladores.Checker.Type actualArrayType = GetExpressionType(arrayArgExprCtx);
                if (actualArrayType.Kind != TypeKind.Array)
                {
                    if (actualArrayType != Compiladores.Checker.Type.Void && actualArrayType != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    Visit(elementArgExprCtx); if (GetExpressionType(elementArgExprCtx) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }
                ArrayType arrMetaType = (ArrayType)actualArrayType;
                Compiladores.Checker.Type elementMetaType = arrMetaType.ElementType;

                Visit(elementArgExprCtx);
                Compiladores.Checker.Type actualElementType = GetExpressionType(elementArgExprCtx);

                System.Type helperParamArrayNetType = ResolveNetType(actualArrayType);
                System.Type helperParamElementNetType = ResolveNetType(elementMetaType);
                System.Type actualElementNetTypeOnStack = ResolveNetType(actualElementType);

                if (helperParamElementNetType == typeof(double) && actualElementNetTypeOnStack == typeof(int))
                {
                    _ilGenerator.Emit(OpCodes.Conv_R8);
                }

                MethodInfo helperMethodInfo_add = null;
                if (elementMetaType == Compiladores.Checker.Type.Int)
                    helperMethodInfo_add = typeof(MiniCSharpRuntimeHelpers).GetMethod("AddIntElement", new[] { typeof(int[]), typeof(int) });
                else if (elementMetaType == Compiladores.Checker.Type.Char)
                    helperMethodInfo_add = typeof(MiniCSharpRuntimeHelpers).GetMethod("AddCharElement", new[] { typeof(char[]), typeof(char) });
                else if (elementMetaType == Compiladores.Checker.Type.Double)
                    helperMethodInfo_add = typeof(MiniCSharpRuntimeHelpers).GetMethod("AddDoubleElement", new[] { typeof(double[]), typeof(double) });

                if (helperMethodInfo_add == null)
                {
                    if (actualElementType != Compiladores.Checker.Type.Void && actualElementType != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    if (actualArrayType != Compiladores.Checker.Type.Void && actualArrayType != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }

                _ilGenerator.Emit(OpCodes.Call, helperMethodInfo_add);
                EmitStoreToDesignator(arrayDesignatorCtx, actualArrayType);
                return;
            }
            else if (resolvedMethodSymbol != null && resolvedMethodSymbol.Name == "del")
            {
                if (_ilGenerator == null) { return; }
                if (actParsCtx == null || actParsCtx.expr().Length != 2)
                {
                    if (actParsCtx != null) foreach (var argExpr_del in actParsCtx.expr()) if (GetExpressionType(argExpr_del) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }

                var arrayArgExprCtx_del = actParsCtx.expr(0);
                var indexArgExprCtx_del = actParsCtx.expr(1);

                MiniCSharpParser.DesignatorContext arrayDesignatorCtx_del = null;
                if (arrayArgExprCtx_del.term(0)?.factor(0) is MiniCSharpParser.DesignatorFactorContext dfCtx_del)
                {
                    arrayDesignatorCtx_del = dfCtx_del.designator();
                }
                if (arrayDesignatorCtx_del == null)
                {
                    Visit(arrayArgExprCtx_del); if (GetExpressionType(arrayArgExprCtx_del) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    Visit(indexArgExprCtx_del); if (GetExpressionType(indexArgExprCtx_del) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }
                Visit(arrayArgExprCtx_del);
                Compiladores.Checker.Type actualArrayType_del = GetExpressionType(arrayArgExprCtx_del);
                if (actualArrayType_del.Kind != TypeKind.Array)
                {
                    if (actualArrayType_del != Compiladores.Checker.Type.Void && actualArrayType_del != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    Visit(indexArgExprCtx_del); if (GetExpressionType(indexArgExprCtx_del) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }
                ArrayType arrMetaType_del = (ArrayType)actualArrayType_del;
                Compiladores.Checker.Type elementMetaType_del = arrMetaType_del.ElementType;
                Visit(indexArgExprCtx_del);
                Compiladores.Checker.Type actualIndexType_del = GetExpressionType(indexArgExprCtx_del);
                if (actualIndexType_del != Compiladores.Checker.Type.Int)
                {
                    if (actualIndexType_del != Compiladores.Checker.Type.Void && actualIndexType_del != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    if (actualArrayType_del != Compiladores.Checker.Type.Void && actualArrayType_del != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }

                MethodInfo helperMethodInfo_del = null;
                if (elementMetaType_del == Compiladores.Checker.Type.Int)
                    helperMethodInfo_del = typeof(MiniCSharpRuntimeHelpers).GetMethod("DeleteIntElementAt", new[] { typeof(int[]), typeof(int) });
                else if (elementMetaType_del == Compiladores.Checker.Type.Char)
                    helperMethodInfo_del = typeof(MiniCSharpRuntimeHelpers).GetMethod("DeleteCharElementAt", new[] { typeof(char[]), typeof(int) });
                else if (elementMetaType_del == Compiladores.Checker.Type.Double)
                    helperMethodInfo_del = typeof(MiniCSharpRuntimeHelpers).GetMethod("DeleteDoubleElementAt", new[] { typeof(double[]), typeof(int) });

                if (helperMethodInfo_del == null)
                {
                    if (actualIndexType_del != Compiladores.Checker.Type.Void && actualIndexType_del != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    if (actualArrayType_del != Compiladores.Checker.Type.Void && actualArrayType_del != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }

                _ilGenerator.Emit(OpCodes.Call, helperMethodInfo_del);

                EmitStoreToDesignator(arrayDesignatorCtx_del, actualArrayType_del);
                return;
            }

            if (resolvedMethodSymbol != null)
            {
                if (_methodBuilders.TryGetValue(resolvedMethodSymbol, out MethodBuilder mb))
                {
                    methodToCall = mb;
                }
                else
                {
                    var objectName = designatorCtx.IDENT(0).GetText();
                    var objectSymbol = _currentCodeGenScope.Find(objectName);
                    if (objectSymbol != null)
                    {
                        var objectType = ResolveNetType(objectSymbol.Type);
                        if (objectType != typeof(object))
                        {
                            var paramTypes = resolvedMethodSymbol.Parameters.Select(p => ResolveNetType(p.Type)).ToArray();
                            methodToCall = objectType.GetMethod(resolvedMethodSymbol.Name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static, null, paramTypes, null);
                        }
                    }

                    if (methodToCall == null)
                    {
                        if (actParsCtx != null) foreach (var argExpr in actParsCtx.expr()) if (GetExpressionType(argExpr) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                        return;
                    }
                }
            }
            else
            {
                string fullMethodName = designatorCtx.GetText();
                if (designatorCtx.DOT().Length > 0 && designatorCtx.IDENT(0).GetText() == "Console")
                {
                    string consoleMethodName = designatorCtx.IDENT(1).GetText();
                    List<System.Type> argTypes = new List<System.Type>();
                    if (actParsCtx != null)
                    {
                        foreach (var argExpr in actParsCtx.expr())
                        {
                            argTypes.Add(ResolveNetType(GetExpressionType(argExpr)));
                        }
                    }
                    try
                    {
                        methodToCall = typeof(System.Console).GetMethod(consoleMethodName, argTypes.ToArray());

                        if (methodToCall == null && consoleMethodName == "WriteLine" && argTypes.Count == 1)
                        {
                            methodToCall = typeof(System.Console).GetMethod("WriteLine", new[] { typeof(object) });
                        }

                        if (methodToCall == null)
                        {
                            return;
                        }
                    }
                    catch (AmbiguousMatchException)
                    {
                        return;
                    }
                }
                else
                {
                    if (actParsCtx != null) foreach (var argExpr in actParsCtx.expr()) if (GetExpressionType(argExpr) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }
            }

            bool isStaticCall = true;
            bool isInstanceMethodOnThis = false;

            if (methodToCall is MethodBuilder mbUser) isStaticCall = mbUser.IsStatic;
            else if (methodToCall is MethodInfo miReflected) isStaticCall = miReflected.IsStatic;

            if (resolvedMethodSymbol != null && !isStaticCall)
            {
                if (designatorCtx.DOT().Length == 0)
                {
                    if ((_currentMethodBuilder?.IsStatic ?? true))
                    {
                        return;
                    }
                    _ilGenerator.Emit(OpCodes.Ldarg_0);
                    isInstanceMethodOnThis = true;
                }
                else if (designatorCtx.DOT().Length > 0)
                {
                    var objectName = designatorCtx.IDENT(0).GetText();
                    var objectSymbol = _currentCodeGenScope.Find(objectName) as VarSymbol;
                    if (objectSymbol != null)
                    {
                        if (_localBuilders.TryGetValue(objectSymbol, out LocalBuilder lb)) _ilGenerator.Emit(OpCodes.Ldloc, lb);
                        else if (_currentGeneratingMethodSymbol != null && _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == objectSymbol.Name && p.Level == objectSymbol.Level) is VarSymbol paramSymbol)
                        {
                            int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                            _ilGenerator.Emit(OpCodes.Ldarg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
                        }
                        else if (_fieldBuilders.TryGetValue(objectSymbol, out FieldBuilder fb_field))
                        {
                            if (!fb_field.IsStatic) _ilGenerator.Emit(OpCodes.Ldarg_0);
                            _ilGenerator.Emit(fb_field.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fb_field);
                        }
                    }
                }
            }

            if (actParsCtx != null)
            {
                var formalParams = resolvedMethodSymbol?.Parameters;
                if (formalParams != null && formalParams.Count != actParsCtx.expr().Length && resolvedMethodSymbol != null)
                {
                    if (isInstanceMethodOnThis) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }

                for (int i = 0; i < actParsCtx.expr().Length; i++)
                {
                    var argExpr = actParsCtx.expr(i);
                    Visit(argExpr);

                    if (formalParams != null && i < formalParams.Count)
                    {
                        Compiladores.Checker.Type formalType = formalParams[i].Type;
                        Compiladores.Checker.Type actualType = GetExpressionType(argExpr);

                        if (formalType == Compiladores.Checker.Type.Double && actualType == Compiladores.Checker.Type.Int)
                        {
                            _ilGenerator.Emit(OpCodes.Conv_R8);
                        }
                        else if (formalType == Compiladores.Checker.Type.Int && actualType == Compiladores.Checker.Type.Double)
                        {
                            _ilGenerator.Emit(OpCodes.Conv_I4);
                        }
                    }
                    else if (methodToCall is MethodInfo reflectedMi)
                    {
                        ParameterInfo[] reflectedParams = reflectedMi.GetParameters();
                        if (i < reflectedParams.Length)
                        {
                            System.Type formalNetType = reflectedParams[i].ParameterType;
                            Compiladores.Checker.Type actualMiniCSharpType = GetExpressionType(argExpr);
                            System.Type actualNetType = ResolveNetType(actualMiniCSharpType);

                            if (formalNetType == typeof(double) && actualNetType == typeof(int))
                            {
                                _ilGenerator.Emit(OpCodes.Conv_R8);
                            }
                            else if (formalNetType == typeof(object) && actualNetType.IsValueType)
                            {
                                _ilGenerator.Emit(OpCodes.Box, actualNetType);
                            }
                        }
                    }
                }
            }

            if (methodToCall != null)
            {
                OpCode callInstruction = methodToCall.IsStatic ? OpCodes.Call : OpCodes.Callvirt;
                if (methodToCall is MethodInfo mi)
                {
                    _ilGenerator.Emit(callInstruction, mi);
                }
                else if (methodToCall is ConstructorInfo)
                {
                    _ilGenerator.Emit(callInstruction, (MethodBuilder)methodToCall);
                }
                else
                {
                    if (actParsCtx != null) foreach (var argExpr in actParsCtx.expr()) if (GetExpressionType(argExpr) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    if (isInstanceMethodOnThis) _ilGenerator.Emit(OpCodes.Pop);
                }
            }
            else
            {
                if (actParsCtx != null) foreach (var argExpr in actParsCtx.expr()) if (GetExpressionType(argExpr) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                if (isInstanceMethodOnThis) _ilGenerator.Emit(OpCodes.Pop);
            }
        }


        private MethodSymbol ResolveDesignatorToCallableSymbol(MiniCSharpParser.DesignatorContext designatorCtx)
        {
            if (designatorCtx == null || designatorCtx.IDENT() == null || designatorCtx.IDENT().Length == 0)
            {
                return null;
            }

            if (designatorCtx.DOT().Length == 0 && designatorCtx.LBRACK().Length == 0)
            {
                string methodName = designatorCtx.IDENT(0).GetText();
                Symbol symbol = _currentCodeGenScope.Find(methodName); 

                if (symbol is MethodSymbol methodSymbol)
                {
                    return methodSymbol;
                }
                else if (symbol != null)
                {
                    return null; 
                }
                Symbol globalSymbol = _symbolTable.SearchGlobal(methodName);
                 if (globalSymbol is MethodSymbol globalMethodSymbol)
                {
                    return globalMethodSymbol;
                }
                return null; 
            }

            if (designatorCtx.DOT().Length > 0)
            {
                string baseIdentifierName = designatorCtx.IDENT(0).GetText();
                string memberMethodName = designatorCtx.IDENT(1).GetText(); 

                Symbol baseSymbol = _currentCodeGenScope.Find(baseIdentifierName) ?? _symbolTable.SearchGlobal(baseIdentifierName);

                if (baseSymbol is ClassSymbol classSymbol) 
                {
                    if (classSymbol.Type is ClassType classType) 
                    {
                        Symbol memberSymbol = classType.Members.FindCurrent(memberMethodName); 
                        if (memberSymbol is MethodSymbol methodSymbol)
                        {
                            return methodSymbol;
                        }
                    }
                }
                else if (baseIdentifierName == "Console") 
                {
                    return null; 
                }
                 else if (baseIdentifierName == "MiniCSharpRuntimeHelpers")
                {
                    if (memberMethodName.StartsWith("Add") || memberMethodName.StartsWith("Del")) {
                        return null; 
                    }
                }
                else if (baseSymbol is VarSymbol varSym) 
                {
                     if (varSym.Type is ClassType instanceClassType) 
                     {
                         Symbol memberSymbol = instanceClassType.Members.FindCurrent(memberMethodName);
                         if (memberSymbol is MethodSymbol methodSymbol)
                         {
                             return methodSymbol;
                         }
                     }
                }
                return null;
            }
            
            return null;
        }
        
        public override object VisitExpr(MiniCSharpParser.ExprContext context)
        {
            if (_ilGenerator == null) { return null; }

            Compiladores.Checker.Type currentType;
            
            Visit(context.term(0));
            currentType = GetExpressionType(context.term(0));

            if (context.cast() != null)
            {
                Compiladores.Checker.Type targetType = GetExpressionType(context.cast());
                EmitConversion(currentType, targetType);
                currentType = targetType;
            }
            
            if (context.MINUS() != null)
            {
                if (currentType == Compiladores.Checker.Type.Int || currentType == Compiladores.Checker.Type.Double)
                {
                    _ilGenerator.Emit(OpCodes.Neg);
                }
            }

            for (int i = 0; i < context.addop().Length; i++)
            {
                Visit(context.term(i + 1)); 
                Compiladores.Checker.Type rightTermType = GetExpressionType(context.term(i + 1));
                                             
                if (currentType == Compiladores.Checker.Type.Error || rightTermType == Compiladores.Checker.Type.Error)
                {
                    if(currentType != Compiladores.Checker.Type.Error && rightTermType == Compiladores.Checker.Type.Error && currentType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    if(currentType == Compiladores.Checker.Type.Error && rightTermType != Compiladores.Checker.Type.Error && rightTermType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    currentType = Compiladores.Checker.Type.Error;
                    continue; 
                }
                
                string op = context.addop(i).GetText();
                OpCode? opCodeToEmit = null;
                
                if ((currentType == Compiladores.Checker.Type.Int || currentType == Compiladores.Checker.Type.Double) &&
                    (rightTermType == Compiladores.Checker.Type.Int || rightTermType == Compiladores.Checker.Type.Double))
                {
                    if (currentType == Compiladores.Checker.Type.Double || rightTermType == Compiladores.Checker.Type.Double)
                    {
                        if (rightTermType == Compiladores.Checker.Type.Int)
                        {
                            _ilGenerator.Emit(OpCodes.Conv_R8);
                        }
                        else if (currentType == Compiladores.Checker.Type.Int)
                        {
                            LocalBuilder tempRightDouble = _ilGenerator.DeclareLocal(typeof(double));
                            _ilGenerator.Emit(OpCodes.Stloc, tempRightDouble); 
                            _ilGenerator.Emit(OpCodes.Conv_R8);              
                            _ilGenerator.Emit(OpCodes.Ldloc, tempRightDouble); 
                        }
                        currentType = Compiladores.Checker.Type.Double;
                    }
                    else 
                    {
                        currentType = Compiladores.Checker.Type.Int;
                    }
                    opCodeToEmit = (op == "+") ? OpCodes.Add : OpCodes.Sub;
                }
                
                if (opCodeToEmit.HasValue)
                {
                    _ilGenerator.Emit(opCodeToEmit.Value);
                }
            }
            return null; 
        }
        
        private void EmitConversion(Compiladores.Checker.Type source, Compiladores.Checker.Type target)
        {
            if (source == target) return;

            if (target == Compiladores.Checker.Type.Double)
            {
                if (source == Compiladores.Checker.Type.Int || source == Compiladores.Checker.Type.Char)
                {
                    _ilGenerator.Emit(OpCodes.Conv_R8);
                }
            }
            else if (target == Compiladores.Checker.Type.Int)
            {
                if (source == Compiladores.Checker.Type.Double)
                {
                    _ilGenerator.Emit(OpCodes.Conv_I4);
                }
            }
            else if (target == Compiladores.Checker.Type.Char)
            {
                _ilGenerator.Emit(OpCodes.Conv_U2);
            }
        }
        
        private void EmitToStringCoercion(Compiladores.Checker.Type typeToConvert, bool isAddressExpected) {
            System.Type netType = ResolveNetType(typeToConvert);
            MethodInfo toStringMethod = null;

            if (netType.IsValueType) {
                toStringMethod = netType.GetMethod("ToString", System.Type.EmptyTypes);

                if (toStringMethod != null && toStringMethod.DeclaringType == netType && !toStringMethod.IsVirtual) {
                    LocalBuilder tempVal = _ilGenerator.DeclareLocal(netType);
                    _ilGenerator.Emit(OpCodes.Stloc, tempVal);
                    _ilGenerator.Emit(OpCodes.Ldloca_S, tempVal); 
                    _ilGenerator.Emit(OpCodes.Call, toStringMethod); 
                } else {
                    _ilGenerator.Emit(OpCodes.Box, netType);
                    toStringMethod = typeof(object).GetMethod("ToString");
                    _ilGenerator.Emit(OpCodes.Callvirt, toStringMethod);
                }
            } else if (netType == typeof(string)) {
            } else { 
                toStringMethod = typeof(object).GetMethod("ToString"); 
                _ilGenerator.Emit(OpCodes.Callvirt, toStringMethod);
            }
        }
        
        public override object VisitTerm(MiniCSharpParser.TermContext context)
        {
            if (_ilGenerator == null) 
            { 
                return null; 
            }

            Visit(context.factor(0)); 
            Compiladores.Checker.Type currentLeftType = GetExpressionType(context.factor(0));

            for (int i = 0; i < context.mulop().Length; i++)
            {
                Visit(context.factor(i + 1)); 
                Compiladores.Checker.Type rightFactorType = GetExpressionType(context.factor(i + 1));
                                             
                if (currentLeftType == Compiladores.Checker.Type.Error || rightFactorType == Compiladores.Checker.Type.Error)
                {
                    if(currentLeftType != Compiladores.Checker.Type.Error && rightFactorType == Compiladores.Checker.Type.Error && currentLeftType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    if(currentLeftType == Compiladores.Checker.Type.Error && rightFactorType != Compiladores.Checker.Type.Error && rightFactorType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    currentLeftType = Compiladores.Checker.Type.Error;
                    continue;
                }

                string op = context.mulop(i).GetText();
                OpCode? opCodeToEmit = null;
                Compiladores.Checker.Type targetType = Compiladores.Checker.Type.Error;

                if ((currentLeftType == Compiladores.Checker.Type.Int || currentLeftType == Compiladores.Checker.Type.Double) &&
                    (rightFactorType == Compiladores.Checker.Type.Int || rightFactorType == Compiladores.Checker.Type.Double))
                {
                    if (currentLeftType == Compiladores.Checker.Type.Double || rightFactorType == Compiladores.Checker.Type.Double)
                    {
                        targetType = Compiladores.Checker.Type.Double;
                        if (rightFactorType == Compiladores.Checker.Type.Int) 
                        {
                            _ilGenerator.Emit(OpCodes.Conv_R8); 
                        }
                        else if (currentLeftType == Compiladores.Checker.Type.Int && rightFactorType == Compiladores.Checker.Type.Double) 
                        {
                            LocalBuilder tempRightDouble = _ilGenerator.DeclareLocal(typeof(double));
                            _ilGenerator.Emit(OpCodes.Stloc, tempRightDouble); 
                            _ilGenerator.Emit(OpCodes.Conv_R8);              
                            _ilGenerator.Emit(OpCodes.Ldloc, tempRightDouble); 
                        }
                    }
                    else 
                    {
                        targetType = Compiladores.Checker.Type.Int;
                    }

                    if (op == "*") opCodeToEmit = OpCodes.Mul;
                    else if (op == "/") opCodeToEmit = OpCodes.Div;
                    else if (op == "%")
                    {
                        if (targetType == Compiladores.Checker.Type.Double)
                        {
                             targetType = Compiladores.Checker.Type.Int; 
                        }
                        opCodeToEmit = OpCodes.Rem;
                    }
                }
                else
                {
                     if (currentLeftType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                     if (rightFactorType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    targetType = Compiladores.Checker.Type.Error;
                }
                
                if (opCodeToEmit.HasValue)
                {
                    _ilGenerator.Emit(opCodeToEmit.Value);
                }
                currentLeftType = targetType;
            }
            return null;
        }
        
        public override object VisitCondition(MiniCSharpParser.ConditionContext context)
        {
            if (_ilGenerator == null)
            {
                return null;
            }
            Visit(context.condTerm(0)); 
            for (int i = 0; i < context.OR().Length; i++)
            {
                Visit(context.condTerm(i + 1)); 
                _ilGenerator.Emit(OpCodes.Or);  
            }
            return null;
        }

        public override object VisitCondTerm(MiniCSharpParser.CondTermContext context)
        {
            if (_ilGenerator == null)
            {
                return null;
            }
            Visit(context.condFact(0)); 
            for (int i = 0; i < context.AND().Length; i++)
            {
                Visit(context.condFact(i + 1)); 
                _ilGenerator.Emit(OpCodes.And); 
            }
            return null;
        }

        public override object VisitCondFact(MiniCSharpParser.CondFactContext context)
        {
            if (_ilGenerator == null)
            {
                return null;
            }

            Visit(context.expr(0));
            Compiladores.Checker.Type leftType = GetExpressionType(context.expr(0));

            if (context.relop() == null)
            {
                if (leftType != Compiladores.Checker.Type.Bool)
                {
                    _ilGenerator.Emit(OpCodes.Ldc_I4_0); 
                    _ilGenerator.Emit(OpCodes.Ceq);      
                    _ilGenerator.Emit(OpCodes.Ldc_I4_0); 
                    _ilGenerator.Emit(OpCodes.Ceq);      
                }
                return null; 
            }

            Visit(context.expr(1));
            Compiladores.Checker.Type rightType = GetExpressionType(context.expr(1));
            
            System.Type netLeftType = ResolveNetType(leftType);
            System.Type netRightType = ResolveNetType(rightType);

            if (netLeftType == typeof(int) && netRightType == typeof(double))
            {
                LocalBuilder tempRightDouble = _ilGenerator.DeclareLocal(typeof(double));
                _ilGenerator.Emit(OpCodes.Stloc, tempRightDouble); 
                _ilGenerator.Emit(OpCodes.Conv_R8);              
                _ilGenerator.Emit(OpCodes.Ldloc, tempRightDouble); 
            }
            else if (netLeftType == typeof(double) && netRightType == typeof(int))
            {
                _ilGenerator.Emit(OpCodes.Conv_R8); 
            }
            
            string op = context.relop().GetText();
            switch (op)
            {
                case "==": _ilGenerator.Emit(OpCodes.Ceq); break;
                case "!=": _ilGenerator.Emit(OpCodes.Ceq); _ilGenerator.Emit(OpCodes.Ldc_I4_0); _ilGenerator.Emit(OpCodes.Ceq); break;
                case "<": _ilGenerator.Emit(OpCodes.Clt); break;
                case "<=": _ilGenerator.Emit(OpCodes.Cgt); _ilGenerator.Emit(OpCodes.Ldc_I4_0); _ilGenerator.Emit(OpCodes.Ceq); break; 
                case ">": _ilGenerator.Emit(OpCodes.Cgt); break;
                case ">=": _ilGenerator.Emit(OpCodes.Clt); _ilGenerator.Emit(OpCodes.Ldc_I4_0); _ilGenerator.Emit(OpCodes.Ceq); break; 
                default:
                    if (leftType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    if (rightType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    _ilGenerator.Emit(OpCodes.Ldc_I4_0); 
                    break;
            }
            return null;
        }
        
        public override object VisitIfStatement(MiniCSharpParser.IfStatementContext context)
        {
            if (_ilGenerator == null)
            {
                return null;
            }

            Visit(context.condition());

            if (context.ELSE() == null) 
            {
                System.Reflection.Emit.Label endIfLabel = _ilGenerator.DefineLabel(); 
                _ilGenerator.Emit(OpCodes.Brfalse_S, endIfLabel); 
                Visit(context.statement(0));
                _ilGenerator.MarkLabel(endIfLabel);
            }
            else 
            {
                System.Reflection.Emit.Label elseLabel = _ilGenerator.DefineLabel();    
                System.Reflection.Emit.Label endIfLabel = _ilGenerator.DefineLabel();   
                _ilGenerator.Emit(OpCodes.Brfalse_S, elseLabel); 
                Visit(context.statement(0));
                _ilGenerator.Emit(OpCodes.Br_S, endIfLabel); 
                _ilGenerator.MarkLabel(elseLabel);
                Visit(context.statement(1));
                _ilGenerator.MarkLabel(endIfLabel);
            }
            return null;
        }

        public override object VisitWhileStatement(MiniCSharpParser.WhileStatementContext context)
        {
            if (_ilGenerator == null) { return null; }

            System.Reflection.Emit.Label conditionLabel = _ilGenerator.DefineLabel();
            System.Reflection.Emit.Label endWhileLabel = _ilGenerator.DefineLabel();

            _breakLabelStack.Push(endWhileLabel); 

            _ilGenerator.MarkLabel(conditionLabel);
            Visit(context.condition()); 
            _ilGenerator.Emit(OpCodes.Brfalse, endWhileLabel);  
    
            Visit(context.statement()); 
    
            _ilGenerator.Emit(OpCodes.Br, conditionLabel);  

            _ilGenerator.MarkLabel(endWhileLabel);
            _breakLabelStack.Pop(); 

            return null;
        }
        
        public override object VisitNewFactor(MiniCSharpParser.NewFactorContext context)
        {
            if (_ilGenerator == null)
            {
                return null;
            }
            
            Compiladores.Checker.Type newMiniCSharpType = GetExpressionType(context);

            if (newMiniCSharpType == Compiladores.Checker.Type.Error)
            {
                _ilGenerator.Emit(OpCodes.Ldnull);
                return null;
            }
            ExpressionTypes[context] = newMiniCSharpType;

            if (newMiniCSharpType.Kind == TypeKind.Array) 
            {
                ArrayType arrayMiniCSharpType = (ArrayType)newMiniCSharpType; 
                
                Visit(context.expr()); 
                                       
                System.Type netElementType = ResolveNetType(arrayMiniCSharpType.ElementType);
                if (netElementType == null || (netElementType == typeof(object) && arrayMiniCSharpType.ElementType != Compiladores.Checker.Type.Null && arrayMiniCSharpType.ElementType != Compiladores.Checker.Type.Error) )
                {
                     _ilGenerator.Emit(OpCodes.Pop);
                     _ilGenerator.Emit(OpCodes.Ldnull); 
                     return null;
                }
                
                _ilGenerator.Emit(OpCodes.Newarr, netElementType);
            }
            else if (newMiniCSharpType.Kind == TypeKind.Class) 
            {
                System.Type netClassType = ResolveNetType(newMiniCSharpType);

                if (netClassType == null || (netClassType == typeof(object) && newMiniCSharpType != Compiladores.Checker.Type.Null && newMiniCSharpType != Compiladores.Checker.Type.Error ))
                {
                     _ilGenerator.Emit(OpCodes.Ldnull); 
                     return null;
                }
                
                ConstructorInfo constructor = netClassType.GetConstructor(System.Type.EmptyTypes);
                
                if (constructor == null && netClassType is TypeBuilder)
                {
                    constructor = (netClassType as TypeBuilder).DefineDefaultConstructor(MethodAttributes.Public);
                }
                
                if (constructor == null)
                {
                     _ilGenerator.Emit(OpCodes.Ldnull);
                     return null;
                }
                
                _ilGenerator.Emit(OpCodes.Newobj, constructor);
            }
            else
            {
                _ilGenerator.Emit(OpCodes.Ldnull); 
            }
            return null;
        }
        
        public override object VisitUsingDirective(MiniCSharpParser.UsingDirectiveContext context) { return base.VisitUsingDirective(context); }
        public override object VisitQualifiedIdent(MiniCSharpParser.QualifiedIdentContext context) { return base.VisitQualifiedIdent(context); }
        public override object VisitFormPars(MiniCSharpParser.FormParsContext context) { return base.VisitFormPars(context); }
        
        public override object VisitForStatement(MiniCSharpParser.ForStatementContext context)
        {
            if (_ilGenerator == null) { return null; }

            System.Reflection.Emit.Label conditionLabel = _ilGenerator.DefineLabel();
            System.Reflection.Emit.Label incrementLabel = _ilGenerator.DefineLabel(); 
            System.Reflection.Emit.Label bodyLabel = _ilGenerator.DefineLabel();
            System.Reflection.Emit.Label endForLabel = _ilGenerator.DefineLabel();

            _breakLabelStack.Push(endForLabel); 

            var initExprCtx = context.expr();
            var conditionCtx = context.condition();
            
            MiniCSharpParser.StatementContext iterStatementCtx = null;
            MiniCSharpParser.StatementContext bodyStatementCtx = null;

            var statementsInRule = context.statement();
            if (statementsInRule.Length == 1) { 
                bodyStatementCtx = statementsInRule[0];
            } else if (statementsInRule.Length == 2) { 
                iterStatementCtx = statementsInRule[0]; 
                bodyStatementCtx = statementsInRule[1]; 
            } else if (statementsInRule.Length == 0 && context.children.LastOrDefault(c => c is MiniCSharpParser.StatementContext) is MiniCSharpParser.StatementContext lastChild) {
                bodyStatementCtx = lastChild; 
            }
             else if (statementsInRule.Length == 0 ) {
                 _breakLabelStack.Pop();
                 return null;
            }

            if (initExprCtx != null)
            {
                Visit(initExprCtx);
                Compiladores.Checker.Type initType = GetExpressionType(initExprCtx); 
                if (initType != Compiladores.Checker.Type.Void && initType != Compiladores.Checker.Type.Error)
                {
                    _ilGenerator.Emit(OpCodes.Pop); 
                }
            }

            _ilGenerator.Emit(OpCodes.Br, conditionLabel); 

            _ilGenerator.MarkLabel(bodyLabel);
            if (bodyStatementCtx != null)
            {
                Visit(bodyStatementCtx);
            }

            _ilGenerator.MarkLabel(incrementLabel);
            if (iterStatementCtx != null)
            {
                Visit(iterStatementCtx); 
            }

            _ilGenerator.MarkLabel(conditionLabel);
            if (conditionCtx != null)
            {
                Visit(conditionCtx); 
                _ilGenerator.Emit(OpCodes.Brfalse, endForLabel); 
            }
            _ilGenerator.Emit(OpCodes.Br, bodyLabel);

            _ilGenerator.MarkLabel(endForLabel);
            _breakLabelStack.Pop();

            return null;
        }
        public override object VisitSwitchStatement(MiniCSharpParser.SwitchStatementContext context)
        {
            if (_ilGenerator == null) { return null; }

            System.Reflection.Emit.Label endSwitchLabel = _ilGenerator.DefineLabel();
            _breakLabelStack.Push(endSwitchLabel);

            Visit(context.expr()); 
            Compiladores.Checker.Type switchExprType = GetExpressionType(context.expr()); 
            System.Type netSwitchExprType = ResolveNetType(switchExprType); 

            LocalBuilder switchValueLocal = _ilGenerator.DeclareLocal(netSwitchExprType);
            _ilGenerator.Emit(OpCodes.Stloc, switchValueLocal);

            var caseContexts = context.switchCase();
            System.Reflection.Emit.Label[] caseTargetLabels = new System.Reflection.Emit.Label[caseContexts.Length];
            for (int i = 0; i < caseContexts.Length; i++)
            {
                caseTargetLabels[i] = _ilGenerator.DefineLabel();
            }

            System.Reflection.Emit.Label defaultTargetLabel = _ilGenerator.DefineLabel(); 

            for (int i = 0; i < caseContexts.Length; i++)
            {
                _ilGenerator.Emit(OpCodes.Ldloc, switchValueLocal); 
                Visit(caseContexts[i].constant()); 
                _ilGenerator.Emit(OpCodes.Beq, caseTargetLabels[i]); 
            }

            if (context.defaultCase() != null)
            {
                _ilGenerator.Emit(OpCodes.Br, defaultTargetLabel);
            }
            else
            {
                _ilGenerator.Emit(OpCodes.Br, endSwitchLabel);
            }

            for (int i = 0; i < caseContexts.Length; i++)
            {
                _ilGenerator.MarkLabel(caseTargetLabels[i]);
                Visit(caseContexts[i]); 
            }

            _ilGenerator.MarkLabel(defaultTargetLabel);
            if (context.defaultCase() != null)
            {
                Visit(context.defaultCase()); 
            }
            _ilGenerator.MarkLabel(endSwitchLabel);
            _breakLabelStack.Pop();
            return null;
        }
        public override object VisitBreakStatement(MiniCSharpParser.BreakStatementContext context)
        {
            if (_ilGenerator == null) { return null; }

            if (_breakLabelStack.Count > 0)
            {
                _ilGenerator.Emit(OpCodes.Br, _breakLabelStack.Peek());
            }
            return null;
        }

        public override object VisitReadStatement(MiniCSharpParser.ReadStatementContext context)
{
    if (_ilGenerator == null) { Console.Error.WriteLine("CodeGen Error (VisitReadStatement): ILGenerator is null."); return null; }

    var designatorCtx = context.designator();
    var designatorType = GetExpressionType(designatorCtx);

    // *** INICIO DE LA CORRECCIÓN ***
    // Preparamos la pila para el almacenamiento, igual que en una asignación.
    if (designatorCtx.DOT().Length > 0)
    {
        // Es un campo (ej. report.rawScore). Cargamos la referencia a 'report'.
        string objectName = designatorCtx.IDENT(0).GetText();
        Symbol objectSymbol = _currentCodeGenScope.Find(objectName) ?? _symbolTable.SearchGlobal(objectName);
        if (objectSymbol is VarSymbol varObj)
        {
            EmitLoadVariable(varObj); // Este método pone la referencia al objeto en la pila.
        }
    }
    else if (designatorCtx.LBRACK().Length > 0)
    {
        // Es un array (ej. miArray[i]). Cargamos la referencia al array y el índice.
        string baseArrayName = designatorCtx.IDENT(0).GetText();
        Symbol baseArraySymbol = _currentCodeGenScope.Find(baseArrayName) ?? _symbolTable.SearchGlobal(baseArrayName);
        if (baseArraySymbol is VarSymbol varSym)
        {
            EmitLoadArrayAndIndex(varSym, designatorCtx.expr(0));
        }
        else
        {
            return null;
        }
    }
    // *** FIN DE LA CORRECCIÓN ***

    // Ahora, con la pila preparada, leemos y parseamos el valor desde la consola.
    MethodInfo readLineMethod = typeof(Console).GetMethod("ReadLine", System.Type.EmptyTypes);
    _ilGenerator.Emit(OpCodes.Call, readLineMethod);

    LocalBuilder readString = _ilGenerator.DeclareLocal(typeof(string));
    _ilGenerator.Emit(OpCodes.Stloc, readString);
    
    System.Reflection.Emit.Label parseLabel = _ilGenerator.DefineLabel();
    System.Reflection.Emit.Label endReadLabel = _ilGenerator.DefineLabel();

    _ilGenerator.Emit(OpCodes.Ldloc, readString);
    _ilGenerator.Emit(OpCodes.Brtrue_S, parseLabel);

    if (designatorType == Compiladores.Checker.Type.Int) _ilGenerator.Emit(OpCodes.Ldc_I4_0);
    else if (designatorType == Compiladores.Checker.Type.Double) _ilGenerator.Emit(OpCodes.Ldc_R8, 0.0);
    else if (designatorType == Compiladores.Checker.Type.Char) _ilGenerator.Emit(OpCodes.Ldc_I4_0);
    _ilGenerator.Emit(OpCodes.Br_S, endReadLabel);

    _ilGenerator.MarkLabel(parseLabel);
    _ilGenerator.Emit(OpCodes.Ldloc, readString);

    if (designatorType == Compiladores.Checker.Type.Int)
    {
        MethodInfo parseIntMethod = typeof(int).GetMethod("Parse", new[] { typeof(string) });
        _ilGenerator.Emit(OpCodes.Call, parseIntMethod);
    }
    else if (designatorType == Compiladores.Checker.Type.Double)
    {
        MethodInfo parseDoubleMethod = typeof(double).GetMethod("Parse", new[] { typeof(string), typeof(IFormatProvider) });
        var cultureInfo = typeof(CultureInfo).GetProperty("InvariantCulture").GetGetMethod();
        _ilGenerator.Emit(OpCodes.Call, cultureInfo);
        _ilGenerator.Emit(OpCodes.Call, parseDoubleMethod);
    }
    else if (designatorType == Compiladores.Checker.Type.Char)
    {
        // Considerar que leer un solo caracter puede ser más complejo.
        // Esta implementación toma el primer caracter de la línea ingresada.
        _ilGenerator.Emit(OpCodes.Ldc_I4_0);
        MethodInfo charAtMethod = typeof(string).GetMethod("get_Chars", new[] { typeof(int) });
        _ilGenerator.Emit(OpCodes.Callvirt, charAtMethod);
    }
    
    _ilGenerator.MarkLabel(endReadLabel);
    
    // La pila ahora está correcta: [obj_ref, value] o [arr_ref, index, value] o solo [value].
    // EmitStoreToDesignator ahora funcionará.
    EmitStoreToDesignator(designatorCtx, designatorType);

    return null;
}
        public override object VisitBlockStatement(MiniCSharpParser.BlockStatementContext context) { return Visit(context.block());} 
        public override object VisitEmptyStatement(MiniCSharpParser.EmptyStatementContext context) { return base.VisitEmptyStatement(context); }
        public override object VisitSwitchCase(MiniCSharpParser.SwitchCaseContext context)
        {
            foreach (var stmtCtx in context.statement())
            {
                Visit(stmtCtx);
            }
            return null;
        }
        public override object VisitDefaultCase(MiniCSharpParser.DefaultCaseContext context)
        {
            foreach (var stmtCtx in context.statement())
            {
                Visit(stmtCtx);
            }
            return null;
        }
        public override object VisitConstant(MiniCSharpParser.ConstantContext context) {
             if (context.number() != null) Visit(context.number());
             else if (context.CHARCONST() != null) _ilGenerator.Emit(OpCodes.Ldc_I4, (int)context.CHARCONST().GetText()[1]);
            return null;
        }
        public override object VisitActPars(MiniCSharpParser.ActParsContext context) { 
            return base.VisitActPars(context); 
        }
        
        public override object VisitCast(MiniCSharpParser.CastContext context) {
            return GetExpressionType(context.type());
        }
        
        public override object VisitParenFactor(MiniCSharpParser.ParenFactorContext context) 
        { 
            Visit(context.expr());
            ExpressionTypes[context] = GetExpressionType(context.expr());
            return null;
        } 
        
        public override object VisitDesignator(MiniCSharpParser.DesignatorContext context) 
        { 
            GetExpressionType(context);
            return base.VisitDesignator(context); 
        } 
        
        public override object VisitNumber(MiniCSharpParser.NumberContext context) { 
            if (context.INTCONST() != null) _ilGenerator.Emit(OpCodes.Ldc_I4, int.Parse(context.INTCONST().GetText()));
            else if (context.DOUBLECONST() != null) _ilGenerator.Emit(OpCodes.Ldc_R8, double.Parse(context.DOUBLECONST().GetText()));
            ExpressionTypes[context] = context.INTCONST() != null ? Compiladores.Checker.Type.Int : Compiladores.Checker.Type.Double;
            return null;
        }
        
        public override object VisitRelop(MiniCSharpParser.RelopContext context) { return base.VisitRelop(context); }
        public override object VisitAddop(MiniCSharpParser.AddopContext context) { return base.VisitAddop(context); }
        public override object VisitMulop(MiniCSharpParser.MulopContext context) { return base.VisitMulop(context); }
    }
}