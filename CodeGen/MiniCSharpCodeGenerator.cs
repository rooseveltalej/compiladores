// Archivo: compiladores/CodeGen/MiniCSharpCodeGenerator.cs
using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime.Tree;
using Compiladores.Checker;
// Asegúrate que el namespace de MiniCSharpRuntimeHelpers sea correcto aquí
// Si MiniCSharpRuntimeHelpers.cs está en namespace Compiladores.Runtime:
using Compiladores; 
// Si MiniCSharpRuntimeHelpers.cs está directamente en namespace Compiladores:
// using Compiladores; // (Ya estaría cubierto por el using Compiladores.Checker;)

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

        public MiniCSharpCodeGenerator(TablaSimbolos symbolTable, string sourceFilePath, string assemblyName, Dictionary<IParseTree, Compiladores.Checker.Type> expressionTypes)
        {
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
                    Console.WriteLine($"Finalizing type: {tbEntry.FullName}");
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
            if (miniCSharpType == null) throw new ArgumentNullException(nameof(miniCSharpType), "El tipo MiniCSharp no puede ser null.");
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
                         Console.WriteLine($"Warning (ResolveNetType for Array Element): Type '{elementTb.FullName}' for array element was not yet created. Forcing creation.");
                         elementTb.CreateTypeInfo(); 
                         elementType = elementTb.AsType(); 
                    }
                    return elementType.MakeArrayType();
                case TypeKind.Class: 
                    Symbol classDefSymbol = _symbolTable.SearchGlobal(miniCSharpType.Name); 
                    if (classDefSymbol == null && _currentCodeGenScope != null) { 
                        classDefSymbol = _currentCodeGenScope.Find(miniCSharpType.Name);
                    }

                    if (classDefSymbol is ClassSymbol userClassSymbol && _definedTypes.TryGetValue(userClassSymbol, out TypeBuilder tb))
                    {
                        if (!tb.IsCreated())
                        {
                            Console.WriteLine($"Warning (ResolveNetType): Type '{tb.FullName}' for user class was not yet created. Forcing creation.");
                            tb.CreateTypeInfo(); 
                        }
                        return tb.AsType(); 
                    }
                    
                    if (miniCSharpType.Name == "Console") return typeof(System.Console);
                    // Asegúrate que el namespace aquí coincida con MiniCSharpRuntimeHelpers.cs
                    if (miniCSharpType.Name == "MiniCSharpRuntimeHelpers") return typeof(Compiladores.MiniCSharpRuntimeHelpers); 
                    
                    Console.WriteLine($"Advertencia (ResolveNetType): No se pudo resolver el tipo .NET para la clase '{miniCSharpType.Name}'. Usando 'object'.");
                    return typeof(object); 
                default:
                    Console.Error.WriteLine($"Error Crítico (ResolveNetType): Tipo MiniCSharp no soportado para conversión: {miniCSharpType.Kind} ('{miniCSharpType.Name}')");
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
            
            // CreateType es llamado en GenerateAssemblyAndGetMainType después que todos los tipos son definidos.
            // No es necesario llamarlo aquí si se hace globalmente al final.
            // var createdType = _currentTypeBuilder.CreateType(); 
            // if (createdType == null) { Console.Error.WriteLine($"CodeGen Error: Falló la creación del tipo para la clase principal '{className}'."); }
            return null; 
        }
        
        public override object VisitClassDecl(MiniCSharpParser.ClassDeclContext context)
        {
            string className = context.IDENT().GetText(); 
            ClassSymbol classSymbol = _currentCodeGenScope.FindCurrent(className) as ClassSymbol; 
            if (classSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No ClassSymbol para clase anidada '{className}' en scope '{_currentTypeBuilder?.Name}'."); return null; } 
            
            TypeBuilder outerTypeBuilder = _currentTypeBuilder; 
            _currentTypeBuilder = outerTypeBuilder.DefineNestedType(className, TypeAttributes.NestedPublic | TypeAttributes.Class | TypeAttributes.AutoLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit | TypeAttributes.Sealed, typeof(object)); 
            if (!_definedTypes.TryAdd(classSymbol, _currentTypeBuilder)) { _definedTypes[classSymbol] = _currentTypeBuilder; } 
            
            Scope outerScope = _currentCodeGenScope; 
            _currentCodeGenScope = (classSymbol.Type as ClassType)?.Members; 
            if (_currentCodeGenScope == null) { Console.Error.WriteLine($"CodeGen Error: ClassSymbol anidado '{className}' no tiene Members."); _currentTypeBuilder = outerTypeBuilder; return null; } 
            
            foreach (var varDeclContext in context.varDecl()) Visit(varDeclContext); 

            _currentCodeGenScope = outerScope; 
            // CreateType es llamado en GenerateAssemblyAndGetMainType
            // var createdNestedType = _currentTypeBuilder.CreateType(); 
            // if (createdNestedType == null) { Console.Error.WriteLine($"CodeGen Error: Falló la creación del tipo para la clase anidada '{className}'.");  }
            _currentTypeBuilder = outerTypeBuilder; 
            return null; 
        }

        public override object VisitVarDecl(MiniCSharpParser.VarDeclContext context)
        {
            Compiladores.Checker.Type varMiniCSharpType = (Compiladores.Checker.Type)Visit(context.type()); 
            if (varMiniCSharpType == Compiladores.Checker.Type.Error) 
            {
                Console.Error.WriteLine($"CodeGen Error (VisitVarDecl): No se pudo resolver el tipo para la variable que comienza con '{context.IDENT(0).GetText()}'."); 
                return null; 
            }
            System.Type varNetType = ResolveNetType(varMiniCSharpType); 

            foreach (var identNode in context.IDENT()) 
            {
                string varName = identNode.GetText(); 
                VarSymbol varSymbol = _currentCodeGenScope.FindCurrent(varName) as VarSymbol; 

                if (varSymbol == null) 
                {
                    Console.Error.WriteLine($"CodeGen Error (VisitVarDecl): No se encontró VarSymbol para '{varName}' en el scope actual (HashCode: {_currentCodeGenScope?.GetHashCode()})."); 
                    if(_currentCodeGenScope != null) { 
                        Console.Error.WriteLine($"Símbolos presentes en este scope del CodeGen ({_currentCodeGenScope.GetHashCode()}): " + string.Join(", ", _currentCodeGenScope.Symbols.Keys)); 
                    }
                    continue; 
                }

                if (_currentMethodBuilder != null) 
                {
                    if (_ilGenerator == null) { Console.Error.WriteLine($"Error Crítico (VisitVarDecl): ILGenerator es null al declarar variable local '{varName}'."); return null; }
                    LocalBuilder lb = _ilGenerator.DeclareLocal(varNetType); 
                    _localBuilders[varSymbol] = lb; 
                    // Console.WriteLine($"CodeGen INFO (VisitVarDecl): Declarado LocalBuilder para '{varName}' (VarSymbol Hash: {varSymbol.GetHashCode()}, LocalBuilder Index: {lb.LocalIndex}, Scope Hash: {_currentCodeGenScope.GetHashCode()})"); 
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
                    // Console.WriteLine($"CodeGen INFO (VisitVarDecl): Definido FieldBuilder para '{varName}' (Estático: {fb.IsStatic})"); 
                }
            }
            return null; 
        }
        
        public override object VisitType(MiniCSharpParser.TypeContext context)
        {
            string typeName = context.IDENT().GetText();
            Symbol typeDefiningSymbol = _symbolTable.Search(typeName);
            if (typeDefiningSymbol == null || !(typeDefiningSymbol is TypeDefSymbol || typeDefiningSymbol is ClassSymbol))
            { Console.Error.WriteLine($"CodeGen Error: Tipo '{typeName}' no encontrado. Scope: {_currentCodeGenScope?.GetHashCode()}"); return Compiladores.Checker.Type.Error; }
            
            Compiladores.Checker.Type baseType = typeDefiningSymbol.Type;
            if (context.LBRACK() != null && context.RBRACK() != null) { return new ArrayType(baseType); } 
            return baseType;
        }

        public override object VisitMethodDecl(MiniCSharpParser.MethodDeclContext context)
        {
            string methodName = context.IDENT().GetText();
            MethodSymbol methodSymbol = _currentCodeGenScope.FindCurrent(methodName) as MethodSymbol;

            if (methodSymbol == null) {
                Console.Error.WriteLine($"CodeGen Error: No se encontró MethodSymbol para '{methodName}' en el scope de la clase '{_currentTypeBuilder?.Name}'. Scope Hash: {_currentCodeGenScope?.GetHashCode()}");
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
                Console.Error.WriteLine($"CodeGen CRITICAL Error: No se encontró el scope del checker para el método '{methodName}'. Context Node Text: {context.GetText().Substring(0, Math.Min(30, context.GetText().Length))}");
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
            if (blockCheckerScope == null) {
                Console.Error.WriteLine($"CodeGen Error: No se encontró el scope del checker para el bloque: {context.GetText().Substring(0,Math.Min(20, context.GetText().Length))}");
            } else {
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
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitNumberFactor."); return null; }
            if (context.number().INTCONST() != null)
            { _ilGenerator.Emit(OpCodes.Ldc_I4, int.Parse(context.number().INTCONST().GetText())); }
            else if (context.number().DOUBLECONST() != null)
            { _ilGenerator.Emit(OpCodes.Ldc_R8, double.Parse(context.number().DOUBLECONST().GetText())); }
            ExpressionTypes[context] = context.number().INTCONST() != null ? Compiladores.Checker.Type.Int : Compiladores.Checker.Type.Double;
            return null;
        }
        public override object VisitCharFactor(MiniCSharpParser.CharFactorContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitCharFactor."); return null; }
            string text = context.CHARCONST().GetText(); char val = text.Length >= 3 ? text[1] : '\0';
            _ilGenerator.Emit(OpCodes.Ldc_I4, (int)val); 
            ExpressionTypes[context] = Compiladores.Checker.Type.Char;
            return null;
        }
        public override object VisitStringFactor(MiniCSharpParser.StringFactorContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitStringFactor."); return null; }
            string text = context.STRINGCONST().GetText(); string val = text.Substring(1, text.Length - 2);
            _ilGenerator.Emit(OpCodes.Ldstr, val); 
            ExpressionTypes[context] = Compiladores.Checker.Type.String;
            return null;
        }
        public override object VisitBoolFactor(MiniCSharpParser.BoolFactorContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitBoolFactor."); return null; }
            _ilGenerator.Emit(context.TRUE() != null ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0); 
            ExpressionTypes[context] = Compiladores.Checker.Type.Bool;
            return null;
        }
        
        // ***** MÉTODO CORREGIDO: VisitWriteStatement *****
        public override object VisitWriteStatement(MiniCSharpParser.WriteStatementContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitWriteStatement."); return null; }
            
            var exprNode = context.expr();
            Visit(exprNode); // Deja el valor de la expresión en la pila

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
                Console.Error.WriteLine($"CodeGen Critical Error: No se encontró método Console.WriteLine compatible para '{exprMiniCSharpType}' (NET: {netTypeForResolvedExpr}).");
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
                Console.Error.WriteLine("CodeGen Error (GetExpressionType): Attempted to get type for a null tree node.");
                return Compiladores.Checker.Type.Error;
            }

            if (ExpressionTypes.TryGetValue(treeNode, out Compiladores.Checker.Type type))
            {
                if (type == null)
                {
                    Console.Error.WriteLine($"CodeGen Warning (GetExpressionType): Null type found in ExpressionTypes for node '{treeNode.GetText()}'. Defaulting to Type.Error.");
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
            // IMPORTANTE: La lógica para DesignatorFactor y Designator debe estar en sus respectivos Visit*
            // y almacenar el tipo en ExpressionTypes. Este GetExpressionType es principalmente un lector del diccionario.
            // Los fallbacks aquí son menos ideales.
            if (treeNode is MiniCSharpParser.DesignatorFactorContext dfCtx) 
            {
                 if (dfCtx.LPAREN() == null) // Not a method call
                    return GetExpressionType(dfCtx.designator());
                 // else, el tipo debería haber sido almacenado por VisitDesignatorFactor para la llamada al método
            }
             if (treeNode is MiniCSharpParser.DesignatorContext desCtx)
            {
                // Este fallback para DesignatorContext puede ser problemático si el checker no almacenó
                // el tipo preciso (ej. tipo de elemento para array[idx] vs tipo de array para 'array')
                string baseName = desCtx.IDENT(0).GetText();
                Symbol sym = _currentCodeGenScope?.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
                if (sym != null && sym.Type != null)
                {
                    // No almacenar aquí, este es un GET. El almacenamiento debe ser en los Visit* del checker y codegen.
                    // ExpressionTypes[treeNode] = sym.Type; 
                    return sym.Type;
                }
            }


            Console.Error.WriteLine($"CodeGen Error (GetExpressionType): Type not found in ExpressionTypes and could not be inferred for node '{treeNode.GetText()}' (Type: {treeNode.GetType().Name}). Defaulting to Type.Error.");
            // No almacenar Type.Error aquí de forma predeterminada, podría ocultar problemas del checker.
            // ExpressionTypes[treeNode] = Compiladores.Checker.Type.Error;
            return Compiladores.Checker.Type.Error;
        }

        public override object VisitReturnStatement(MiniCSharpParser.ReturnStatementContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitReturnStatement."); return null; }
            if (context.expr() != null) { Visit(context.expr()); }
            _ilGenerator.Emit(OpCodes.Ret);
            return null;
        }

        // ***** MÉTODO CORREGIDO: VisitDesignatorFactor *****
        public override object VisitDesignatorFactor(MiniCSharpParser.DesignatorFactorContext context)
        {
            if (_ilGenerator == null)
            {
                Console.Error.WriteLine("CodeGen Error (VisitDesignatorFactor): ILGenerator is null.");
                return null;
            }

            var designatorNode = context.designator();

            if (context.LPAREN() == null) // No es una llamada a método
            {
                Compiladores.Checker.Type designatorValueType = Compiladores.Checker.Type.Error;

                if (designatorNode.LBRACK().Length > 0) // Acceso a array: designator[expr]
                {
                    string baseArrayName = designatorNode.IDENT(0).GetText();
                    Symbol baseArraySymbol = _currentCodeGenScope.Find(baseArrayName) ?? _symbolTable.SearchGlobal(baseArrayName);

                    if (baseArraySymbol is VarSymbol arrayVarSymbol && arrayVarSymbol.Type is ArrayType arrayMiniCSharpType)
                    {
                        designatorValueType = arrayMiniCSharpType.ElementType; 

                        if (_localBuilders.TryGetValue(arrayVarSymbol, out LocalBuilder lb)) _ilGenerator.Emit(OpCodes.Ldloc, lb);
                        else if (_currentGeneratingMethodSymbol != null && _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == arrayVarSymbol.Name && p.Level == arrayVarSymbol.Level) is VarSymbol paramSymbol)
                        {
                            int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                            _ilGenerator.Emit(OpCodes.Ldarg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
                        }
                        else if (_fieldBuilders.TryGetValue(arrayVarSymbol, out FieldBuilder fb_arr))
                        {
                            if (!fb_arr.IsStatic) _ilGenerator.Emit(OpCodes.Ldarg_0);
                            _ilGenerator.Emit(fb_arr.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fb_arr);
                        }
                        else { Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): No se encontró almacenamiento para el array '{baseArrayName}'."); ExpressionTypes[context] = designatorValueType; return null; }
                        
                        Visit(designatorNode.expr(0)); 
                        
                        _ilGenerator.Emit(GetLdElemOpCode(designatorValueType));
                        // Console.WriteLine($"CodeGen INFO (VisitDesignatorFactor): Emitido {GetLdElemOpCode(designatorValueType)} para cargar elemento de array '{baseArrayName}'.");
                    }
                    else { Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Designator '{baseArrayName}' no es un array o no se encontró."); }
                }
                else if (designatorNode.DOT().Length == 0) // IDENT simple
                {
                    string varName = designatorNode.IDENT(0).GetText();
                    Symbol symbol = _currentCodeGenScope.Find(varName) ?? _symbolTable.SearchGlobal(varName);
                    if (symbol is VarSymbol varSymbol)
                    {
                        designatorValueType = varSymbol.Type;
                        if (varSymbol.Kind == SymbolKind.Constant)
                        {
                            if (varSymbol.Name == "null" && varSymbol.Type.Kind == TypeKind.Null) _ilGenerator.Emit(OpCodes.Ldnull);
                            else Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Carga de constante '{varSymbol.Name}' no implementada.");
                        }
                        else 
                        {
                            if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lb)) _ilGenerator.Emit(OpCodes.Ldloc, lb);
                            else if (_currentGeneratingMethodSymbol != null && _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == varSymbol.Name && p.Level == varSymbol.Level) is VarSymbol paramSymbol)
                            {
                                int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                                _ilGenerator.Emit(OpCodes.Ldarg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
                            }
                            else if (_fieldBuilders.TryGetValue(varSymbol, out FieldBuilder fb_field))
                            {
                                if (!fb_field.IsStatic) _ilGenerator.Emit(OpCodes.Ldarg_0);
                                _ilGenerator.Emit(fb_field.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fb_field);
                            }
                            else { Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): No se encontró almacenamiento para VarSymbol '{varName}'."); }
                        }
                    } else { Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Símbolo '{varName}' no es VarSymbol o no encontrado."); }
                }
                else // Acceso a campo obj.field
                {
                    designatorValueType = GetExpressionType(designatorNode); 
                    
                    Console.Error.WriteLine($"CodeGen Warning (VisitDesignatorFactor): Carga IL para designador con DOT '{designatorNode.GetText()}' necesita implementación completa de carga de objeto y luego campo.");
                    var netFieldType = ResolveNetType(designatorValueType);
                    if (designatorValueType != Compiladores.Checker.Type.Error && designatorValueType != Compiladores.Checker.Type.Void) {
                        if (!netFieldType.IsValueType) _ilGenerator.Emit(OpCodes.Ldnull);
                        else if (netFieldType == typeof(int) || netFieldType == typeof(char) || netFieldType == typeof(bool)) _ilGenerator.Emit(OpCodes.Ldc_I4_0);
                        else if (netFieldType == typeof(double)) _ilGenerator.Emit(OpCodes.Ldc_R8, 0.0);
                    } else {
                         _ilGenerator.Emit(OpCodes.Ldnull); 
                    }
                }
                ExpressionTypes[context] = designatorValueType; 
            }
            else // Es una llamada a método: designator ( [actPars] )
            {
                MethodSymbol calledMethodSym = ResolveDesignatorToCallableSymbol(designatorNode);
                Compiladores.Checker.Type methodReturnType = Compiladores.Checker.Type.Error;

                if (calledMethodSym != null) 
                {
                    methodReturnType = calledMethodSym.Type;
                }
                else 
                {
                    string methodNameText = designatorNode.GetText(); 
                    if (methodNameText.StartsWith("Console.")) { methodReturnType = Compiladores.Checker.Type.Void; }
                    else if (methodNameText == "len") { methodReturnType = Compiladores.Checker.Type.Int; }
                    else if (methodNameText == "add" || methodNameText == "del") { methodReturnType = Compiladores.Checker.Type.Void; }
                }
                
                HandleMethodCall(designatorNode, context.actPars());
                
                if (ExpressionTypes != null)
                {
                    ExpressionTypes[context] = methodReturnType; 
                }
            }
            return null;
        }


        // ***** MÉTODO CORREGIDO: VisitDesignatorStatement *****
        public override object VisitDesignatorStatement(MiniCSharpParser.DesignatorStatementContext context)
        {
            if (_ilGenerator == null)
            {
                Console.Error.WriteLine("CodeGen Error (VisitDesignatorStatement): ILGenerator is null.");
                return null;
            }

            var designatorNode = context.designator();

            if (context.ASSIGN() != null) 
            {
                if (designatorNode.LBRACK().Length > 0) 
                {
                    string baseArrayName = designatorNode.IDENT(0).GetText();
                    Symbol baseArraySymbol = _currentCodeGenScope.Find(baseArrayName) ?? _symbolTable.SearchGlobal(baseArrayName);

                    if (!(baseArraySymbol is VarSymbol arrayVarSymbol) || !(arrayVarSymbol.Type is ArrayType arrayMiniCSharpType))
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement Assign to Array Element): Designator '{baseArrayName}' no es un array o no se encontró.");
                        if (context.expr() != null) { Visit(context.expr()); if (GetExpressionType(context.expr()) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); } 
                        return null;
                    }
                    EmitLoadArrayAndIndex(arrayVarSymbol, designatorNode.expr(0)); 
                    Visit(context.expr()); 
                    
                    Compiladores.Checker.Type rhsType = GetExpressionType(context.expr());
                    Compiladores.Checker.Type lhsElementType = arrayMiniCSharpType.ElementType; 

                    if (lhsElementType == Compiladores.Checker.Type.Double && rhsType == Compiladores.Checker.Type.Int)
                    {
                        _ilGenerator.Emit(OpCodes.Conv_R8);
                    }
                    _ilGenerator.Emit(GetStElemOpCode(lhsElementType)); 
                }
                else 
                {
                    Compiladores.Checker.Type lhsType = GetExpressionType(designatorNode); 
                    Visit(context.expr()); 
                    Compiladores.Checker.Type rhsType = GetExpressionType(context.expr());

                    if (lhsType == Compiladores.Checker.Type.Double && rhsType == Compiladores.Checker.Type.Int)
                    {
                        _ilGenerator.Emit(OpCodes.Conv_R8);
                    }
                    EmitStoreToDesignator(designatorNode, lhsType); 
                }
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
                    Console.Error.WriteLine($"CodeGen Error (INC/DEC): Identificador '{baseIdentName}' no encontrado.");
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
                        Console.Error.WriteLine($"CodeGen Error (INC/DEC): Array base '{baseIdentName}' no encontrado o no es un array.");
                        return null;
                    }
                    varSymbol_inc_dec = arrayVarSym; 
                    targetType = arrayTypeObj.ElementType; 
                } else {
                    varSymbol_inc_dec = symbol as VarSymbol;
                    if (varSymbol_inc_dec == null) {
                         Console.Error.WriteLine($"CodeGen Error (INC/DEC): Símbolo '{baseIdentName}' no es una variable modificable.");
                         return null;
                    }
                    targetType = varSymbol_inc_dec.Type;
                }
                
                if (targetType != Compiladores.Checker.Type.Int && targetType != Compiladores.Checker.Type.Double) {
                     Console.Error.WriteLine($"CodeGen Error (INC/DEC): Operador solo para int o double, no para {targetType.Name}.");
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
                Console.Error.WriteLine($"CodeGen Error (EmitLoadArrayAndIndex): No se encontró almacenamiento para el array '{arrayVarSymbol.Name}'.");
                _ilGenerator.Emit(OpCodes.Ldnull); 
                _ilGenerator.Emit(OpCodes.Ldc_I4_0); 
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
    
            Console.Error.WriteLine($"CodeGen Warning: No se pudo determinar el scope de miembros para la clase actual '{_currentTypeBuilder.Name}' para buscar el campo '{fieldNameHint}'.");
            return null;
        }
        
        private void EmitStoreToDesignator(MiniCSharpParser.DesignatorContext designatorCtx, Compiladores.Checker.Type valueTypeOnStack)
        {
            if (_ilGenerator == null) 
            {
                Console.Error.WriteLine("CodeGen Error (EmitStoreToDesignator): ILGenerator is null.");
                if (valueTypeOnStack != Compiladores.Checker.Type.Void && valueTypeOnStack != Compiladores.Checker.Type.Error)
                     _ilGenerator.Emit(OpCodes.Pop);
                return;
            }
            if (designatorCtx == null)
            {
                Console.Error.WriteLine("CodeGen Error (EmitStoreToDesignator): designatorCtx es null.");
                if (valueTypeOnStack != Compiladores.Checker.Type.Void && valueTypeOnStack != Compiladores.Checker.Type.Error)
                     _ilGenerator.Emit(OpCodes.Pop); 
                return;
            }
            
            string baseName = designatorCtx.IDENT(0).GetText();
            Symbol symbol = _currentCodeGenScope.Find(baseName); 
            if (symbol == null && _currentTypeBuilder != null) { 
                Scope classScopeForFields = GetCurrentClassScopeForFieldAccess(baseName); 
                if (classScopeForFields != null) symbol = classScopeForFields.FindCurrent(baseName);
            }
            if (symbol == null) symbol = _symbolTable.SearchGlobal(baseName); 


            if (designatorCtx.LBRACK().Length > 0) 
            {
                Compiladores.Checker.Type arraySymbolType = null;
                Symbol arrayBaseSymbol = _currentCodeGenScope.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
                if (arrayBaseSymbol is VarSymbol vs) arraySymbolType = vs.Type;

                if (arraySymbolType is ArrayType arrayT)
                {
                    _ilGenerator.Emit(GetStElemOpCode(arrayT.ElementType));
                }
                else
                {
                     Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): '{baseName}' no es un array para asignación de elemento.");
                     if (valueTypeOnStack != Compiladores.Checker.Type.Void && valueTypeOnStack != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop); 
                     _ilGenerator.Emit(OpCodes.Pop); 
                     _ilGenerator.Emit(OpCodes.Pop); 
                }
            }
            else if (designatorCtx.DOT().Length > 0) 
            {
                string ident0 = designatorCtx.IDENT(0).GetText();
                string ident1 = designatorCtx.IDENT(1).GetText(); 
                Symbol baseSym = _currentCodeGenScope.Find(ident0) ?? _symbolTable.SearchGlobal(ident0);

                FieldBuilder fieldToStore = null;

                if (baseSym is VarSymbol baseVarSymbol) 
                {
                    if (baseVarSymbol.Type is ClassType objClassType)
                    {
                        Symbol fieldMemberSym = objClassType.Members.Find(ident1);
                        if (fieldMemberSym is VarSymbol fieldMemberVar && _fieldBuilders.TryGetValue(fieldMemberVar, out fieldToStore))
                        {
                            if (fieldToStore.IsStatic)
                            {
                                Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): Intento de acceso a campo estático '{ident1}' mediante instancia '{ident0}'.");
                                if (valueTypeOnStack != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); return;
                            }
                            
                            // Cargar la instancia 'obj'
                            // Pila antes: ..., value_to_store
                            // Necesitamos: ..., obj_ref, value_to_store -> Stfld
                            LocalBuilder tempValueToStore = _ilGenerator.DeclareLocal(ResolveNetType(valueTypeOnStack));
                            _ilGenerator.Emit(OpCodes.Stloc, tempValueToStore); // value_to_store -> temp. Pila: ...

                            if(_localBuilders.TryGetValue(baseVarSymbol, out LocalBuilder lbBase)) _ilGenerator.Emit(OpCodes.Ldloc, lbBase);
                            else if (_currentGeneratingMethodSymbol != null && _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p=>p.Name == baseVarSymbol.Name && p.Level == baseVarSymbol.Level) is VarSymbol pBase) {
                                int pIdx = _currentGeneratingMethodSymbol.Parameters.IndexOf(pBase);
                                _ilGenerator.Emit(OpCodes.Ldarg, _currentMethodBuilder.IsStatic ? pIdx : pIdx +1);
                            } else if (_fieldBuilders.TryGetValue(baseVarSymbol, out FieldBuilder fbBase) && !fbBase.IsStatic) {
                                _ilGenerator.Emit(OpCodes.Ldarg_0);
                                _ilGenerator.Emit(OpCodes.Ldfld, fbBase);
                            }
                            else {
                                 Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): No se pudo cargar la referencia para el objeto base '{baseVarSymbol.Name}'.");
                                 _ilGenerator.Emit(OpCodes.Ldloc, tempValueToStore); // Restaurar valor para pop si es necesario
                                 if (valueTypeOnStack != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); return;
                            }
                            // Pila ahora: ..., obj_ref
                            _ilGenerator.Emit(OpCodes.Ldloc, tempValueToStore); // Pila: ..., obj_ref, value_to_store
                            _ilGenerator.Emit(OpCodes.Stfld, fieldToStore);
                        } else { Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): Campo '{ident1}' no encontrado en '{ident0}'."); if (valueTypeOnStack != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
                    } else { Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): '{ident0}' no es una clase."); if (valueTypeOnStack != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
                }
                else if (baseSym is ClassSymbol baseClassSymbol) 
                {
                     if (baseClassSymbol.Type is ClassType objClassType) {
                        Symbol fieldMemberSym = objClassType.Members.Find(ident1);
                        if (fieldMemberSym is VarSymbol fieldMemberVar && _fieldBuilders.TryGetValue(fieldMemberVar, out fieldToStore)) {
                            if (!fieldToStore.IsStatic) {
                                Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): Campo '{ident1}' de '{ident0}' no es estático.");
                                if (valueTypeOnStack != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); return;
                            }
                            _ilGenerator.Emit(OpCodes.Stsfld, fieldToStore); 
                        } else { Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): Campo estático '{ident1}' no encontrado en clase '{ident0}'."); if (valueTypeOnStack != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
                     } else { Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): Símbolo '{ident0}' no es un tipo de clase válido."); if (valueTypeOnStack != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
                }
                else { Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): Base '{ident0}' para acceso a campo no es variable ni clase conocida."); if (valueTypeOnStack != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
            }
            else 
            {
                if (symbol is VarSymbol varSymbol)
                {
                    if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lb))
                    {
                        _ilGenerator.Emit(OpCodes.Stloc, lb);
                    }
                    else if (_currentGeneratingMethodSymbol != null && 
                             _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == varSymbol.Name && p.Level == varSymbol.Level) is VarSymbol paramSymbol)
                    {
                        int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                        _ilGenerator.Emit(OpCodes.Starg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
                    }
                    else if (_fieldBuilders.TryGetValue(varSymbol, out FieldBuilder fb))
                    {
                        if (fb.IsStatic)
                        {
                            _ilGenerator.Emit(OpCodes.Stsfld, fb);
                        }
                        else
                        {
                            LocalBuilder tempVal = _ilGenerator.DeclareLocal(ResolveNetType(valueTypeOnStack));
                            _ilGenerator.Emit(OpCodes.Stloc, tempVal);      
                            _ilGenerator.Emit(OpCodes.Ldarg_0);             
                            _ilGenerator.Emit(OpCodes.Ldloc, tempVal);      
                            _ilGenerator.Emit(OpCodes.Stfld, fb);
                        }
                    }
                    else
                    {
                        Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): No se encontró almacenamiento para VarSymbol '{varSymbol.Name}'.");
                         if (valueTypeOnStack != Compiladores.Checker.Type.Void && valueTypeOnStack != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                    }
                }
                else
                {
                     Console.Error.WriteLine($"CodeGen Error (EmitStoreToDesignator): Símbolo '{baseName}' no es VarSymbol o no encontrado para la asignación.");
                     if (valueTypeOnStack != Compiladores.Checker.Type.Void && valueTypeOnStack != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
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

            Console.Error.WriteLine($"CodeGen Warning (GetLdElemOpCode): No se encontró LDELEM específico para tipo {netElementType.FullName}. Usando Ldelem_Ref por defecto.");
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

            Console.Error.WriteLine($"CodeGen Warning (GetStElemOpCode): No se encontró STELEM específico para tipo {netElementType.FullName}. Usando Stelem_Ref por defecto.");
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
                     Console.Error.WriteLine("CodeGen Error (HandleMethodCall - len): ILGenerator is null.");
                     return;
                }
                if (actParsCtx == null || actParsCtx.expr().Length != 1)
                {
                    Console.Error.WriteLine("CodeGen Error (HandleMethodCall - len): La función 'len' espera 1 argumento (un array).");
                    if (actParsCtx != null) foreach (var argExpr_len in actParsCtx.expr()) if(GetExpressionType(argExpr_len)!=Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    _ilGenerator.Emit(OpCodes.Ldc_I4_M1); 
                    return;
                }

                var arrayExpr_len = actParsCtx.expr(0);
                Visit(arrayExpr_len); 

                Compiladores.Checker.Type arrayExprType_len = GetExpressionType(arrayExpr_len);
                if (arrayExprType_len.Kind != TypeKind.Array)
                {
                    Console.Error.WriteLine($"CodeGen Error (HandleMethodCall - len): El argumento para 'len' debe ser un array, se obtuvo {arrayExprType_len.Name}.");
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
                if (_ilGenerator == null) { Console.Error.WriteLine("CodeGen Error (HandleMethodCall - add): ILGenerator is null."); return; }
                if (actParsCtx == null || actParsCtx.expr().Length != 2)
                {
                    Console.Error.WriteLine("CodeGen Error (HandleMethodCall - add): La función 'add' espera 2 argumentos (array, elemento).");
                    if (actParsCtx != null) foreach (var argExpr_add in actParsCtx.expr()) if(GetExpressionType(argExpr_add)!=Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
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
                     Console.Error.WriteLine("CodeGen Error (HandleMethodCall - add): El primer argumento para 'add' debe ser una variable de array (designador).");
                     Visit(arrayArgExprCtx); if(GetExpressionType(arrayArgExprCtx) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                     Visit(elementArgExprCtx); if(GetExpressionType(elementArgExprCtx) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                     return;
                }
                
                Visit(arrayArgExprCtx); 
                Compiladores.Checker.Type actualArrayType = GetExpressionType(arrayArgExprCtx);
                 if (actualArrayType.Kind != TypeKind.Array) {
                    Console.Error.WriteLine($"CodeGen Error (HandleMethodCall - add): El primer argumento para 'add' debe ser un array, se obtuvo {actualArrayType.Name}.");
                     if (actualArrayType != Compiladores.Checker.Type.Void && actualArrayType != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                     Visit(elementArgExprCtx); if(GetExpressionType(elementArgExprCtx) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
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
                
                if (helperMethodInfo_add == null)
                {
                    Console.Error.WriteLine($"CodeGen Error (HandleMethodCall - add): No se encontró un helper Add para el tipo de elemento {elementMetaType.Name}.");
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
                if (_ilGenerator == null) { Console.Error.WriteLine("CodeGen Error (HandleMethodCall - del): ILGenerator is null."); return; }
                if (actParsCtx == null || actParsCtx.expr().Length != 2)
                {
                    Console.Error.WriteLine("CodeGen Error (HandleMethodCall - del): La función 'del' espera 2 argumentos (array, index).");
                    if (actParsCtx != null) foreach (var argExpr_del in actParsCtx.expr()) if(GetExpressionType(argExpr_del)!=Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
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
                     Console.Error.WriteLine("CodeGen Error (HandleMethodCall - del): El primer argumento para 'del' debe ser una variable de array (designador).");
                     Visit(arrayArgExprCtx_del); if(GetExpressionType(arrayArgExprCtx_del) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                     Visit(indexArgExprCtx_del); if(GetExpressionType(indexArgExprCtx_del) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                     return;
                }
                Visit(arrayArgExprCtx_del); 
                Compiladores.Checker.Type actualArrayType_del = GetExpressionType(arrayArgExprCtx_del);
                 if (actualArrayType_del.Kind != TypeKind.Array) {
                    Console.Error.WriteLine($"CodeGen Error (HandleMethodCall - del): El primer argumento para 'del' debe ser un array, se obtuvo {actualArrayType_del.Name}.");
                     if (actualArrayType_del != Compiladores.Checker.Type.Void && actualArrayType_del != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop);
                     Visit(indexArgExprCtx_del); if(GetExpressionType(indexArgExprCtx_del) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }
                ArrayType arrMetaType_del = (ArrayType)actualArrayType_del;
                Compiladores.Checker.Type elementMetaType_del = arrMetaType_del.ElementType; 
                Visit(indexArgExprCtx_del); 
                Compiladores.Checker.Type actualIndexType_del = GetExpressionType(indexArgExprCtx_del);
                if (actualIndexType_del != Compiladores.Checker.Type.Int) {
                     Console.Error.WriteLine($"CodeGen Error (HandleMethodCall - del): El segundo argumento para 'del' (index) debe ser un entero, se obtuvo {actualIndexType_del.Name}.");
                     if (actualIndexType_del != Compiladores.Checker.Type.Void && actualIndexType_del != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop); 
                     if (actualArrayType_del != Compiladores.Checker.Type.Void && actualArrayType_del != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop); 
                    return;
                }
                
                MethodInfo helperMethodInfo_del = null;
                if (elementMetaType_del == Compiladores.Checker.Type.Int)
                    helperMethodInfo_del = typeof(MiniCSharpRuntimeHelpers).GetMethod("DeleteIntElementAt", new[] { typeof(int[]), typeof(int) });
                else if (elementMetaType_del == Compiladores.Checker.Type.Char)
                    helperMethodInfo_del = typeof(MiniCSharpRuntimeHelpers).GetMethod("DeleteCharElementAt", new[] { typeof(char[]), typeof(int) });
                
                if (helperMethodInfo_del == null)
                {
                    Console.Error.WriteLine($"CodeGen Error (HandleMethodCall - del): No se encontró un helper Delete para el tipo de elemento {elementMetaType_del.Name}.");
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
                    Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): MethodBuilder not found for resolved MethodSymbol '{resolvedMethodSymbol.Name}'.");
                    if (actParsCtx != null) foreach (var argExpr in actParsCtx.expr()) if(GetExpressionType(argExpr)!=Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    return;
                }
            }
            else 
            {
                string fullMethodName = designatorCtx.GetText();
                if (designatorCtx.DOT().Length > 0 && designatorCtx.IDENT(0).GetText() == "Console") {
                    string consoleMethodName = designatorCtx.IDENT(1).GetText();
                    List<System.Type> argTypes = new List<System.Type>();
                    if (actParsCtx != null) {
                        foreach (var argExpr in actParsCtx.expr()) {
                             argTypes.Add(ResolveNetType(GetExpressionType(argExpr)));
                        }
                    }
                    try {
                        methodToCall = typeof(System.Console).GetMethod(consoleMethodName, argTypes.ToArray());
                        
                        if (methodToCall == null && consoleMethodName == "WriteLine" && argTypes.Count == 1) {
                            methodToCall = typeof(System.Console).GetMethod("WriteLine", new[] { typeof(object) });
                        }

                        if(methodToCall == null) {
                             Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Could not find System.Console.{consoleMethodName} with specified argument types.");
                             return;
                        }
                    } catch (AmbiguousMatchException) {
                         Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Ambiguous match for System.Console.{consoleMethodName}.");
                         return;
                    }
                } else {
                    Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Could not resolve method '{designatorCtx.GetText()}' to a MethodSymbol or known external.");
                    if (actParsCtx != null) foreach (var argExpr in actParsCtx.expr()) if(GetExpressionType(argExpr)!=Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
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
                         Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Cannot call instance method '{resolvedMethodSymbol.Name}' from a static context without an object instance.");
                         return;
                    }
                    _ilGenerator.Emit(OpCodes.Ldarg_0); 
                    isInstanceMethodOnThis = true;
                }
                 else if (designatorCtx.DOT().Length > 0) 
                {

                }
            }
            
            if (actParsCtx != null)
            {
                var formalParams = resolvedMethodSymbol?.Parameters; 
                if (formalParams != null && formalParams.Count != actParsCtx.expr().Length && resolvedMethodSymbol != null)
                {
                     Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Argument count mismatch for method '{resolvedMethodSymbol.Name}'. Expected {formalParams.Count}, got {actParsCtx.expr().Length}.");
                     if(isInstanceMethodOnThis) _ilGenerator.Emit(OpCodes.Pop); 
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
                OpCode callInstruction;
                if (methodToCall.IsStatic) {
                    callInstruction = OpCodes.Call;
                } else {
                    if (methodToCall.DeclaringType != null && methodToCall.DeclaringType.IsValueType) {
                        callInstruction = OpCodes.Call; 
                    } else {
                        callInstruction = OpCodes.Callvirt; 
                    }
                }
                
                if (methodToCall is MethodInfo mi) 
                {
                    _ilGenerator.Emit(callInstruction, mi);
                }
                else if (methodToCall is ConstructorInfo ci && callInstruction == OpCodes.Call) 
                {
                    _ilGenerator.Emit(OpCodes.Call, ci); 
                }
                else
                {
                    Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): methodToCall ('{methodToCall.Name}') no es MethodInfo o ConstructorInfo compatible. Tipo actual: {methodToCall.GetType().FullName}");
                    if (actParsCtx != null) foreach (var argExpr in actParsCtx.expr()) if(GetExpressionType(argExpr)!=Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                    if (isInstanceMethodOnThis) _ilGenerator.Emit(OpCodes.Pop);
                }
            }
            else
            {
                Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): methodToCall es null antes de emitir OpCodes.Call para '{designatorCtx.GetText()}'.");
                if (actParsCtx != null) foreach (var argExpr in actParsCtx.expr()) if(GetExpressionType(argExpr)!=Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                if (isInstanceMethodOnThis) _ilGenerator.Emit(OpCodes.Pop); 
            }
        }
         private MethodSymbol ResolveDesignatorToCallableSymbol(MiniCSharpParser.DesignatorContext designatorCtx)
        {
            if (designatorCtx == null || designatorCtx.IDENT() == null || designatorCtx.IDENT().Length == 0)
            {
                Console.Error.WriteLine("CodeGen Error (ResolveDesignatorToCallableSymbol): Invalid designator context.");
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
                 else if (baseIdentifierName == "MiniCSharpRuntimeHelpers") // Para llamar a nuestros helpers estáticamente
                {
                    // Esto es un marcador, la lógica real de encontrar el MethodInfo está en HandleMethodCall para helpers.
                    // Pero para que ResolveDesignatorToCallableSymbol no falle completamente:
                    if (memberMethodName.StartsWith("Add") || memberMethodName.StartsWith("Del")) { // Nombres de nuestros helpers
                         // Devolver un MethodSymbol ficticio o buscarlo si lo añadimos a una "tabla de símbolos de runtime"
                         // Por ahora, esto permitirá que no se considere un error de resolución inmediato aquí.
                         // La lógica real de llamada está en HandleMethodCall.
                         // O, mejor aún, no hacer nada aquí y dejar que HandleMethodCall lo resuelva por reflexión.
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
                Console.Error.WriteLine($"CodeGen Error (ResolveDesignatorToCallableSymbol): Cannot resolve method '{memberMethodName}' on base '{baseIdentifierName}'. Base is not a known class or object with members.");
                return null;
            }
            
            Console.Error.WriteLine($"CodeGen Error (ResolveDesignatorToCallableSymbol): Complex designator for method call not fully supported: {designatorCtx.GetText()}");
            return null;
        }
        
        public override object VisitExpr(MiniCSharpParser.ExprContext context)
        {
            if (_ilGenerator == null) 
            { 
                Console.Error.WriteLine("CodeGen Error (VisitExpr): ILGenerator is null."); 
                return null; 
            }

            Visit(context.term(0)); 
            Compiladores.Checker.Type currentLeftType = GetExpressionType(context.term(0));


            if (context.MINUS() != null) 
            {
                if (currentLeftType == Compiladores.Checker.Type.Int || currentLeftType == Compiladores.Checker.Type.Double)
                {
                    _ilGenerator.Emit(OpCodes.Neg);
                }
                else if (currentLeftType != Compiladores.Checker.Type.Error) 
                {
                    Console.Error.WriteLine($"CodeGen Error (VisitExpr): Unary minus cannot be applied to type '{currentLeftType.Name}'.");
                }
            }

            for (int i = 0; i < context.addop().Length; i++)
            {
                Visit(context.term(i + 1)); 
                Compiladores.Checker.Type rightTermType = GetExpressionType(context.term(i + 1));
                                             
                if (currentLeftType == Compiladores.Checker.Type.Error || rightTermType == Compiladores.Checker.Type.Error)
                {
                    if(currentLeftType != Compiladores.Checker.Type.Error && rightTermType == Compiladores.Checker.Type.Error && currentLeftType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    if(currentLeftType == Compiladores.Checker.Type.Error && rightTermType != Compiladores.Checker.Type.Error && rightTermType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    currentLeftType = Compiladores.Checker.Type.Error;
                    continue; 
                }
                
                string op = context.addop(i).GetText();
                OpCode? opCodeToEmit = null;
                Compiladores.Checker.Type targetType = Compiladores.Checker.Type.Error;

                if ((currentLeftType == Compiladores.Checker.Type.Int || currentLeftType == Compiladores.Checker.Type.Double) &&
                    (rightTermType == Compiladores.Checker.Type.Int || rightTermType == Compiladores.Checker.Type.Double))
                {
                    if (currentLeftType == Compiladores.Checker.Type.Double || rightTermType == Compiladores.Checker.Type.Double)
                    {
                        targetType = Compiladores.Checker.Type.Double;
                        if (rightTermType == Compiladores.Checker.Type.Int)
                        {
                            _ilGenerator.Emit(OpCodes.Conv_R8);
                        }
                        else if (currentLeftType == Compiladores.Checker.Type.Int && rightTermType == Compiladores.Checker.Type.Double)
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
                    opCodeToEmit = (op == "+") ? OpCodes.Add : OpCodes.Sub;
                }
                else if (op == "+" && 
                    (currentLeftType == Compiladores.Checker.Type.String || rightTermType == Compiladores.Checker.Type.String))
                {
                    if (rightTermType != Compiladores.Checker.Type.String) {
                         EmitToStringCoercion(rightTermType, false); 
                    }
                    if (currentLeftType != Compiladores.Checker.Type.String) {
                        LocalBuilder tempRightString = _ilGenerator.DeclareLocal(typeof(string));
                        _ilGenerator.Emit(OpCodes.Stloc, tempRightString); 
                        EmitToStringCoercion(currentLeftType, false); 
                        _ilGenerator.Emit(OpCodes.Ldloc, tempRightString); 
                    }

                    MethodInfo concatMethod = typeof(string).GetMethod("Concat", new[] { typeof(string), typeof(string) });
                    if (concatMethod != null) {
                         _ilGenerator.Emit(OpCodes.Call, concatMethod);
                         targetType = Compiladores.Checker.Type.String;
                    } else {
                        Console.Error.WriteLine($"CodeGen Error (VisitExpr): Could not find string.Concat(string,string) method.");
                         if (currentLeftType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                         if (rightTermType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop);
                        targetType = Compiladores.Checker.Type.Error;
                    }
                }
                else
                {
                    Console.Error.WriteLine($"CodeGen Error (VisitExpr): Operator '{op}' cannot be applied to operands of type '{currentLeftType.Name}' and '{rightTermType.Name}'.");
                    if (currentLeftType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
                    if (rightTermType != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); 
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
                Console.Error.WriteLine("CodeGen Error (VisitTerm): ILGenerator is null."); 
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
                             Console.Error.WriteLine($"CodeGen Error (VisitTerm): Operator '%' not supported for double operands. Assuming integer operation based on checker rules.");
                             targetType = Compiladores.Checker.Type.Int; 
                        }
                        opCodeToEmit = OpCodes.Rem;
                    }
                }
                else
                {
                    Console.Error.WriteLine($"CodeGen Error (VisitTerm): Operator '{op}' cannot be applied to operands of type '{currentLeftType.Name}' and '{rightFactorType.Name}'.");
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
                Console.Error.WriteLine("CodeGen Error (VisitCondition): ILGenerator is null.");
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
                Console.Error.WriteLine("CodeGen Error (VisitCondTerm): ILGenerator is null.");
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
                Console.Error.WriteLine("CodeGen Error (VisitCondFact): ILGenerator is null.");
                return null;
            }

            Visit(context.expr(0));
            Compiladores.Checker.Type leftType = GetExpressionType(context.expr(0));

            if (context.relop() == null)
            {
                if (leftType != Compiladores.Checker.Type.Bool)
                {
                    Console.Error.WriteLine($"CodeGen Warning (VisitCondFact): Condition '{context.expr(0).GetText()}' is not bool and used directly. Emitting Ldc_I4_0; Ceq to coerce to bool (0 for true, 1 for false then inverted).");
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
                    Console.Error.WriteLine($"CodeGen Error (VisitCondFact): Operador relacional desconocido '{op}'.");
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
                Console.Error.WriteLine("CodeGen Error (VisitIfStatement): ILGenerator is null.");
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
            if (_ilGenerator == null) { Console.Error.WriteLine("CodeGen Error (VisitWhileStatement): ILGenerator is null."); return null; }

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
                Console.Error.WriteLine("CodeGen Error (VisitNewFactor): ILGenerator is null.");
                return null;
            }
            
            // El tipo de un NewFactor es determinado por el checker y almacenado en ExpressionTypes.
            // No necesitamos recalcularlo aquí, solo obtenerlo.
            Compiladores.Checker.Type newMiniCSharpType = GetExpressionType(context); // Usa el tipo ya calculado.
            
            // Es crucial almacenar el tipo del NewFactor mismo, ya que GetExpressionType lo necesitará.
            // El checker ya debería haber hecho StoreAndReturnType(context, determinedType)
            // Por lo tanto, GetExpressionType(context) ya debería devolver el tipo correcto.

            if (newMiniCSharpType == Compiladores.Checker.Type.Error)
            {
                Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): No se pudo determinar el tipo para 'new {context.IDENT().GetText()}'. Tipo resuelto por checker: Error.");
                _ilGenerator.Emit(OpCodes.Ldnull); // Poner algo en la pila para evitar más errores
                return null;
            }
            ExpressionTypes[context] = newMiniCSharpType; // Asegurar que esté almacenado aquí también si GetExpressionType lo infirió.

            if (newMiniCSharpType.Kind == TypeKind.Array) 
            {
                ArrayType arrayMiniCSharpType = (ArrayType)newMiniCSharpType; 
                
                Visit(context.expr()); // Pone el tamaño del array en la pila
                                       
                System.Type netElementType = ResolveNetType(arrayMiniCSharpType.ElementType);
                if (netElementType == null || (netElementType == typeof(object) && arrayMiniCSharpType.ElementType != Compiladores.Checker.Type.Null && arrayMiniCSharpType.ElementType != Compiladores.Checker.Type.Error) )
                {
                     Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): No se pudo resolver el tipo de elemento .NET para el array '{arrayMiniCSharpType.ElementType.Name}'.");
                     _ilGenerator.Emit(OpCodes.Pop); // Pop size
                     _ilGenerator.Emit(OpCodes.Ldnull); 
                     return null;
                }
                
                _ilGenerator.Emit(OpCodes.Newarr, netElementType);
                // Console.WriteLine($"CodeGen INFO (VisitNewFactor): Emitido Newarr para tipo {netElementType.FullName}[]."); // Log ya existe
            }
            else if (newMiniCSharpType.Kind == TypeKind.Class) 
            {
                System.Type netClassType = ResolveNetType(newMiniCSharpType);

                if (netClassType == null || (netClassType == typeof(object) && newMiniCSharpType != Compiladores.Checker.Type.Null && newMiniCSharpType != Compiladores.Checker.Type.Error ))
                {
                     Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): No se pudo resolver el tipo .NET para la clase '{newMiniCSharpType.Name}'.");
                     _ilGenerator.Emit(OpCodes.Ldnull); 
                     return null;
                }

                if (netClassType.IsAbstract || netClassType.IsInterface || netClassType is TypeBuilder) { // TypeBuilder no es instanciable directamente con Newobj si no está "horneado"
                     Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): No se puede instanciar tipo '{netClassType.FullName}' (puede ser abstracto, interfaz o TypeBuilder no finalizado).");
                     _ilGenerator.Emit(OpCodes.Ldnull);
                     return null;
                }
                
                ConstructorInfo constructor = netClassType.GetConstructor(System.Type.EmptyTypes);
                
                if (constructor == null)
                {
                    Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): No se encontró un constructor por defecto sin parámetros para la clase '{netClassType.FullName}'.");
                     _ilGenerator.Emit(OpCodes.Ldnull);
                     return null;
                }
                
                _ilGenerator.Emit(OpCodes.Newobj, constructor);
                // Console.WriteLine($"CodeGen INFO (VisitNewFactor): Emitido Newobj para clase {netClassType.FullName}."); // Log ya existe
            }
            else
            {
                Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): Tipo inesperado '{newMiniCSharpType.Name}' (Kind: {newMiniCSharpType.Kind}) para operación 'new'.");
                _ilGenerator.Emit(OpCodes.Ldnull); 
            }
            return null;
        }
        
        public override object VisitUsingDirective(MiniCSharpParser.UsingDirectiveContext context) { return base.VisitUsingDirective(context); }
        public override object VisitQualifiedIdent(MiniCSharpParser.QualifiedIdentContext context) { return base.VisitQualifiedIdent(context); }
        public override object VisitFormPars(MiniCSharpParser.FormParsContext context) { return base.VisitFormPars(context); }
        
        public override object VisitForStatement(MiniCSharpParser.ForStatementContext context)
{
    if (_ilGenerator == null) { Console.Error.WriteLine("CodeGen Error (VisitForStatement): ILGenerator is null."); return null; }

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
         Console.Error.WriteLine("CodeGen Error (VisitForStatement): No se encontró el statement del cuerpo para el bucle FOR.");
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
    else 
    {
        Console.Error.WriteLine("CodeGen Error (VisitForStatement): El contexto del cuerpo del bucle FOR es nulo.");
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
    if (_ilGenerator == null) { Console.Error.WriteLine("CodeGen Error (VisitSwitchStatement): ILGenerator is null."); return null; }

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
            if (_ilGenerator == null) { Console.Error.WriteLine("CodeGen Error (VisitBreakStatement): ILGenerator is null."); return null; }

            if (_breakLabelStack.Count > 0)
            {
                _ilGenerator.Emit(OpCodes.Br, _breakLabelStack.Peek());
            }
            else
            {
                Console.Error.WriteLine("CodeGen Error (VisitBreakStatement): No hay bucle o switch envolvente del que salir con break.");
            }
            return null;
        }

        public override object VisitReadStatement(MiniCSharpParser.ReadStatementContext context)
        {
            Console.Error.WriteLine($"CodeGen SKIPPED: VisitReadStatement no implementado completamente.");
            return base.VisitReadStatement(context);
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
            Console.Error.WriteLine($"CodeGen SKIPPED: VisitCast no implementado (casting no soportado).");
            return base.VisitCast(context); 
        }
        public override object VisitParenFactor(MiniCSharpParser.ParenFactorContext context) 
        { 
            Visit(context.expr());
            // Un factor entre paréntesis tiene el tipo de la expresión interna.
            // Asegurarse de que este tipo se almacene para el nodo ParenFactorContext.
            ExpressionTypes[context] = GetExpressionType(context.expr());
            return null; // Visit ya puso el valor en la pila.
        } 
        
        public override object VisitDesignator(MiniCSharpParser.DesignatorContext context) 
        { 
            // Este método normalmente no emitiría código por sí mismo si se llama directamente.
            // La lógica de carga/almacenamiento está en VisitDesignatorFactor o VisitDesignatorStatement.
            // Pero sí necesitamos que el tipo del DesignatorContext esté en ExpressionTypes,
            // lo cual el Checker debería hacer. Si GetExpressionType lo llama como fallback,
            // intentará resolverlo y cachearlo.
            GetExpressionType(context); // Para asegurar que se resuelva y cachee si es necesario.
            return base.VisitDesignator(context); 
        } 
        
        public override object VisitNumber(MiniCSharpParser.NumberContext context) { 
            if (context.INTCONST() != null) _ilGenerator.Emit(OpCodes.Ldc_I4, int.Parse(context.INTCONST().GetText()));
            else if (context.DOUBLECONST() != null) _ilGenerator.Emit(OpCodes.Ldc_R8, double.Parse(context.DOUBLECONST().GetText()));
            // Almacenar el tipo del nodo NumberContext
            ExpressionTypes[context] = context.INTCONST() != null ? Compiladores.Checker.Type.Int : Compiladores.Checker.Type.Double;
            return null;
        }
        
        public override object VisitRelop(MiniCSharpParser.RelopContext context) { return base.VisitRelop(context); }
        public override object VisitAddop(MiniCSharpParser.AddopContext context) { return base.VisitAddop(context); }
        public override object VisitMulop(MiniCSharpParser.MulopContext context) { return base.VisitMulop(context); }

    }
}