// Archivo: compiladores/CodeGen/MiniCSharpCodeGenerator.cs
using System;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime.Tree;
using Compiladores.Checker;
using generated;

namespace Compiladores.CodeGen
{
    public class MiniCSharpCodeGenerator : MiniCSharpParserBaseVisitor<object>
    {
        private readonly TablaSimbolos _symbolTable; //
        private readonly string _assemblyNameBase; //
        private MiniCSharpParser.ProgramContext _programContext; //
        private MethodSymbol _currentGeneratingMethodSymbol = null; //

        private AssemblyBuilder _assemblyBuilder; //
        private ModuleBuilder _moduleBuilder; //
        private TypeBuilder _currentTypeBuilder; //
        private MethodBuilder _currentMethodBuilder; //
        private ILGenerator _ilGenerator; //

        private Dictionary<Symbol, FieldBuilder> _fieldBuilders = new Dictionary<Symbol, FieldBuilder>(); //
        private Dictionary<Symbol, MethodBuilder> _methodBuilders = new Dictionary<Symbol, MethodBuilder>(); //
        private Dictionary<Symbol, LocalBuilder> _localBuilders = new Dictionary<Symbol, LocalBuilder>(); //
        private Dictionary<ClassSymbol, TypeBuilder> _definedTypes = new Dictionary<ClassSymbol, TypeBuilder>(); //

        private Scope _currentCodeGenScope; //
        

        // NUEVO: Stack para manejar los targets de los 'break' statements
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
            string mainClassName = programContext.IDENT().GetText();
            ClassSymbol mainClassSymbol = _symbolTable.SearchGlobal(mainClassName) as ClassSymbol;
            if (mainClassSymbol != null && _definedTypes.TryGetValue(mainClassSymbol, out TypeBuilder mainTypeBuilder))
            {
                return mainTypeBuilder.IsCreated() ? mainTypeBuilder : mainTypeBuilder.CreateTypeInfo().AsType();
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
                    var arrayType = (ArrayType)miniCSharpType; //
                    System.Type elementType = ResolveNetType(arrayType.ElementType);
                    return elementType.MakeArrayType();
                case TypeKind.Class: //
                    Symbol classDefSymbol = _symbolTable.SearchGlobal(miniCSharpType.Name); 
                    if (classDefSymbol == null && _currentCodeGenScope != null) { 
                        classDefSymbol = _currentCodeGenScope.Find(miniCSharpType.Name);
                    }

                    if (classDefSymbol is ClassSymbol userClassSymbol && _definedTypes.TryGetValue(userClassSymbol, out TypeBuilder tb))
                    {
                        return tb;
                    }
                    if (miniCSharpType.Name == "Console") return typeof(System.Console);
                    
                    Console.WriteLine($"Advertencia (ResolveNetType): No se pudo resolver el tipo .NET para la clase '{miniCSharpType.Name}'. Usando 'object'.");
                    return typeof(object);
                default:
                    Console.Error.WriteLine($"Error Crítico (ResolveNetType): Tipo MiniCSharp no soportado para conversión: {miniCSharpType.Kind} ('{miniCSharpType.Name}')");
                    return typeof(object); 
            }
        }

        public override object VisitProgram(MiniCSharpParser.ProgramContext context)
        {
            _symbolTable.SetCurrentScopeTo(_symbolTable.GetGlobalScope(), _symbolTable.GetGlobalScopeLevel()); //
            _currentCodeGenScope = _symbolTable.GetGlobalScope(); //
            string className = context.IDENT().GetText(); //
            ClassSymbol progClassSymbol = _symbolTable.SearchGlobal(className) as ClassSymbol; //
            if (progClassSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No ClassSymbol para '{className}'."); return null; } //
            
            _currentTypeBuilder = _moduleBuilder.DefineType(className, TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.AutoLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit, typeof(object)); //
            if (!_definedTypes.TryAdd(progClassSymbol, _currentTypeBuilder)) { _definedTypes[progClassSymbol] = _currentTypeBuilder; } //
            
            Scope classScope = (progClassSymbol.Type as ClassType)?.Members; 
            if (classScope == null) { Console.Error.WriteLine($"CodeGen Error: ClassSymbol '{className}' no tiene Members."); return null; } //
            
            Scope outerScope = _currentCodeGenScope; //
            _currentCodeGenScope = classScope; //
            
            foreach (var varDeclContext in context.varDecl()) Visit(varDeclContext); //
            foreach (var classDeclContext in context.classDecl()) Visit(classDeclContext);  //
            foreach (var methodDeclContext in context.methodDecl()) Visit(methodDeclContext); //
            
            _currentCodeGenScope = outerScope; //
            
            var createdType = _currentTypeBuilder.CreateType(); //
            if (createdType == null) //
            {
                Console.Error.WriteLine($"CodeGen Error: Falló la creación del tipo para la clase principal '{className}'."); //
            }
            return null; //
        }
        
        public override object VisitClassDecl(MiniCSharpParser.ClassDeclContext context)
        {
            string className = context.IDENT().GetText(); //
            ClassSymbol classSymbol = _currentCodeGenScope.FindCurrent(className) as ClassSymbol; //
            if (classSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No ClassSymbol para clase anidada '{className}' en scope '{_currentTypeBuilder?.Name}'."); return null; } //
            
            TypeBuilder outerTypeBuilder = _currentTypeBuilder; //
            _currentTypeBuilder = outerTypeBuilder.DefineNestedType(className, TypeAttributes.NestedPublic | TypeAttributes.Class | TypeAttributes.AutoLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit | TypeAttributes.Sealed, typeof(object)); //
            if (!_definedTypes.TryAdd(classSymbol, _currentTypeBuilder)) { _definedTypes[classSymbol] = _currentTypeBuilder; } //
            
            Scope outerScope = _currentCodeGenScope; //
            _currentCodeGenScope = (classSymbol.Type as ClassType)?.Members; 
            if (_currentCodeGenScope == null) { Console.Error.WriteLine($"CodeGen Error: ClassSymbol anidado '{className}' no tiene Members."); _currentTypeBuilder = outerTypeBuilder; return null; } //
            
            foreach (var varDeclContext in context.varDecl()) Visit(varDeclContext); //

            _currentCodeGenScope = outerScope; //
            var createdNestedType = _currentTypeBuilder.CreateType(); //
            if (createdNestedType == null) //
            {
                Console.Error.WriteLine($"CodeGen Error: Falló la creación del tipo para la clase anidada '{className}'."); //
            }
            _currentTypeBuilder = outerTypeBuilder; //
            return null; //
        }

        public override object VisitVarDecl(MiniCSharpParser.VarDeclContext context)
        {
            Compiladores.Checker.Type varMiniCSharpType = (Compiladores.Checker.Type)Visit(context.type()); //
            
            if (varMiniCSharpType == Compiladores.Checker.Type.Error) //
            {
                Console.Error.WriteLine($"CodeGen Error (VisitVarDecl): No se pudo resolver el tipo para la variable que comienza con '{context.IDENT(0).GetText()}'."); //
                return null; //
            }
            System.Type varNetType = ResolveNetType(varMiniCSharpType); //

            foreach (var identNode in context.IDENT()) //
            {
                string varName = identNode.GetText(); //
                VarSymbol varSymbol = _currentCodeGenScope.FindCurrent(varName) as VarSymbol; //

                if (varSymbol == null) //
                {
                    Console.Error.WriteLine($"CodeGen Error (VisitVarDecl): No se encontró VarSymbol para '{varName}' en el scope actual (HashCode: {_currentCodeGenScope?.GetHashCode()})."); //
                    if(_currentCodeGenScope != null) { //
                        Console.Error.WriteLine($"Símbolos presentes en este scope del CodeGen ({_currentCodeGenScope.GetHashCode()}): " + string.Join(", ", _currentCodeGenScope.Symbols.Keys)); //
                    }
                    continue; //
                }

                if (_currentMethodBuilder != null) //
                {
                    if (_ilGenerator == null) //
                    {
                        Console.Error.WriteLine($"Error Crítico (VisitVarDecl): ILGenerator es null al declarar variable local '{varName}'."); //
                        return null; //
                    }
                    LocalBuilder lb = _ilGenerator.DeclareLocal(varNetType); //
                    _localBuilders[varSymbol] = lb; //
                    Console.WriteLine($"CodeGen INFO (VisitVarDecl): Declarado LocalBuilder para '{varName}' (VarSymbol Hash: {varSymbol.GetHashCode()}, LocalBuilder Index: {lb.LocalIndex}, Scope Hash: {_currentCodeGenScope.GetHashCode()})"); //
                }
                else //
                {
                    FieldAttributes attributes = FieldAttributes.Public; //
                    if (_currentTypeBuilder != null && _programContext != null && _currentTypeBuilder.Name == _programContext.IDENT().GetText()) //
                    {
                        attributes |= FieldAttributes.Static; //
                    }
                    FieldBuilder fb = _currentTypeBuilder.DefineField(varName, varNetType, attributes); //
                    _fieldBuilders[varSymbol] = fb; //
                    Console.WriteLine($"CodeGen INFO (VisitVarDecl): Definido FieldBuilder para '{varName}' (Estático: {fb.IsStatic})"); //
                }
            }
            return null; //
        }
        
        public override object VisitType(MiniCSharpParser.TypeContext context)
        {
            string typeName = context.IDENT().GetText();
            Symbol typeDefiningSymbol = _symbolTable.Search(typeName);
            if (typeDefiningSymbol == null || !(typeDefiningSymbol is TypeDefSymbol || typeDefiningSymbol is ClassSymbol))
            { Console.Error.WriteLine($"CodeGen Error: Tipo '{typeName}' no encontrado. Scope: {_currentCodeGenScope?.GetHashCode()}"); return Compiladores.Checker.Type.Error; }
            
            Compiladores.Checker.Type baseType = typeDefiningSymbol.Type;
            if (context.LBRACK() != null && context.RBRACK() != null) { return new ArrayType(baseType); } //
            return baseType;
        }

        public override object VisitMethodDecl(MiniCSharpParser.MethodDeclContext context)
        {
            string methodName = context.IDENT().GetText();
            MethodSymbol methodSymbol = _currentCodeGenScope.FindCurrent(methodName) as MethodSymbol;

            if (methodSymbol == null) {
                Console.Error.WriteLine($"CodeGen Error: No se encontró MethodSymbol para '{methodName}' en el scope de la clase '{_currentTypeBuilder?.Name}'.");
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
            if (methodName == "Main" && _currentTypeBuilder != null && _programContext != null && _currentTypeBuilder.Name == _programContext.IDENT().GetText())
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

            Scope methodScopeFromChecker = _symbolTable.GetScopeForNode(context); //

            if (methodScopeFromChecker == null) {
                Console.Error.WriteLine($"CodeGen CRITICAL Error: No se encontró el scope del checker para el método '{methodName}'.");
                _ilGenerator = previousILGenerator;
                _currentMethodBuilder = previousMethodBuilder;
                _currentGeneratingMethodSymbol = previousGeneratingMethodSymbol;
                return null;
            }
            
            _currentCodeGenScope = methodScopeFromChecker;
            
            int methodScopeLevel = methodSymbol.Parameters.Any() ? methodSymbol.Parameters[0].Level : (originalLevelInSymbolTable + 1);
            _symbolTable.SetCurrentScopeTo(methodScopeFromChecker, methodScopeLevel);
            
            Visit(context.block());
            
            _symbolTable.SetCurrentScopeTo(classScopeForCodeGen, originalLevelInSymbolTable);
            _currentCodeGenScope = classScopeForCodeGen;

            bool isVoid = (returnType == typeof(void));
            var stmts = context.block().statement();
            bool blockIsEmptyOrNull = stmts == null || !stmts.Any();
            var lastStatementNode = stmts?.LastOrDefault();
            bool lastStmtIsReturn = lastStatementNode != null && lastStatementNode.GetChild(0) is MiniCSharpParser.ReturnStatementContext;

            if (isVoid && (blockIsEmptyOrNull || !lastStmtIsReturn) )
            {
                _ilGenerator.Emit(OpCodes.Ret);
            }
            
            _currentGeneratingMethodSymbol = previousGeneratingMethodSymbol;
            _ilGenerator = previousILGenerator;
            _currentMethodBuilder = previousMethodBuilder;
            return null;
        }

        public override object VisitBlock(MiniCSharpParser.BlockContext context)
        {
            Scope scopeBeforeBlock = _currentCodeGenScope;

            Scope blockCheckerScope = _symbolTable.GetScopeForNode(context); //
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
            return null;
        }
        public override object VisitCharFactor(MiniCSharpParser.CharFactorContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitCharFactor."); return null; }
            string text = context.CHARCONST().GetText(); char val = text.Length >= 3 ? text[1] : '\0';
            _ilGenerator.Emit(OpCodes.Ldc_I4, (int)val); return null;
        }
        public override object VisitStringFactor(MiniCSharpParser.StringFactorContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitStringFactor."); return null; }
            string text = context.STRINGCONST().GetText(); string val = text.Substring(1, text.Length - 2);
            _ilGenerator.Emit(OpCodes.Ldstr, val); return null;
        }
        public override object VisitBoolFactor(MiniCSharpParser.BoolFactorContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitBoolFactor."); return null; }
            _ilGenerator.Emit(context.TRUE() != null ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0); return null;
        }

        public override object VisitWriteStatement(MiniCSharpParser.WriteStatementContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitWriteStatement."); return null; }
            Visit(context.expr());
            Compiladores.Checker.Type exprMiniCSharpType = GetExpressionType(context.expr());
            if (exprMiniCSharpType == Compiladores.Checker.Type.Error) { Console.Error.WriteLine($"CodeGen Error: tipo no resuelto para write: {context.expr().GetText()}"); return null; }
            
            System.Type netType = ResolveNetType(exprMiniCSharpType);
            MethodInfo writeLineMethod = typeof(System.Console).GetMethod("WriteLine", new System.Type[] { netType });
            
            if (writeLineMethod == null)
            {
                if (netType.IsValueType && netType != typeof(void))
                {
                    _ilGenerator.Emit(OpCodes.Box, netType);
                }
                writeLineMethod = typeof(System.Console).GetMethod("WriteLine", new System.Type[] { typeof(object) });
            }
            
            if (writeLineMethod != null)
            {
                _ilGenerator.Emit(OpCodes.Call, writeLineMethod);
            }
            else
            {
                Console.Error.WriteLine($"CodeGen Error: No Console.WriteLine para {netType} (MiniC#: {exprMiniCSharpType}).");
                if (netType != typeof(void) && _ilGenerator.ILOffset > 0) 
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
            
            if (treeNode is MiniCSharpParser.DesignatorContext desCtx)
            {
                string baseName = desCtx.IDENT(0).GetText();
                Symbol sym = _currentCodeGenScope?.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
                if (sym != null && sym.Type != null)
                {
                    ExpressionTypes[treeNode] = sym.Type;
                    return sym.Type;
                }
            }

            Console.Error.WriteLine($"CodeGen Error (GetExpressionType): Type not found in ExpressionTypes and could not be inferred for node '{treeNode.GetText()}' (Type: {treeNode.GetType().Name}). Defaulting to Type.Error.");
            ExpressionTypes[treeNode] = Compiladores.Checker.Type.Error;
            return Compiladores.Checker.Type.Error;
        }

        public override object VisitReturnStatement(MiniCSharpParser.ReturnStatementContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitReturnStatement."); return null; }
            if (context.expr() != null) { Visit(context.expr()); }
            _ilGenerator.Emit(OpCodes.Ret);
            return null;
        }

        // ***** MÉTODO MODIFICADO / NUEVO *****
        public override object VisitDesignatorFactor(MiniCSharpParser.DesignatorFactorContext context)
        {
            if (_ilGenerator == null)
            {
                Console.Error.WriteLine("CodeGen Error (VisitDesignatorFactor): ILGenerator is null.");
                return null;
            }

            var designatorNode = context.designator();

            if (context.LPAREN() == null) // No es una llamada a método, es una variable, campo o acceso a array.
            {
                // Caso 1: Acceso a Array (ej. miArray[indice])
                if (designatorNode.LBRACK().Length > 0) // designator: IDENT (DOT IDENT | LBRACK expr RBRACK)*;
                {
                    // El designador es del tipo IDENT LBRACK expr RBRACK (simplificado para un solo índice)
                    string baseArrayName = designatorNode.IDENT(0).GetText();
                    Symbol baseArraySymbol = _currentCodeGenScope.Find(baseArrayName);

                    if (!(baseArraySymbol is VarSymbol arrayVarSymbol) || !(arrayVarSymbol.Type is ArrayType arrayMiniCSharpType)) //
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Designator '{baseArrayName}' no es un array o no se encontró.");
                        return null; // O emitir ldnull y dejar que falle en runtime
                    }

                    // 1. Cargar la referencia del array en la pila
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
                        if (!fb.IsStatic) _ilGenerator.Emit(OpCodes.Ldarg_0); // 'this'
                        _ilGenerator.Emit(fb.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fb);
                    }
                    else
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): No se encontró almacenamiento para el array '{baseArrayName}'.");
                        return null;
                    }
                    // Pila: [array_ref]

                    // 2. Cargar el índice en la pila
                    Visit(designatorNode.expr(0)); // Asume un solo índice por ahora
                                                 // El checker asegura que expr(0) es int
                    // Pila: [array_ref, index_val]

                    // 3. Emitir Ldelem.*
                    // GetExpressionType(designatorNode) devuelve el TIPO DEL ELEMENTO del array (gracias al checker)
                    Compiladores.Checker.Type elementType = GetExpressionType(designatorNode);
                    OpCode ldElemOpCode = GetLdElemOpCode(elementType);
                    _ilGenerator.Emit(ldElemOpCode);
                    Console.WriteLine($"CodeGen INFO (VisitDesignatorFactor): Emitido {ldElemOpCode} para cargar elemento de array '{baseArrayName}'.");
                    // Pila: [element_value]
                }
                // Caso 2: Designador simple (variable, parámetro, campo)
                else if (designatorNode.DOT().Length == 0) // Es un IDENT simple
                {
                    string varName = designatorNode.IDENT(0).GetText();
                    Symbol symbol = _currentCodeGenScope.Find(varName);

                    if (symbol is VarSymbol varSymbol)
                    {
                        if (varSymbol.Kind == SymbolKind.Constant)
                        {
                            if (varSymbol.Name == "null" && varSymbol.Type.Kind == TypeKind.Null)
                            {
                                _ilGenerator.Emit(OpCodes.Ldnull);
                            }
                            else
                            {
                                Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Carga de constante con nombre '{varSymbol.Name}' no implementada (aparte de 'null').");
                            }
                        }
                        else // Variable o Parámetro
                        {
                            if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lb))
                            {
                                _ilGenerator.Emit(OpCodes.Ldloc, lb);
                            }
                            else if (_currentGeneratingMethodSymbol != null &&
                                     _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == varSymbol.Name && p.Level == varSymbol.Level) is VarSymbol paramSymbol)
                            {
                                int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                                _ilGenerator.Emit(OpCodes.Ldarg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
                            }
                            else if (_fieldBuilders.TryGetValue(varSymbol, out FieldBuilder fb))
                            {
                                if (!fb.IsStatic) _ilGenerator.Emit(OpCodes.Ldarg_0); // 'this'
                                _ilGenerator.Emit(fb.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fb);
                            }
                            else
                            {
                                Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): No se encontró almacenamiento para VarSymbol '{varName}'.");
                            }
                        }
                    }
                    else
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Símbolo '{varName}' no es VarSymbol o no encontrado.");
                    }
                }
                else // Es un designador complejo con DOT (obj.field), no implementado completamente aquí para carga
                {
                    // TODO: Implementar carga para obj.field
                    Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Carga para designador con DOT '{designatorNode.GetText()}' no completamente implementada.");
                     _ilGenerator.Emit(OpCodes.Ldnull); // Dejar algo en la pila para evitar errores de pila posteriores.
                }
            }
            else // Es una llamada a método: designator ( [actPars] )
            {
                HandleMethodCall(designatorNode, context.actPars());
            }
            return null;
        }

        // ***** MÉTODO MODIFICADO / NUEVO *****
        public override object VisitDesignatorStatement(MiniCSharpParser.DesignatorStatementContext context)
        {
            if (_ilGenerator == null)
            {
                Console.Error.WriteLine("CodeGen Error (VisitDesignatorStatement): ILGenerator is null.");
                return null;
            }

            var designatorNode = context.designator();

            if (context.ASSIGN() != null) // Asignación: designator = expr
            {
                // Caso 1: Asignación a elemento de Array (ej. miArray[indice] = expr)
                if (designatorNode.LBRACK().Length > 0) //
                {
                    string baseArrayName = designatorNode.IDENT(0).GetText();
                    Symbol baseArraySymbol = _currentCodeGenScope.Find(baseArrayName);

                    if (!(baseArraySymbol is VarSymbol arrayVarSymbol) || !(arrayVarSymbol.Type is ArrayType arrayMiniCSharpType)) //
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): Designator '{baseArrayName}' no es un array o no se encontró para asignación.");
                        if (context.expr() != null) { Visit(context.expr()); if (GetExpressionType(context.expr()) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
                        return null;
                    }

                    // 1. Cargar la referencia del array en la pila
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
                        if (!fb.IsStatic) _ilGenerator.Emit(OpCodes.Ldarg_0); // 'this'
                        _ilGenerator.Emit(fb.IsStatic ? OpCodes.Ldsfld : OpCodes.Ldfld, fb);
                    }
                    else
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): No se encontró almacenamiento para el array '{baseArrayName}'.");
                        if (context.expr() != null) { Visit(context.expr()); if (GetExpressionType(context.expr()) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
                        return null;
                    }
                    // Pila: [array_ref]

                    // 2. Cargar el índice en la pila
                    Visit(designatorNode.expr(0)); // Asume un solo índice
                    // Pila: [array_ref, index_val]

                    // 3. Evaluar la expresión del lado derecho (RHS) y cargar su valor
                    Visit(context.expr());
                    // Pila: [array_ref, index_val, rhs_value]
                    
                    Compiladores.Checker.Type rhsType = GetExpressionType(context.expr());
                    // GetExpressionType(designatorNode) devuelve el tipo del elemento (T) de array[index]
                    Compiladores.Checker.Type lhsElementType = GetExpressionType(designatorNode);


                    // 4. Coerción de tipos (si es necesario) para el valor del RHS
                    if (lhsElementType == Compiladores.Checker.Type.Double && rhsType == Compiladores.Checker.Type.Int)
                    {
                        _ilGenerator.Emit(OpCodes.Conv_R8);
                    }
                    // Añadir más si es necesario

                    // 5. Emitir Stelem.*
                    OpCode stElemOpCode = GetStElemOpCode(lhsElementType);
                    _ilGenerator.Emit(stElemOpCode);
                    Console.WriteLine($"CodeGen INFO (VisitDesignatorStatement): Emitido {stElemOpCode} para almacenar en elemento de array '{baseArrayName}'.");
                    // Stelem consume array_ref, index_val, y rhs_value
                }
                // Caso 2: Asignación a designador simple (variable, parámetro, campo)
                else if (designatorNode.DOT().Length == 0)
                {
                    string varName = designatorNode.IDENT(0).GetText();
                    Symbol symbol = _currentCodeGenScope.Find(varName);

                    if (!(symbol is VarSymbol varSymbol))
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): VarSymbol para '{varName}' no encontrado para asignación.");
                        if (context.expr() != null) { Visit(context.expr()); if (GetExpressionType(context.expr()) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
                        return null;
                    }

                    FieldBuilder fbAssign = null;
                    bool isStaticFieldAssign = false;

                    if (_fieldBuilders.TryGetValue(varSymbol, out fbAssign))
                    {
                        isStaticFieldAssign = fbAssign.IsStatic;
                        if (!isStaticFieldAssign)
                        {
                            _ilGenerator.Emit(OpCodes.Ldarg_0); // 'this' para Stfld
                        }
                    }
                    
                    Visit(context.expr()); // Valor del RHS en la pila
                    Compiladores.Checker.Type rhsType = GetExpressionType(context.expr());
                    Compiladores.Checker.Type lhsType = varSymbol.Type;

                    if (lhsType == Compiladores.Checker.Type.Double && rhsType == Compiladores.Checker.Type.Int)
                    {
                        _ilGenerator.Emit(OpCodes.Conv_R8);
                    }

                    if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lbAssign))
                    {
                        _ilGenerator.Emit(OpCodes.Stloc, lbAssign);
                    }
                    else if (fbAssign != null)
                    {
                        _ilGenerator.Emit(isStaticFieldAssign ? OpCodes.Stsfld : OpCodes.Stfld, fbAssign);
                    }
                    else if (_currentGeneratingMethodSymbol != null &&
                             _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == varSymbol.Name && p.Level == varSymbol.Level) is VarSymbol paramSymbol)
                    {
                        int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                        _ilGenerator.Emit(OpCodes.Starg, (short)(_currentMethodBuilder.IsStatic ? paramIndex : paramIndex + 1));
                    }
                    else
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): No se encontró almacenamiento para VarSymbol '{varName}' en asignación.");
                    }
                }
                else // Asignación a designador complejo con DOT (obj.field)
                {
                    // TODO: Implementar asignación a obj.field
                    Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): Asignación a designador con DOT '{designatorNode.GetText()}' no implementada.");
                    if (context.expr() != null) { Visit(context.expr()); if (GetExpressionType(context.expr()) != Compiladores.Checker.Type.Void) _ilGenerator.Emit(OpCodes.Pop); }
                }
            }
            else if (context.INC() != null || context.DEC() != null)
            {
                // TODO: Implementar INC/DEC para elementos de array y campos de objeto
                if (designatorNode.LBRACK().Length > 0 || designatorNode.DOT().Length > 0)
                {
                     Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): INC/DEC para designador complejo '{designatorNode.GetText()}' no implementado.");
                }
                else // INC/DEC en variable/parámetro/campo simple (lógica existente)
                {
                    string varName = designatorNode.IDENT(0).GetText();
                    Symbol symbol = _currentCodeGenScope.Find(varName);

                    if (!(symbol is VarSymbol varSymbol))
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): VarSymbol para '{varName}' no encontrado para INC/DEC.");
                        return null;
                    }
                    Compiladores.Checker.Type varType = GetExpressionType(designatorNode);
                    if (varType != Compiladores.Checker.Type.Int && varType != Compiladores.Checker.Type.Double)
                    {
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): INC/DEC solo para int o double, no {varType.Name}.");
                        return null;
                    }

                    // Lógica para cargar, operar y guardar (simplificada, especialmente para campos)
                    // ... (lógica existente para INC/DEC de variables simples) ...
                    // Esta parte es compleja para campos de instancia y requiere un patrón de carga/dup/operación/almacenamiento cuidadoso.
                    // La lógica anterior para esto era una simplificación.
                    Console.Error.WriteLine($"CodeGen INFO (VisitDesignatorStatement): INC/DEC para simple var '{varName}'. Revisar robustez para campos.");
                     // Placeholder: Aquí iría la lógica detallada de carga, operación, guardado para INC/DEC
                     // Para locales: ldloc, ldc.i4.1, add, stloc
                     // Para campos: (ldarg_0), ldfld, ldc.i4.1, add, (ldarg_0), stfld -- el this duplicado es clave
                     // Por ahora, no se emite CIL completo aquí para evitar errores, ya que la lógica anterior era incompleta.
                }
            }
            else if (context.LPAREN() != null) // Llamada a método como statement
            {
                HandleMethodCall(designatorNode, context.actPars());
                // Pop del valor de retorno si el método no es void
                Symbol calledSymbol = ResolveDesignatorToCallableSymbol(designatorNode);
                if (calledSymbol is MethodSymbol calledMethodSym)
                {
                    if (calledMethodSym.Type != Compiladores.Checker.Type.Void)
                    {
                        _ilGenerator.Emit(OpCodes.Pop);
                    }
                } else {
                     var factorType = GetExpressionType(designatorNode.Parent); 
                     if (factorType != null && factorType != Compiladores.Checker.Type.Void && factorType != Compiladores.Checker.Type.Error) {
                        _ilGenerator.Emit(OpCodes.Pop);
                     } else if (factorType == null) {
                        //Console.Error.WriteLine($"CodeGen Warning (VisitDesignatorStatement): Could not determine return type for method call '{designatorNode.GetText()}' used as statement to decide on Pop.");
                     }
                }
            }
            return null;
        }

        // ***** NUEVOS MÉTODOS HELPER *****
        private OpCode GetLdElemOpCode(Compiladores.Checker.Type elementType)
        {
            System.Type netElementType = ResolveNetType(elementType);
            if (netElementType == typeof(int)) return OpCodes.Ldelem_I4;
            if (netElementType == typeof(double)) return OpCodes.Ldelem_R8;
            if (netElementType == typeof(char)) return OpCodes.Ldelem_U2; // Char se carga como unsigned 2 bytes -> int
            if (netElementType == typeof(bool)) return OpCodes.Ldelem_I1; // Bool se carga como signed 1 byte -> int
            if (!netElementType.IsValueType || netElementType.IsGenericParameter) return OpCodes.Ldelem_Ref; // Para strings y otros tipos referencia

            Console.Error.WriteLine($"CodeGen Warning (GetLdElemOpCode): No se encontró LDELEM específico para tipo {netElementType.FullName}. Usando Ldelem_Ref por defecto.");
            return OpCodes.Ldelem_Ref; // Fallback genérico
        }

        private OpCode GetStElemOpCode(Compiladores.Checker.Type elementType)
        {
            System.Type netElementType = ResolveNetType(elementType);
            if (netElementType == typeof(int)) return OpCodes.Stelem_I4;
            if (netElementType == typeof(double)) return OpCodes.Stelem_R8;
            if (netElementType == typeof(char)) return OpCodes.Stelem_I2; // Char se almacena como 2 bytes
            if (netElementType == typeof(bool)) return OpCodes.Stelem_I1; // Bool se almacena como 1 byte
            if (!netElementType.IsValueType || netElementType.IsGenericParameter) return OpCodes.Stelem_Ref;

            Console.Error.WriteLine($"CodeGen Warning (GetStElemOpCode): No se encontró STELEM específico para tipo {netElementType.FullName}. Usando Stelem_Ref por defecto.");
            return OpCodes.Stelem_Ref;
        }


        // ... (resto de los métodos de Visit* y helpers como HandleMethodCall, ResolveDesignatorToCallableSymbol, etc. sin cambios respecto a la última versión que te di)
        // ... Asegúrate de que la lógica de HandleMethodCall, ResolveDesignatorToCallableSymbol, VisitExpr, VisitTerm, etc.,
        // ... así como los métodos de control de flujo (VisitIfStatement, VisitWhileStatement) y otros literales
        // ... permanezcan como estaban en la versión anterior que funcionaba con el error CS1503 corregido.
        // ... Solo los métodos VisitDesignatorFactor, VisitDesignatorStatement y los nuevos GetLdElemOpCode/GetStElemOpCode se añaden/modifican aquí.

        // Copia aquí el resto de los métodos de la clase MiniCSharpCodeGenerator
        // que te proporcioné en la respuesta anterior y que no se han modificado
        // en este paso. Esto incluye:
        // HandleMethodCall, ResolveDesignatorToCallableSymbol, VisitExpr, VisitTerm,
        // VisitCondition, VisitCondTerm, VisitCondFact, VisitIfStatement, VisitWhileStatement,
        // VisitNewFactor (que ya modificamos para new arr[] y new Class()), etc.

        private void HandleMethodCall(MiniCSharpParser.DesignatorContext designatorCtx, MiniCSharpParser.ActParsContext actParsCtx)
        {
            MethodSymbol resolvedMethodSymbol = ResolveDesignatorToCallableSymbol(designatorCtx);
            MethodBase methodToCall = null; 

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
                    //Console.Error.WriteLine($"CodeGen Info (ResolveDesignatorToCallableSymbol): Identifier '{methodName}' found but it is not a method (Kind: {symbol.Kind}). This might be a variable.");
                    return null; // No es un método
                }
                Symbol globalSymbol = _symbolTable.SearchGlobal(methodName);
                 if (globalSymbol is MethodSymbol globalMethodSymbol)
                {
                    return globalMethodSymbol;
                }

                //Console.Error.WriteLine($"CodeGen Info (ResolveDesignatorToCallableSymbol): Method '{methodName}' not found in current or global scope. This might be a variable.");
                return null; // No encontrado o no es método
            }

            if (designatorCtx.DOT().Length > 0)
            {
                // Caso: IDENT(0).IDENT(1)
                // IDENT(0) puede ser una clase (para llamada estática) o una instancia (para llamada de instancia)
                string baseIdentifierName = designatorCtx.IDENT(0).GetText();
                string memberMethodName = designatorCtx.IDENT(1).GetText(); 

                Symbol baseSymbol = _currentCodeGenScope.Find(baseIdentifierName) ?? _symbolTable.SearchGlobal(baseIdentifierName);

                if (baseSymbol is ClassSymbol classSymbol) // Ej: Math.Max()
                {
                    if (classSymbol.Type is ClassType classType) //
                    {
                        Symbol memberSymbol = classType.Members.FindCurrent(memberMethodName); 
                        if (memberSymbol is MethodSymbol methodSymbol)
                        {
                            // TODO: Verificar si el método es estático, ya que se está llamando como Clase.Metodo()
                            return methodSymbol;
                        }
                    }
                }
                else if (baseIdentifierName == "Console") 
                {
                    return null; // Deja que HandleMethodCall lo maneje por reflexión
                }
                else if (baseSymbol is VarSymbol varSym) // Ej: myObj.DoSomething()
                {
                     // Necesitamos el tipo de varSym para encontrar el método miembro
                     if (varSym.Type is ClassType instanceClassType) //
                     {
                         Symbol memberSymbol = instanceClassType.Members.FindCurrent(memberMethodName);
                         if (memberSymbol is MethodSymbol methodSymbol)
                         {
                             // TODO: Verificar si el método NO es estático
                             return methodSymbol;
                         }
                     }
                }
                Console.Error.WriteLine($"CodeGen Error (ResolveDesignatorToCallableSymbol): Cannot resolve method '{memberMethodName}' on base '{baseIdentifierName}'. Base is not a known class or object with members.");
                return null;
            }
            
            // Otros casos complejos de designador (ej. array[idx].Method()) no están completamente soportados aquí.
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
                        if (currentLeftType == Compiladores.Checker.Type.Int) 
                        {
                            LocalBuilder tempDouble = _ilGenerator.DeclareLocal(typeof(double));
                            _ilGenerator.Emit(OpCodes.Stloc, tempDouble);    
                            _ilGenerator.Emit(OpCodes.Conv_R8);              
                            _ilGenerator.Emit(OpCodes.Ldloc, tempDouble);    
                        }
                        else if (rightTermType == Compiladores.Checker.Type.Int) 
                        {
                            _ilGenerator.Emit(OpCodes.Conv_R8); 
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
                    MethodInfo concatMethod = typeof(string).GetMethod("Concat", new[] { typeof(string), typeof(string) });
                    
                    if (currentLeftType != Compiladores.Checker.Type.String) { 
                        EmitToStringCoercion(currentLeftType); 
                    }
                    if (rightTermType != Compiladores.Checker.Type.String) { 
                        EmitToStringCoercion(rightTermType); 
                    }

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
        
        private void EmitToStringCoercion(Compiladores.Checker.Type typeToConvert) {
            System.Type netType = ResolveNetType(typeToConvert);
            if (netType.IsValueType) {
                LocalBuilder tempVal = _ilGenerator.DeclareLocal(netType);
                _ilGenerator.Emit(OpCodes.Stloc, tempVal); // Guardar valor
                _ilGenerator.Emit(OpCodes.Ldloca_S, tempVal); // Cargar dirección del local
                 MethodInfo toStringMethod = netType.GetMethod("ToString", System.Type.EmptyTypes);
                 if (toStringMethod == null || toStringMethod.DeclaringType == typeof(object) || toStringMethod.DeclaringType == typeof(ValueType)) {
                    // Si no hay un ToString específico o es el de Object/ValueType, boxear primero
                     _ilGenerator.Emit(OpCodes.Box, netType); // Boxear (si ldloca no lo hizo implícitamente para callvirt)
                     toStringMethod = typeof(object).GetMethod("ToString"); // Usar el de object
                     _ilGenerator.Emit(OpCodes.Callvirt, toStringMethod);
                 } else {
                    // Si hay un ToString específico en el tipo valor (struct)
                    _ilGenerator.Emit(OpCodes.Call, toStringMethod); // Llamar directamente
                 }


            } else if (netType == typeof(string)) {
                // Ya es string, no hacer nada.
            } else { 
                MethodInfo toStringMethod = netType.GetMethod("ToString", System.Type.EmptyTypes) ?? typeof(object).GetMethod("ToString");
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
                        if (currentLeftType == Compiladores.Checker.Type.Int) 
                        {
                            LocalBuilder tempDouble = _ilGenerator.DeclareLocal(typeof(double));
                            _ilGenerator.Emit(OpCodes.Stloc, tempDouble);    
                            _ilGenerator.Emit(OpCodes.Conv_R8);              
                            _ilGenerator.Emit(OpCodes.Ldloc, tempDouble);    
                        }
                        else if (rightFactorType == Compiladores.Checker.Type.Int) 
                        {
                            _ilGenerator.Emit(OpCodes.Conv_R8); 
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

            if (netLeftType == typeof(double) && netRightType == typeof(int))
            {
                _ilGenerator.Emit(OpCodes.Conv_R8);
            }
            else if (netLeftType == typeof(int) && netRightType == typeof(double))
            {
                LocalBuilder tempDouble = _ilGenerator.DeclareLocal(typeof(double));
                _ilGenerator.Emit(OpCodes.Stloc, tempDouble); 
                _ilGenerator.Emit(OpCodes.Conv_R8);          
                _ilGenerator.Emit(OpCodes.Ldloc, tempDouble); 
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

            _breakLabelStack.Push(endWhileLabel); // Para 'break' statements

            _ilGenerator.MarkLabel(conditionLabel);
            Visit(context.condition()); // Deja bool en la pila
            _ilGenerator.Emit(OpCodes.Brfalse, endWhileLabel);  // Si es false, salir del bucle
    
            Visit(context.statement()); // Ejecutar cuerpo del bucle
    
            _ilGenerator.Emit(OpCodes.Br, conditionLabel);  // Volver a la condición

            _ilGenerator.MarkLabel(endWhileLabel);
            _breakLabelStack.Pop(); // Remover el target de break para este bucle

            return null;
        }
        
        public override object VisitNewFactor(MiniCSharpParser.NewFactorContext context)
        {
            if (_ilGenerator == null)
            {
                Console.Error.WriteLine("CodeGen Error (VisitNewFactor): ILGenerator is null.");
                return null;
            }
            
            Compiladores.Checker.Type newMiniCSharpType = GetExpressionType(context);
            if (newMiniCSharpType == Compiladores.Checker.Type.Error)
            {
                Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): No se pudo determinar el tipo para 'new {context.IDENT().GetText()}'.");
                return null;
            }

            if (newMiniCSharpType.Kind == TypeKind.Array) 
            {
                ArrayType arrayMiniCSharpType = (ArrayType)newMiniCSharpType; //
                
                Visit(context.expr()); 
                                       
                System.Type netElementType = ResolveNetType(arrayMiniCSharpType.ElementType);
                if (netElementType == null || netElementType == typeof(object) && arrayMiniCSharpType.ElementType != Compiladores.Checker.Type.Null && arrayMiniCSharpType.ElementType != Compiladores.Checker.Type.Error)
                {
                     Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): No se pudo resolver el tipo de elemento .NET para el array '{arrayMiniCSharpType.ElementType.Name}'.");
                     _ilGenerator.Emit(OpCodes.Pop); 
                     _ilGenerator.Emit(OpCodes.Ldnull); 
                     return null;
                }
                
                _ilGenerator.Emit(OpCodes.Newarr, netElementType);
                Console.WriteLine($"CodeGen INFO (VisitNewFactor): Emitido Newarr para tipo {netElementType.FullName}[].");
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

                if (netClassType.IsAbstract || netClassType.IsInterface) {
                     Console.Error.WriteLine($"CodeGen Error (VisitNewFactor): No se puede instanciar tipo abstracto o interfaz '{netClassType.FullName}'.");
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
                Console.WriteLine($"CodeGen INFO (VisitNewFactor): Emitido Newobj para clase {netClassType.FullName}.");
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
    System.Reflection.Emit.Label incrementLabel = _ilGenerator.DefineLabel(); // Usada si hay statement de incremento
    System.Reflection.Emit.Label bodyLabel = _ilGenerator.DefineLabel();
    System.Reflection.Emit.Label endForLabel = _ilGenerator.DefineLabel();

    _breakLabelStack.Push(endForLabel); // Para 'break' statements

    // La gramática es: FOR LPAREN initExpr=expr? SEMI cond=condition? SEMI iterStmtRule=statement? RPAREN bodyStmtRule=statement
    var initExprCtx = context.expr();
    var conditionCtx = context.condition();
    
    MiniCSharpParser.StatementContext iterStatementCtx = null;
    MiniCSharpParser.StatementContext bodyStatementCtx = null;

    // ANTLR agrupa los 'statement' que coinciden.
    // statement() devuelve un array. Si hay dos (iterador y cuerpo), context.statement(0) es el iterador y context.statement(1) el cuerpo.
    // Si solo hay uno (cuerpo), context.statement(0) es el cuerpo.
    var statementsInRule = context.statement();
    if (statementsInRule.Length == 1) { // Solo el statement del cuerpo obligatorio
        bodyStatementCtx = statementsInRule[0];
    } else if (statementsInRule.Length == 2) { // El statement opcional de iteración y el statement del cuerpo
        iterStatementCtx = statementsInRule[0]; // El statement? dentro de los paréntesis
        bodyStatementCtx = statementsInRule[1]; // El statement después de los paréntesis
    } else if (statementsInRule.Length == 0 && context.children.LastOrDefault(c => c is MiniCSharpParser.StatementContext) is MiniCSharpParser.StatementContext lastChild) {
        // Fallback si ANTLR no los agrupa en el array `statement()` como se espera para opcionales (poco probable con `?` en la regla)
        bodyStatementCtx = lastChild; // Asumir que el último hijo Statement es el cuerpo
    }
     else if (statementsInRule.Length == 0 ) {
         // Esto no debería ocurrir si la gramática es `statement` (no opcional) para el cuerpo.
         Console.Error.WriteLine("CodeGen Error (VisitForStatement): No se encontró el statement del cuerpo para el bucle FOR.");
         _breakLabelStack.Pop();
         return null;
    }


    // 1. Inicialización (expr?)
    if (initExprCtx != null)
    {
        Visit(initExprCtx);
        Compiladores.Checker.Type initType = GetExpressionType(initExprCtx); //
        if (initType != Compiladores.Checker.Type.Void && initType != Compiladores.Checker.Type.Error)
        {
            _ilGenerator.Emit(OpCodes.Pop); // Pop si la expresión de inicialización deja un valor no utilizado
        }
    }

    _ilGenerator.Emit(OpCodes.Br, conditionLabel); // Saltar a la comprobación de condición primero

    // 2. Cuerpo del bucle
    _ilGenerator.MarkLabel(bodyLabel);
    if (bodyStatementCtx != null)
    {
        Visit(bodyStatementCtx);
    }
    else 
    {
        // Si bodyStatementCtx es null aquí, hay un problema con la lógica de obtención de statements o la gramática.
        Console.Error.WriteLine("CodeGen Error (VisitForStatement): El contexto del cuerpo del bucle FOR es nulo.");
    }


    // 3. Statement de incremento/iteración
    _ilGenerator.MarkLabel(incrementLabel);
    if (iterStatementCtx != null)
    {
        Visit(iterStatementCtx); // Es un statement, maneja su propia pila
    }

    // 4. Comprobación de condición
    _ilGenerator.MarkLabel(conditionLabel);
    if (conditionCtx != null)
    {
        Visit(conditionCtx); // Deja bool en la pila
        _ilGenerator.Emit(OpCodes.Brfalse, endForLabel); // Si la condición es false, saltar al final
    }
    // Si la condición es true (o no hay condición), saltar al cuerpo
    _ilGenerator.Emit(OpCodes.Br, bodyLabel);

    // 5. Fin del bucle
    _ilGenerator.MarkLabel(endForLabel);
    _breakLabelStack.Pop();

    return null;
}
        public override object VisitSwitchStatement(MiniCSharpParser.SwitchStatementContext context)
{
    if (_ilGenerator == null) { Console.Error.WriteLine("CodeGen Error (VisitSwitchStatement): ILGenerator is null."); return null; }

    System.Reflection.Emit.Label endSwitchLabel = _ilGenerator.DefineLabel();
    _breakLabelStack.Push(endSwitchLabel);

    Visit(context.expr()); // Evaluar expresión del switch, deja valor en la pila
    Compiladores.Checker.Type switchExprType = GetExpressionType(context.expr()); //
    System.Type netSwitchExprType = ResolveNetType(switchExprType); //

    // Almacenar el valor de la expresión del switch en una variable local
    LocalBuilder switchValueLocal = _ilGenerator.DeclareLocal(netSwitchExprType);
    _ilGenerator.Emit(OpCodes.Stloc, switchValueLocal);

    var caseContexts = context.switchCase();
    System.Reflection.Emit.Label[] caseTargetLabels = new System.Reflection.Emit.Label[caseContexts.Length];
    for (int i = 0; i < caseContexts.Length; i++)
    {
        caseTargetLabels[i] = _ilGenerator.DefineLabel();
    }

    System.Reflection.Emit.Label defaultTargetLabel = _ilGenerator.DefineLabel(); 

    // Generar lógica de despacho (comparaciones y saltos)
    for (int i = 0; i < caseContexts.Length; i++)
    {
        _ilGenerator.Emit(OpCodes.Ldloc, switchValueLocal); // Cargar el valor del switch
        Visit(caseContexts[i].constant()); // Cargar el valor constante del case
        // El checker debería asegurar que los tipos son compatibles para comparación (int o char)
        _ilGenerator.Emit(OpCodes.Beq, caseTargetLabels[i]); // Si son iguales, saltar al target del case
    }

    // Si ningún case coincide, saltar a default o al final del switch
    if (context.defaultCase() != null)
    {
        _ilGenerator.Emit(OpCodes.Br, defaultTargetLabel);
    }
    else
    {
        _ilGenerator.Emit(OpCodes.Br, endSwitchLabel);
    }

    // Generar código para cada bloque case
    for (int i = 0; i < caseContexts.Length; i++)
    {
        _ilGenerator.MarkLabel(caseTargetLabels[i]);
        Visit(caseContexts[i]); // Visitar statements dentro del case
                                // El 'break' dentro de Visit(caseContexts[i]) saltará a endSwitchLabel
    }

    // Generar código para el bloque default (si existe)
    _ilGenerator.MarkLabel(defaultTargetLabel);
    if (context.defaultCase() != null)
    {
        Visit(context.defaultCase()); // Visitar statements del default
    }
    // Si no hubo un caso default explícito y los cases anteriores no tuvieron break,
    // la ejecución continuará aquí y luego al endSwitchLabel.

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
                // Esto debería ser detectado por el analizador semántico.
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
            // Visitar los statements dentro del case.
            // El comportamiento de fall-through es natural a menos que se encuentre un 'break'.
            foreach (var stmtCtx in context.statement())
            {
                Visit(stmtCtx);
            }
            // MiniCSharp, al igual que C#, requeriría un 'break' para evitar el fall-through explícito
            // a menos que el case esté vacío. El 'break' es manejado por VisitBreakStatement.
            // Si no hay break y el case no está vacío, en C# es un error.
            // Aquí, si no hay break, simplemente se ejecutará el siguiente bloque de case si el IL lo permite (lo cual hace).
            return null;
        }
        public override object VisitDefaultCase(MiniCSharpParser.DefaultCaseContext context)
        {
            // Visitar los statements dentro del case default.
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
        public override object VisitParenFactor(MiniCSharpParser.ParenFactorContext context) { return Visit(context.expr()); } 
        
        public override object VisitDesignator(MiniCSharpParser.DesignatorContext context) { return base.VisitDesignator(context); } 
        
        public override object VisitNumber(MiniCSharpParser.NumberContext context) { 
            if (context.INTCONST() != null) _ilGenerator.Emit(OpCodes.Ldc_I4, int.Parse(context.INTCONST().GetText()));
            else if (context.DOUBLECONST() != null) _ilGenerator.Emit(OpCodes.Ldc_R8, double.Parse(context.DOUBLECONST().GetText()));
            return null;
        }
        
        public override object VisitRelop(MiniCSharpParser.RelopContext context) { return base.VisitRelop(context); }
        public override object VisitAddop(MiniCSharpParser.AddopContext context) { return base.VisitAddop(context); }
        public override object VisitMulop(MiniCSharpParser.MulopContext context) { return base.VisitMulop(context); }

    }
}