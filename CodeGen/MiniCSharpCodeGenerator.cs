// En Compiladores/CodeGen/MiniCSharpCodeGenerator.cs
// En Compiladores/CodeGen/MiniCSharpCodeGenerator.cs
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

        // NUEVO: Para almacenar el tipo de los nodos de expresión (NECESITAS LLENAR ESTO EN TU CHECKER)
        // O tener un método en _symbolTable o en el checker para obtenerlo.
        public Dictionary<IParseTree, Compiladores.Checker.Type> ExpressionTypes { get; }


        public MiniCSharpCodeGenerator(TablaSimbolos symbolTable, string sourceFilePath, string assemblyName, Dictionary<IParseTree, Compiladores.Checker.Type> expressionTypes)
        {
            _symbolTable = symbolTable;
            // _sourceFilePath = sourceFilePath;
            _assemblyNameBase = assemblyName; 
            ExpressionTypes = expressionTypes; // <<< GUARDAR EL DICCIONARIO
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
                return mainTypeBuilder; 
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
                    return elementType.MakeArrayType();
                case TypeKind.Class:
                    var classSymbol = _symbolTable.SearchGlobal(miniCSharpType.Name) as ClassSymbol;
                    if (classSymbol != null && _definedTypes.TryGetValue(classSymbol, out TypeBuilder tb))
                    { return tb; }
                    if (miniCSharpType.Name == "Console") return typeof(System.Console);
                    Console.WriteLine($"Advertencia: No se pudo resolver el tipo .NET para la clase '{miniCSharpType.Name}'. Usando 'object'.");
                    return typeof(object);
                default:
                    throw new ArgumentOutOfRangeException($"Tipo MiniCSharp no soportado para conversión: {miniCSharpType.Kind}");
            }
        }

        public override object VisitProgram(MiniCSharpParser.ProgramContext context)
        {
            _symbolTable.SetCurrentScopeTo(_symbolTable.GetGlobalScope(), _symbolTable.GetGlobalScopeLevel());
            _currentCodeGenScope = _symbolTable.GetGlobalScope();
            string className = context.IDENT().GetText();
            ClassSymbol progClassSymbol = _symbolTable.SearchGlobal(className) as ClassSymbol;
            if (progClassSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No ClassSymbol para '{className}'."); return null; }
            _currentTypeBuilder = _moduleBuilder.DefineType(className, TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.BeforeFieldInit, typeof(object));
            if (!_definedTypes.TryAdd(progClassSymbol, _currentTypeBuilder)) { _definedTypes[progClassSymbol] = _currentTypeBuilder; }
            Scope classScope = (progClassSymbol.Type as ClassType)?.Members;
            if (classScope == null) { Console.Error.WriteLine($"CodeGen Error: ClassSymbol '{className}' no tiene Members."); return null; }
            Scope outerScope = _currentCodeGenScope;
            _currentCodeGenScope = classScope;
            foreach (var varDeclContext in context.varDecl()) Visit(varDeclContext);
            foreach (var classDeclContext in context.classDecl()) Visit(classDeclContext);
            foreach (var methodDeclContext in context.methodDecl()) Visit(methodDeclContext);
            _currentCodeGenScope = outerScope;
            _currentTypeBuilder.CreateType(); 
            return null;
        }
        
        public override object VisitClassDecl(MiniCSharpParser.ClassDeclContext context)
        {
            string className = context.IDENT().GetText();
            ClassSymbol classSymbol = _currentCodeGenScope.FindCurrent(className) as ClassSymbol; 
            if (classSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No ClassSymbol para clase anidada '{className}'."); return null; }
            TypeBuilder outerTypeBuilder = _currentTypeBuilder; 
            _currentTypeBuilder = outerTypeBuilder.DefineNestedType(className, TypeAttributes.NestedPublic | TypeAttributes.Class | TypeAttributes.BeforeFieldInit | TypeAttributes.Sealed, typeof(object));
            if (!_definedTypes.TryAdd(classSymbol, _currentTypeBuilder)) { _definedTypes[classSymbol] = _currentTypeBuilder; }
            Scope outerScope = _currentCodeGenScope;
            _currentCodeGenScope = (classSymbol.Type as ClassType)?.Members;
            if (_currentCodeGenScope == null) { Console.Error.WriteLine($"CodeGen Error: ClassSymbol anidado '{className}' no tiene Members."); _currentTypeBuilder = outerTypeBuilder; return null; }
            foreach (var varDeclContext in context.varDecl()) Visit(varDeclContext);
            _currentCodeGenScope = outerScope;
            _currentTypeBuilder.CreateType(); 
            _currentTypeBuilder = outerTypeBuilder; 
            return null;
        }

        // En MiniCSharpCodeGenerator.cs

// ... (otros campos, constructor, y métodos Visit* como GenerateAssemblyAndGetMainType, ResolveNetType, VisitProgram, VisitClassDecl, etc.) ...

// Asegúrate de que VisitMethodDecl y VisitBlock configuren _currentCodeGenScope correctamente ANTES de llamar a este método.
public override object VisitVarDecl(MiniCSharpParser.VarDeclContext context)
{
    // 1. Obtener el tipo MiniCSharp de la declaración (ej. Type.Int, Type.String, etc.)
    //    Esto llama a VisitType, que debería devolver tu Compiladores.Checker.Type
    Compiladores.Checker.Type varMiniCSharpType = (Compiladores.Checker.Type)Visit(context.type()); 
    
    if (varMiniCSharpType == Compiladores.Checker.Type.Error) 
    { 
        Console.Error.WriteLine($"CodeGen Error (VisitVarDecl): No se pudo resolver el tipo para la variable que comienza con '{context.IDENT(0).GetText()}'."); 
        return null; 
    }
    // Convertir el tipo MiniCSharp a un System.Type de .NET para usar con Reflection.Emit
    System.Type varNetType = ResolveNetType(varMiniCSharpType);

    // Iterar sobre cada identificador en la declaración (ej. int a, b, c;)
    foreach (var identNode in context.IDENT())
    {
        string varName = identNode.GetText();

        // Buscar el VarSymbol en el _currentCodeGenScope.
        // _currentCodeGenScope DEBE haber sido establecido por el método llamador 
        // (VisitBlock para locales, VisitProgram/VisitClassDecl para campos)
        // al scope correcto que el Checker pobló.
        // Usamos FindCurrent porque una VarDecl siempre declara en el scope más interno.
        VarSymbol varSymbol = _currentCodeGenScope.FindCurrent(varName) as VarSymbol;

        if (varSymbol == null) 
        { 
            // Si esto sucede, significa que _currentCodeGenScope no es el scope donde el Checker
            // insertó este VarSymbol, o el Checker no lo insertó.
            Console.Error.WriteLine($"CodeGen Error (VisitVarDecl): No se encontró VarSymbol para '{varName}' en el scope actual (HashCode: {_currentCodeGenScope?.GetHashCode()}).");
            if(_currentCodeGenScope != null) {
                Console.Error.WriteLine($"Símbolos presentes en este scope del CodeGen ({_currentCodeGenScope.GetHashCode()}): " + string.Join(", ", _currentCodeGenScope.Symbols.Keys));
            }
            // Para depurar más, podrías intentar buscar en el scope padre:
            // Symbol foundInOuter = _currentCodeGenScope?.Outer?.FindCurrent(varName);
            // if (foundInOuter != null) Console.Error.WriteLine($"'{varName}' fue encontrado en el scope padre del CodeGen.");
            continue; 
        }

        if (_currentMethodBuilder != null) // Estamos dentro de un método, así que es una variable local
        {
            if (_ilGenerator == null) 
            { 
                Console.Error.WriteLine($"Error Crítico (VisitVarDecl): ILGenerator es null al declarar variable local '{varName}'."); 
                return null; 
            }
            LocalBuilder lb = _ilGenerator.DeclareLocal(varNetType);
            _localBuilders[varSymbol] = lb; // Mapear el VarSymbol al LocalBuilder
            Console.WriteLine($"CodeGen INFO (VisitVarDecl): Declarado LocalBuilder para '{varName}' (VarSymbol Hash: {varSymbol.GetHashCode()}, LocalBuilder Index: {lb.LocalIndex}, Scope Hash: {_currentCodeGenScope.GetHashCode()})");
        }
        else // No estamos dentro de un método, así que es un campo de clase
        {
            FieldAttributes attributes = FieldAttributes.Public; 
            // Si la clase actual es la clase principal del programa (no una anidada), sus campos son estáticos.
            if (_currentTypeBuilder != null && _programContext != null && _currentTypeBuilder.Name == _programContext.IDENT().GetText()) 
            { 
                attributes |= FieldAttributes.Static; 
            }
            // Para clases anidadas, los campos son de instancia por defecto según esta lógica.

            FieldBuilder fb = _currentTypeBuilder.DefineField(varName, varNetType, attributes);
            _fieldBuilders[varSymbol] = fb; // Mapear el VarSymbol al FieldBuilder
            Console.WriteLine($"CodeGen INFO (VisitVarDecl): Definido FieldBuilder para '{varName}' (Estático: {fb.IsStatic})");
        }
    }
    return null;
}

// ... (El resto de tus métodos Visit* en MiniCSharpCodeGenerator.cs) ...
        
        
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

        // En MiniCSharpCodeGenerator.cs

// ... (otros using y campos de la clase sin cambios) ...

        // En MiniCSharpCodeGenerator.cs

// ... (otros campos y métodos using de la clase sin cambios) ...

public override object VisitMethodDecl(MiniCSharpParser.MethodDeclContext context)
{
    string methodName = context.IDENT().GetText();
    // _currentCodeGenScope es el scope de la clase en este punto.
    // Buscamos el MethodSymbol en el scope de la clase.
    MethodSymbol methodSymbol = _currentCodeGenScope.FindCurrent(methodName) as MethodSymbol;

    if (methodSymbol == null) { 
        Console.Error.WriteLine($"CodeGen Error: No se encontró MethodSymbol para '{methodName}' en el scope de la clase '{_currentTypeBuilder?.Name}'."); 
        return null; 
    }

    // Guardar estado del generador antes de procesar este método
    MethodSymbol previousGeneratingMethodSymbol = _currentGeneratingMethodSymbol;
    MethodBuilder previousMethodBuilder = _currentMethodBuilder; 
    ILGenerator previousILGenerator = _ilGenerator;
    
    _currentGeneratingMethodSymbol = methodSymbol; // Establecer el MethodSymbol actual

    // Resolver tipos para la firma del método en .NET
    System.Type returnType = ResolveNetType(methodSymbol.Type);
    List<System.Type> paramTypesList = methodSymbol.Parameters.Select(p => ResolveNetType(p.Type)).ToList();
    System.Type[] paramTypes = paramTypesList.ToArray();

    MethodAttributes attributes = MethodAttributes.Public;
    if (methodName == "Main" && _currentTypeBuilder != null && _programContext != null && _currentTypeBuilder.Name == _programContext.IDENT().GetText())
    { 
        attributes |= MethodAttributes.Static; 
    }

    _currentMethodBuilder = _currentTypeBuilder.DefineMethod(methodName, attributes, returnType, paramTypes);
    _methodBuilders[methodSymbol] = _currentMethodBuilder; // Mapear Symbol a MethodBuilder

    // Definir nombres de parámetros en el MethodBuilder (para depuración/reflexión)
    for (int i = 0; i < methodSymbol.Parameters.Count; i++)
    { 
        _currentMethodBuilder.DefineParameter(i + 1, ParameterAttributes.None, methodSymbol.Parameters[i].Name); 
    }

    _ilGenerator = _currentMethodBuilder.GetILGenerator();
    
    // --- MANEJO DE SCOPE MEJORADO ---
    Scope classScopeForCodeGen = _currentCodeGenScope; // Guardar el scope de la clase actual del CodeGen
    int originalLevelInSymbolTable = _symbolTable.CurrentLevel; // Guardar el nivel actual de la TablaSimbolos

    // Obtener el scope que el Checker asoció con este MethodDeclContext.
    // Este scope ya contiene los parámetros del método.
    Scope methodScopeFromChecker = _symbolTable.GetScopeForNode(context);

    if (methodScopeFromChecker == null) {
        Console.Error.WriteLine($"CodeGen CRITICAL Error: No se encontró el scope del checker para el método '{methodName}'. Las variables locales y parámetros no se resolverán.");
        // Si esto ocurre, es un problema grave en la fase del Checker o en TablaSimbolos.
        // Por ahora, no podemos continuar de forma segura con este método.
        // Restaurar y salir.
        _ilGenerator = previousILGenerator;
        _currentMethodBuilder = previousMethodBuilder;
        _currentGeneratingMethodSymbol = previousGeneratingMethodSymbol;
        return null;
    }
    
    // Establecer el _currentCodeGenScope del generador al scope obtenido del checker.
    _currentCodeGenScope = methodScopeFromChecker;
    
    // Sincronizar el estado interno de _symbolTable para que coincida con este scope y su nivel.
    // El nivel del scope de un método es el nivel de sus parámetros/variables locales de más alto nivel.
    // Los parámetros tienen su 'Level' asignado por el Checker cuando fueron insertados.
    int methodScopeLevel = methodSymbol.Parameters.Any() ? methodSymbol.Parameters[0].Level : (originalLevelInSymbolTable + 1);
    _symbolTable.SetCurrentScopeTo(methodScopeFromChecker, methodScopeLevel);
    // Ya NO es necesario re-insertar parámetros aquí porque methodScopeFromChecker ya los tiene.
    // --- FIN MANEJO DE SCOPE ---
    
    Visit(context.block()); // VisitBlock ahora usará el _currentCodeGenScope (scope del método)
                            // y DENTRO de VisitBlock, se obtendrá el scope específico del bloque.
    
    // --- RESTAURACIÓN DE SCOPE de TablaSimbolos y CodeGen ---
    _symbolTable.SetCurrentScopeTo(classScopeForCodeGen, originalLevelInSymbolTable); // Restaurar estado interno de TablaSimbolos
    _currentCodeGenScope = classScopeForCodeGen; // Restaurar el scope del CodeGen al de la clase.
    // --- FIN RESTAURACIÓN DE SCOPE ---

    // Emitir Ret si es necesario
    bool isVoid = (returnType == typeof(void));
    var stmts = context.block().statement(); // Obtener lista de statements
    bool blockIsEmptyOrNull = stmts == null || !stmts.Any(); // Chequeo más robusto
    var lastStatementNode = stmts?.LastOrDefault(); // Obtener el último statement de forma segura
    bool lastStmtIsReturn = lastStatementNode != null && lastStatementNode.GetChild(0) is MiniCSharpParser.ReturnStatementContext;

    if (isVoid && (blockIsEmptyOrNull || !lastStmtIsReturn) ) 
    { 
        _ilGenerator.Emit(OpCodes.Ret); 
    }
    // Para métodos no-void, el checker semántico ya debió asegurar que todos los caminos tienen un return.
    // Si no, el CIL generado será verificable pero incorrecto, o el verificador de CIL podría fallar.
    
    // Restaurar estado del generador para el método
    _currentGeneratingMethodSymbol = previousGeneratingMethodSymbol; 
    _ilGenerator = previousILGenerator; 
    _currentMethodBuilder = previousMethodBuilder;
    return null;
}

public override object VisitBlock(MiniCSharpParser.BlockContext context)
{
    Scope scopeBeforeBlock = _currentCodeGenScope; 

    // --- OBTENER EL SCOPE DEL BLOQUE CREADO POR EL CHECKER ---
    Scope blockCheckerScope = _symbolTable.GetScopeForNode(context);
    if (blockCheckerScope == null) {
        Console.Error.WriteLine($"CodeGen Error: No se encontró el scope del checker para el bloque: {context.GetText().Substring(0,20)}");
        _currentCodeGenScope = scopeBeforeBlock; // Mantener el scope anterior si no se encuentra el específico
    } else {
        _currentCodeGenScope = blockCheckerScope; // USAR EL SCOPE DEL CHECKER
    }
    // --- FIN OBTENER SCOPE ---

    if (context.children != null) {
        foreach (var item in context.children) {
            if (item is MiniCSharpParser.VarDeclContext varCtx) 
            { 
                VisitVarDecl(varCtx); // Ahora debería usar el _currentCodeGenScope correcto
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


        // --- Literales (Sin Cambios) ---
        public override object VisitNumberFactor(MiniCSharpParser.NumberFactorContext context)
        { /* ... como antes ... */ 
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitNumberFactor."); return null; }
            if (context.number().INTCONST() != null)
            { _ilGenerator.Emit(OpCodes.Ldc_I4, int.Parse(context.number().INTCONST().GetText())); }
            else if (context.number().DOUBLECONST() != null)
            { _ilGenerator.Emit(OpCodes.Ldc_R8, double.Parse(context.number().DOUBLECONST().GetText())); }
            return null;
        }
        public override object VisitCharFactor(MiniCSharpParser.CharFactorContext context)
        { /* ... como antes ... */ 
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitCharFactor."); return null; }
            string text = context.CHARCONST().GetText(); char val = text.Length >= 3 ? text[1] : '\0'; 
            _ilGenerator.Emit(OpCodes.Ldc_I4, (int)val); return null;
        }
        public override object VisitStringFactor(MiniCSharpParser.StringFactorContext context)
        { /* ... como antes ... */ 
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitStringFactor."); return null; }
            string text = context.STRINGCONST().GetText(); string val = text.Substring(1, text.Length - 2); 
            _ilGenerator.Emit(OpCodes.Ldstr, val); return null;
        }
        public override object VisitBoolFactor(MiniCSharpParser.BoolFactorContext context)
        { /* ... como antes ... */
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitBoolFactor."); return null; }
            _ilGenerator.Emit(context.TRUE() != null ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0); return null;
        }

        // --- Write Statement (Sin Cambios) ---
        public override object VisitWriteStatement(MiniCSharpParser.WriteStatementContext context)
        { /* ... como antes, depende de GetExpressionType ... */
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitWriteStatement."); return null; }
            Visit(context.expr()); Compiladores.Checker.Type exprMiniCSharpType = GetExpressionType(context.expr()); 
            if (exprMiniCSharpType == Compiladores.Checker.Type.Error) { Console.Error.WriteLine($"CodeGen Error: tipo no resuelto para write: {context.expr().GetText()}"); return null; }
            System.Type netType = ResolveNetType(exprMiniCSharpType); MethodInfo writeLineMethod = typeof(System.Console).GetMethod("WriteLine", new System.Type[] { netType });
            if (writeLineMethod == null) { if (netType.IsValueType && netType != typeof(void)) { _ilGenerator.Emit(OpCodes.Box, netType); } writeLineMethod = typeof(System.Console).GetMethod("WriteLine", new System.Type[] { typeof(object) }); }
            if (writeLineMethod != null) { _ilGenerator.Emit(OpCodes.Call, writeLineMethod); }
            else { Console.Error.WriteLine($"CodeGen Error: No Console.WriteLine para {netType} (MiniC#: {exprMiniCSharpType})."); if (netType != typeof(void)) _ilGenerator.Emit(OpCodes.Pop); }
            return null;
        }
        
        // --- GetExpressionType (Sigue siendo Placeholder CRÍTICO) ---
        private Compiladores.Checker.Type GetExpressionType(IParseTree treeNode)
        {
            // IDEALMENTE: Recuperar de un mapa llenado por el Checker
            if (ExpressionTypes.TryGetValue(treeNode, out Compiladores.Checker.Type type))
            {
                return type;
            }

            // Placeholder MUY BÁSICO (solo para literales directos como única parte de la expresión):
            if (treeNode is MiniCSharpParser.ExprContext exprContext && exprContext.term().Length == 1 && exprContext.term(0).factor().Length == 1 && exprContext.MINUS() == null && exprContext.cast() == null)
            {
                var factor = exprContext.term(0).factor(0);
                if (factor is MiniCSharpParser.NumberFactorContext nfCtx) {
                    if (nfCtx.number().INTCONST() != null) return Compiladores.Checker.Type.Int;
                    if (nfCtx.number().DOUBLECONST() != null) return Compiladores.Checker.Type.Double;
                }
                if (factor is MiniCSharpParser.StringFactorContext) return Compiladores.Checker.Type.String;
                if (factor is MiniCSharpParser.CharFactorContext) return Compiladores.Checker.Type.Char;
                if (factor is MiniCSharpParser.BoolFactorContext) return Compiladores.Checker.Type.Bool;
                if (factor is MiniCSharpParser.DesignatorFactorContext dfCtx) {
                     return GetExpressionType(dfCtx.designator()); // Recursión simple
                }
            }
            else if (treeNode is MiniCSharpParser.DesignatorContext desCtx)
            {
                // Esto es simplificado. Necesitas obtener el símbolo y su tipo.
                // Asume que es un identificador simple y está en el scope actual del método.
                Symbol sym = _currentCodeGenScope.Find(desCtx.IDENT(0).GetText());
                if (sym != null) return sym.Type;
            }

            Console.WriteLine($"Advertencia GetExpressionType: tipo no determinado para '{treeNode.GetText()}'. Usando Type.Error.");
            return Compiladores.Checker.Type.Error;
        }

        public override object VisitReturnStatement(MiniCSharpParser.ReturnStatementContext context)
        { /* ... como antes ... */ 
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitReturnStatement."); return null; }
            if (context.expr() != null) { Visit(context.expr()); } _ilGenerator.Emit(OpCodes.Ret); return null;
        }

        // --- NUEVAS IMPLEMENTACIONES ---
        public override object VisitDesignatorFactor(MiniCSharpParser.DesignatorFactorContext context)
{
    if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitDesignatorFactor."); return null; }
    var designatorNode = context.designator();
    if (context.LPAREN() == null) 
    {
        if (designatorNode.IDENT().Length > 0 && designatorNode.DOT().Length == 0 && designatorNode.LBRACK().Length == 0)
        {
            string varName = designatorNode.IDENT(0).GetText();
            // Usa Find para buscar jerárquicamente (bloque actual, luego método, luego clase, luego global)
            Symbol symbol = _currentCodeGenScope.Find(varName); 

            if (symbol is VarSymbol varSymbol)
            {
                if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lb))
                {
                    _ilGenerator.Emit(OpCodes.Ldloc, lb);
                    Console.WriteLine($"CodeGen INFO: Ldloc para '{varName}' (Index: {lb.LocalIndex})");
                }
                else if (_currentGeneratingMethodSymbol != null && 
                         _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p == varSymbol) is VarSymbol paramSymbol)
                {
                    int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                    bool isStaticMethod = (_currentMethodBuilder?.Attributes & MethodAttributes.Static) == MethodAttributes.Static;
                    _ilGenerator.Emit(OpCodes.Ldarg, (short)(isStaticMethod ? paramIndex : paramIndex + 1));
                     Console.WriteLine($"CodeGen INFO: Ldarg para parámetro '{varName}' (Index: {(isStaticMethod ? paramIndex : paramIndex + 1)})");
                }
                else if (_fieldBuilders.TryGetValue(varSymbol, out FieldBuilder fb))
                {
                    if (fb.IsStatic) { _ilGenerator.Emit(OpCodes.Ldsfld, fb); }
                    else { _ilGenerator.Emit(OpCodes.Ldarg_0); _ilGenerator.Emit(OpCodes.Ldfld, fb); }
                    Console.WriteLine($"CodeGen INFO: Ld(s)fld para campo '{varName}'");
                }
                else { Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): No se encontró almacenamiento para VarSymbol '{varName}'. Scope: {_currentCodeGenScope.GetHashCode()}"); }
            }
            else { Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Designador '{varName}' no es VarSymbol o no se encontró. Encontrado: {symbol?.GetType().Name}. Scope: {_currentCodeGenScope.GetHashCode()}"); }
        }
        else { Console.Error.WriteLine($"CodeGen Error: Carga de designador complejo no implementada: {designatorNode.GetText()}"); }
    }
    else 
    { HandleMethodCall(designatorNode, context.actPars()); }
    return null;
}
        
        public override object VisitDesignatorStatement(MiniCSharpParser.DesignatorStatementContext context)
{
    if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitDesignatorStatement."); return null; }

    // El designador es el objetivo de la operación
    var designatorNode = context.designator(); 

    if (context.ASSIGN() != null) // Es una asignación: designator = expr
    {
        string varName = designatorNode.IDENT(0).GetText(); // Simplificado, asume designador simple
        VarSymbol varSymbol = _currentCodeGenScope.Find(varName) as VarSymbol;
        if (varSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No se encontró VarSymbol para '{varName}' en asignación."); return null; }

        // Lógica para determinar si es campo y cargar 'this' si es necesario (para Stfld)
        FieldBuilder fb_assign = null;
        bool isStaticField_assign = false;
        if (_fieldBuilders.TryGetValue(varSymbol, out fb_assign))
        {
            isStaticField_assign = fb_assign.IsStatic;
            if (!isStaticField_assign)
            {
                _ilGenerator.Emit(OpCodes.Ldarg_0); // Cargar 'this' para campo de instancia
            }
        }
        
        Visit(context.expr()); // El valor de la expresión del lado derecho queda en la pila

        // Ahora la pila tiene: [this (si es campo no estático)] [valor_expr]

        if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lb_assign))
        {
            _ilGenerator.Emit(OpCodes.Stloc, lb_assign);
        }
        else if (fb_assign != null) // Usar el fb_assign obtenido antes
        {
            _ilGenerator.Emit(isStaticField_assign ? OpCodes.Stsfld : OpCodes.Stfld, fb_assign);
        }
        else if (_currentGeneratingMethodSymbol != null) // ¿Es un parámetro?
        {
            int paramIndex = _currentGeneratingMethodSymbol.Parameters.FindIndex(p => p.Name == varName);
            if (paramIndex != -1)
            {
                bool isStaticMethod = (_currentMethodBuilder?.Attributes & MethodAttributes.Static) == MethodAttributes.Static;
                _ilGenerator.Emit(OpCodes.Starg, (short)(isStaticMethod ? paramIndex : paramIndex + 1));
            }
            else { Console.Error.WriteLine($"CodeGen Error: No se encontró Local/Field/Param para asignación a '{varName}'."); }
        }
        else { Console.Error.WriteLine($"CodeGen Error: No se encontró Local/Field para asignación a '{varName}'."); }
    }
    else if (context.INC() != null || context.DEC() != null)
    {
        string varName = designatorNode.IDENT(0).GetText(); // Simplificado
        VarSymbol varSymbol = _currentCodeGenScope.Find(varName) as VarSymbol;
        if (varSymbol == null) { Console.Error.WriteLine($"CodeGen Error: No VarSymbol para '{varName}' en INC/DEC."); return null; }

        // 1. Determinar dónde está la variable (local, field, param) y cargar 'this' si es field de instancia
        LocalBuilder lb_incdec = null;
        FieldBuilder fb_incdec = null;
        int paramIndex_incdec = -1;
        bool isStaticField_incdec = false;
        bool isStaticMethod_incdec = (_currentMethodBuilder?.Attributes & MethodAttributes.Static) == MethodAttributes.Static;


        if (_localBuilders.TryGetValue(varSymbol, out lb_incdec))
        {
            // Local: no se necesita cargar 'this'
        }
        else if (_fieldBuilders.TryGetValue(varSymbol, out fb_incdec))
        {
            isStaticField_incdec = fb_incdec.IsStatic;
            if (!isStaticField_incdec)
            {
                _ilGenerator.Emit(OpCodes.Ldarg_0); // Cargar 'this' para el STFLD/LDFLD
                _ilGenerator.Emit(OpCodes.Dup);      // Duplicar 'this' (uno para LDFLD, otro para STFLD)
            }
        }
        else if (_currentGeneratingMethodSymbol != null && 
                 (paramIndex_incdec = _currentGeneratingMethodSymbol.Parameters.FindIndex(p => p.Name == varName)) != -1)
        {
            // Parámetro: no se necesita cargar 'this' para el parámetro en sí
        } else { Console.Error.WriteLine($"CodeGen Error: Variable '{varName}' no encontrada para INC/DEC."); return null;}


        // 2. Cargar el valor actual de la variable
        if (lb_incdec != null) _ilGenerator.Emit(OpCodes.Ldloc, lb_incdec);
        else if (fb_incdec != null) _ilGenerator.Emit(isStaticField_incdec ? OpCodes.Ldsfld : OpCodes.Ldfld, fb_incdec);
        else if (paramIndex_incdec != -1) _ilGenerator.Emit(OpCodes.Ldarg, (short)(isStaticMethod_incdec ? paramIndex_incdec : paramIndex_incdec + 1));

        // 3. Cargar 1
        _ilGenerator.Emit(OpCodes.Ldc_I4_1);

        // 4. Operar
        _ilGenerator.Emit(context.INC() != null ? OpCodes.Add : OpCodes.Sub);
        // El nuevo valor está en la pila. Si es un campo de instancia, 'this' ya fue duplicado y está debajo.

        // 5. Guardar
        if (lb_incdec != null) _ilGenerator.Emit(OpCodes.Stloc, lb_incdec);
        else if (fb_incdec != null) _ilGenerator.Emit(isStaticField_incdec ? OpCodes.Stsfld : OpCodes.Stfld, fb_incdec);
        else if (paramIndex_incdec != -1) _ilGenerator.Emit(OpCodes.Starg, (short)(isStaticMethod_incdec ? paramIndex_incdec : paramIndex_incdec + 1));
    }
    else if (context.LPAREN() != null) // Llamada a método como statement: designator ( [actPars] )
    {
        // Visitar el designador como si fuera un factor que resulta en una llamada a método
        // Esto es similar a lo que haría VisitDesignatorFactor para una llamada.
        // Necesitamos construir una estructura DesignatorFactorContext implícita o replicar su lógica.
        // Por ahora, llamaremos a un método helper o directamente la lógica.

        // Esta es la parte más compleja de este refactor:
        // El `context.designator()` es el `MiniCSharpParser.DesignatorContext`
        // Y `context.actPars()` es el `MiniCSharpParser.ActParsContext` (si existe)

        HandleMethodCall(designatorNode, context.actPars()); // Nuevo método helper

        // Si el método llamado NO era void, su valor de retorno está en la pila. Hay que quitarlo.
        // Necesitamos obtener el MethodSymbol para saber su tipo de retorno.
        // Esta lógica de Pop es importante.
        Symbol calledSymbol = ResolveDesignatorToCallableSymbol(designatorNode); // Necesitas este helper
        if (calledSymbol is MethodSymbol calledMethodSym)
        {
            if (calledMethodSym.Type != Compiladores.Checker.Type.Void)
            {
                _ilGenerator.Emit(OpCodes.Pop);
            }
        } else {
             Console.Error.WriteLine($"CodeGen Warning: No se pudo determinar el tipo de retorno para la llamada a '{designatorNode.GetText()}' como statement. No se emitió Pop.");
        }
    }
    return null;
}

// --- NUEVO MÉTODO HELPER para manejar llamadas a métodos ---
// (Similar a la lógica que estaría en VisitDesignatorFactor para llamadas)
private void HandleMethodCall(MiniCSharpParser.DesignatorContext designatorCtx, MiniCSharpParser.ActParsContext actParsCtx)
{
    // 1. Resolver el designador a un MethodSymbol
    Symbol methodSymRaw = ResolveDesignatorToCallableSymbol(designatorCtx); // Necesitas implementar esto

    if (!(methodSymRaw is MethodSymbol methodSymbol))
    {
        Console.Error.WriteLine($"CodeGen Error: '{designatorCtx.GetText()}' no es un método llamable.");
        // Podrías necesitar limpiar la pila si los argumentos ya se evaluaron
        if (actParsCtx != null) {
            foreach (var argExpr in actParsCtx.expr()) _ilGenerator.Emit(OpCodes.Pop); // Asumiendo que cada arg es 1 valor
        }
        return;
    }

    // 2. Obtener el MethodBuilder (si es un método definido en este ensamblado)
    //    o MethodInfo (si es un método externo como Console.WriteLine)
    MethodInfo methodToCall = null;
    if (_methodBuilders.TryGetValue(methodSymbol, out MethodBuilder mb))
    {
        methodToCall = mb;
    }
    else
    {
        // Lógica para métodos externos (ej. Console.WriteLine si lo manejaras aquí)
        // Por ahora, asumimos que solo llamamos a métodos definidos en MiniC#
        Console.Error.WriteLine($"CodeGen Error: No se encontró MethodBuilder para el símbolo del método '{methodSymbol.Name}'.");
        return;
    }

    // 3. Cargar 'this' si es un método de instancia (no estático)
    //    Para MiniC#, si 'Main' es estático y otros son de instancia, necesitarías esta lógica.
    //    Si todos los métodos de la clase principal son estáticos, no se necesita.
    if (!methodToCall.IsStatic)
    {
        // Asume que el objeto está en arg0 (si estamos dentro de un método de instancia)
        // o que se cargó previamente si es una llamada tipo obj.Metodo().
        // Para MiniC# simple, los métodos de la clase Program probablemente serán todos estáticos
        // o necesitarás una forma de obtener la instancia de la clase Program.
        // Si son métodos de la clase actual y esta se instancia:
        // _ilGenerator.Emit(OpCodes.Ldarg_0); // Carga 'this'
        // Por ahora, si son métodos de la clase principal y estáticos, esto no es necesario.
        // Si son de clases internas (que en MiniC# son como structs sin métodos), esto no aplica.
         Console.WriteLine($"CodeGen Info: Asumiendo que el método '{methodSymbol.Name}' es estático o 'this' se maneja externamente.");
    }

    // 4. Visitar actPars para poner los argumentos en la pila en el orden correcto
    if (actParsCtx != null)
    {
        for (int i = 0; i < actParsCtx.expr().Length; i++)
        {
            Visit(actParsCtx.expr(i)); // El valor del argumento queda en la pila
            // TODO: Chequeo de tipo y conversión si el argumento actual no coincide con el tipo del parámetro formal
            // Compiladores.Checker.Type formalParamType = methodSymbol.Parameters[i].Type;
            // Compiladores.Checker.Type actualArgType = GetExpressionType(actParsCtx.expr(i));
            // if (!AreTypesDirectlyCompatibleOrConvertible(formalParamType, actualArgType)) { ... emitir conversión o error ... }
        }
    }

    // 5. Emitir la instrucción Call
    _ilGenerator.Emit(OpCodes.Call, methodToCall); // Usar Callvirt si fueran métodos virtuales
}

// --- NUEVO MÉTODO HELPER (NECESITAS IMPLEMENTARLO CORRECTAMENTE) ---
private Symbol ResolveDesignatorToCallableSymbol(MiniCSharpParser.DesignatorContext designatorCtx)
{
    // Esta es una función compleja. Debe navegar el designador.
    // Si es solo IDENT -> busca en el scope actual y global.
    // Si es IDENT.IDENT... -> Resuelve el primer IDENT a un objeto/clase, luego busca el miembro.
    // Por ahora, una simplificación MUY GRANDE:
    string baseName = designatorCtx.IDENT(0).GetText();
    if (designatorCtx.ChildCount == 1) // Solo un IDENT
    {
        return _currentCodeGenScope.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
    }
    // TODO: Implementar para designadores complejos (obj.metodo, etc.)
    Console.Error.WriteLine($"CodeGen Error: ResolveDesignatorToCallableSymbol no implementado para designadores complejos: {designatorCtx.GetText()}");
    return null;
}
        
        public override object VisitExpr(MiniCSharpParser.ExprContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitExpr."); return null; }

            // 1. Manejar el signo MENOS unario inicial
            bool hasUnaryMinus = context.MINUS() != null;

            // 2. Visitar el primer término
            Visit(context.term(0)); // El valor del primer término queda en la pila

            // 3. Aplicar el menos unario si existe
            if (hasUnaryMinus)
            {
                Compiladores.Checker.Type termType = GetExpressionType(context.term(0)); // Necesitas el tipo del término
                if (termType == Compiladores.Checker.Type.Int || termType == Compiladores.Checker.Type.Double) // Asumimos que Neg es para numéricos
                {
                    _ilGenerator.Emit(OpCodes.Neg);
                } else { Console.Error.WriteLine($"CodeGen Error: Operador unario '-' aplicado a tipo no numérico {termType}."); }
            }

            // 4. Procesar operaciones aditivas
            for (int i = 0; i < context.addop().Length; i++)
            {
                Visit(context.term(i + 1)); // El valor del siguiente término queda en la pila
                                            // Ahora la pila tiene [valor_izq] [valor_der]

                // TODO: Implementar promoción de tipos si es necesario (int a double)
                //       basado en GetExpressionType(context.term(i)) y GetExpressionType(context.term(i+1))

                string op = context.addop(i).GetText();
                if (op == "+")
                {
                    _ilGenerator.Emit(OpCodes.Add);
                }
                else if (op == "-")
                {
                    _ilGenerator.Emit(OpCodes.Sub);
                }
            }
            return null; 
        }
        
        public override object VisitTerm(MiniCSharpParser.TermContext context)
        {
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitTerm."); return null; }

            Visit(context.factor(0)); // El valor del primer factor queda en la pila

            for (int i = 0; i < context.mulop().Length; i++)
            {
                Visit(context.factor(i + 1)); // El valor del siguiente factor queda en la pila
                                             // Pila: [valor_izq] [valor_der]
                // TODO: Promoción de tipos
                
                string op = context.mulop(i).GetText();
                if (op == "*")
                {
                    _ilGenerator.Emit(OpCodes.Mul);
                }
                else if (op == "/")
                {
                    _ilGenerator.Emit(OpCodes.Div);
                }
                else if (op == "%")
                {
                    _ilGenerator.Emit(OpCodes.Rem);
                }
            }
            return null;
        }

        // --- Esqueletos para Estructuras de Control ---
        public override object VisitIfStatement(MiniCSharpParser.IfStatementContext context)
        {
            Console.WriteLine($"TODO CodeGen: Implementar VisitIfStatement para: {context.GetText()}");
            return base.VisitIfStatement(context);
        }

        public override object VisitWhileStatement(MiniCSharpParser.WhileStatementContext context)
        {
            Console.WriteLine($"TODO CodeGen: Implementar VisitWhileStatement para: {context.GetText()}");
            return base.VisitWhileStatement(context);
        }
    }
}
  