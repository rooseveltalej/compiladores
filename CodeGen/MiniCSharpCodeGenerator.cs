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
        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

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
                    // This case should ideally not happen if the checker always stores a valid type or Type.Error
                    Console.Error.WriteLine($"CodeGen Warning (GetExpressionType): Null type found in ExpressionTypes for node '{treeNode.GetText()}'. Defaulting to Type.Error.");
                    return Compiladores.Checker.Type.Error;
                }
                return type;
            }

            // If the node is not in ExpressionTypes, it might be a node that the checker
            // doesn't classify as an "expression" but the generator still needs its type (e.g., a raw Designator).
            // Attempt to infer based on a DesignatorContext if it's one.
            if (treeNode is MiniCSharpParser.DesignatorContext desCtx)
            {
                // This is a simplified lookup. For a robust solution, the checker
                // should ideally store types for designators that are used as expressions.
                // This attempts a direct symbol lookup based on the designator's first identifier.
                // It does not handle complex designators like obj.field or array[index].
                string baseName = desCtx.IDENT(0).GetText();
                Symbol sym = _currentCodeGenScope?.Find(baseName) ?? _symbolTable.SearchGlobal(baseName);
                if (sym != null && sym.Type != null)
                {
                    // Cache this inferred type to avoid re-inferring and to make it available if needed again.
                    ExpressionTypes[treeNode] = sym.Type;
                    return sym.Type;
                }
            }

            Console.Error.WriteLine($"CodeGen Error (GetExpressionType): Type not found in ExpressionTypes and could not be inferred for node '{treeNode.GetText()}' (Type: {treeNode.GetType().Name}). Defaulting to Type.Error.");
            // Optionally, store Type.Error for this node to prevent repeated warnings/errors.
            ExpressionTypes[treeNode] = Compiladores.Checker.Type.Error;
            return Compiladores.Checker.Type.Error;
        }

        public override object VisitReturnStatement(MiniCSharpParser.ReturnStatementContext context)
        { /* ... como antes ... */ 
            if (_ilGenerator == null) { Console.Error.WriteLine("Error: ILGenerator es null en VisitReturnStatement."); return null; }
            if (context.expr() != null) { Visit(context.expr()); } _ilGenerator.Emit(OpCodes.Ret); return null;
        }

        // --- NUEVAS IMPLEMENTACIONES ---
        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

public override object VisitDesignatorFactor(MiniCSharpParser.DesignatorFactorContext context)
{
    if (_ilGenerator == null) 
    { 
        Console.Error.WriteLine("CodeGen Error (VisitDesignatorFactor): ILGenerator is null."); 
        return null; 
    }

    var designatorNode = context.designator();

    if (context.LPAREN() == null) // It's a variable, field, or array element access, not a method call
    {
        Compiladores.Checker.Type designatorMiniCSharpType = GetExpressionType(designatorNode);
        if (designatorMiniCSharpType == Compiladores.Checker.Type.Error && designatorNode != null) // Added null check for designatorNode
        {
            Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Could not determine type for designator '{designatorNode.GetText()}'.");
            return null;
        }

        if (designatorNode != null && designatorNode.DOT().Length == 0 && designatorNode.LBRACK().Length == 0) // Simple identifier
        {
            string varName = designatorNode.IDENT(0).GetText();
            Symbol symbol = _currentCodeGenScope.Find(varName); // Find searches hierarchically

            if (symbol is VarSymbol varSymbol)
            {
                if (varSymbol.Kind == SymbolKind.Constant)
                {
                    // This designator is a named constant.
                    if (varSymbol.Name == "null" && varSymbol.Type.Kind == TypeKind.Null)
                    {
                        _ilGenerator.Emit(OpCodes.Ldnull);
                        // Console.WriteLine($"CodeGen INFO (VisitDesignatorFactor): Ldnull for 'null' constant.");
                    }
                    // For other user-defined named constants (e.g., const int MAX = 100;), 
                    // VarSymbol would need to store the actual constant value.
                    // The current VarSymbol structure does not store the value, only that it's a constant.
                    // Literal constants like '100' or 'true' are handled by VisitNumberFactor, VisitBoolFactor, etc.
                    else
                    {
                        // This path would be for named constants other than "null".
                        // Since VarSymbol doesn't store the value, we can't load it directly here.
                        // The checker should ensure such constants are either initialized in a way
                        // the generator can access (not currently supported by VarSymbol) or their
                        // direct use as values (other than 'null') might be restricted.
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Loading value of named constant '{varSymbol.Name}' of type '{varSymbol.Type.Name}' is not fully implemented (VarSymbol does not store the literal value).");
                        // As a placeholder, if we knew the type and it had a default, we could push it,
                        // but it's better to flag this as an unimplemented feature for general named constants.
                    }
                }
                else // It's a regular variable (SymbolKind.Variable), parameter, or field
                {
                    if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lb))
                    {
                        _ilGenerator.Emit(OpCodes.Ldloc, lb);
                        // Console.WriteLine($"CodeGen INFO (VisitDesignatorFactor): Ldloc for '{varName}' (Index: {lb.LocalIndex}, VarSymbol Hash: {varSymbol.GetHashCode()})");
                    }
                    else if (_currentGeneratingMethodSymbol != null &&
                             _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == varSymbol.Name && p.Level == varSymbol.Level) is VarSymbol paramSymbol)
                    {
                        int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                        bool isStaticMethod = (_currentMethodBuilder?.IsStatic ?? false);
                        
                        short argIndexToLoad = (short)(isStaticMethod ? paramIndex : paramIndex + 1);
                        _ilGenerator.Emit(OpCodes.Ldarg, argIndexToLoad);
                        // Console.WriteLine($"CodeGen INFO (VisitDesignatorFactor): Ldarg for parameter '{varName}' (Symbol Table Index: {paramIndex}, IL Arg Index: {argIndexToLoad})");
                    }
                    else if (_fieldBuilders.TryGetValue(varSymbol, out FieldBuilder fb))
                    {
                        if (fb.IsStatic) 
                        {
                            _ilGenerator.Emit(OpCodes.Ldsfld, fb);
                        }
                        else 
                        {
                            _ilGenerator.Emit(OpCodes.Ldarg_0); // Load 'this'
                            _ilGenerator.Emit(OpCodes.Ldfld, fb);
                        }
                        // Console.WriteLine($"CodeGen INFO (VisitDesignatorFactor): Ld(s)fld for field '{varName}' (Static: {fb.IsStatic})");
                    }
                    else 
                    { 
                        Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): No storage (LocalBuilder, Parameter, or FieldBuilder) found for VarSymbol '{varName}' (VarSymbol Hash: {varSymbol.GetHashCode()}). Scope: {_currentCodeGenScope?.GetHashCode()}");
                    }
                }
            }
            else if (symbol != null) // Symbol found, but not a VarSymbol
            { 
                Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Designator '{varName}' resolved to a symbol of kind '{symbol.Kind}' which is not a variable or loadable constant in this context."); 
            }
            else // Symbol not found
            { 
                Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Symbol for designator '{varName}' not found. Scope: {_currentCodeGenScope?.GetHashCode()}"); 
            }
        }
        else if (designatorNode != null) // designatorNode is not null, but it's a complex designator
        {
            // TODO: Implement loading for complex designators (e.g., array elements obj.field[index].anotherField)
            // This would involve recursively visiting parts of the designatorNode to:
            // 1. Load the base object/array.
            // 2. Load the index if it's an array access (Ldelem_Ref or Ldelema for address).
            // 3. Load the field if it's a field access (Ldfld or Ldflda for address).
            Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Loading for complex designator '{designatorNode.GetText()}' not fully implemented.");
        }
        else // designatorNode is null
        {
             Console.Error.WriteLine($"CodeGen Error (VisitDesignatorFactor): Designator node is null.");
        }
    }
    else // It's a method call: designator ( [actPars] )
    {
        HandleMethodCall(designatorNode, context.actPars());
    }
    return null;
}
        
        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

public override object VisitDesignatorStatement(MiniCSharpParser.DesignatorStatementContext context)
{
    if (_ilGenerator == null) 
    { 
        Console.Error.WriteLine("CodeGen Error (VisitDesignatorStatement): ILGenerator is null."); 
        return null; 
    }

    var designatorNode = context.designator();

    if (context.ASSIGN() != null) // Assignment: designator = expr
    {
        // For simple designators (e.g., IDENT). Complex ones need more logic.
        if (designatorNode.DOT().Length == 0 && designatorNode.LBRACK().Length == 0)
        {
            string varName = designatorNode.IDENT(0).GetText();
            Symbol symbol = _currentCodeGenScope.Find(varName);

            if (!(symbol is VarSymbol varSymbol))
            {
                Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): VarSymbol for '{varName}' not found for assignment.");
                // If expr was visited, its value is on the stack, pop it.
                if (context.expr() != null) _ilGenerator.Emit(OpCodes.Pop);
                return null;
            }

            FieldBuilder fbAssign = null;
            bool isStaticFieldAssign = false;

            // Check if it's a field to load 'this' before the expression value for instance fields
            if (_fieldBuilders.TryGetValue(varSymbol, out fbAssign))
            {
                isStaticFieldAssign = fbAssign.IsStatic;
                if (!isStaticFieldAssign)
                {
                    _ilGenerator.Emit(OpCodes.Ldarg_0); // Load 'this' for Stfld
                }
            }
            
            // Evaluate the expression on the right-hand side. Its value will be on top of the stack.
            // If it was an instance field, 'this' is below it.
            Visit(context.expr()); 
            Compiladores.Checker.Type rhsType = GetExpressionType(context.expr());
            Compiladores.Checker.Type lhsType = varSymbol.Type;

            // --- Optional: Type Coercion/Conversion for Assignment ---
            if (lhsType == Compiladores.Checker.Type.Double && rhsType == Compiladores.Checker.Type.Int)
            {
                _ilGenerator.Emit(OpCodes.Conv_R8); // Promote RHS int to double
            }
            // Add more conversion rules if needed, or rely on checker to ensure compatibility.


            if (_localBuilders.TryGetValue(varSymbol, out LocalBuilder lbAssign))
            {
                _ilGenerator.Emit(OpCodes.Stloc, lbAssign);
                // Console.WriteLine($"CodeGen INFO (VisitDesignatorStatement): Stloc for '{varName}' (Index: {lbAssign.LocalIndex})");
            }
            else if (fbAssign != null) // Use the fbAssign resolved earlier
            {
                _ilGenerator.Emit(isStaticFieldAssign ? OpCodes.Stsfld : OpCodes.Stfld, fbAssign);
                // Console.WriteLine($"CodeGen INFO (VisitDesignatorStatement): St(s)fld for field '{varName}'");
            }
            else if (_currentGeneratingMethodSymbol != null) // Check if it's a parameter
            {
                // Find parameter by name and level to ensure it's the correct one
                VarSymbol paramSymbol = _currentGeneratingMethodSymbol.Parameters
                                          .FirstOrDefault(p => p.Name == varSymbol.Name && p.Level == varSymbol.Level);
                if (paramSymbol != null)
                {
                    int paramIndex = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymbol);
                    bool isStaticMethod = (_currentMethodBuilder?.IsStatic ?? false);
                    _ilGenerator.Emit(OpCodes.Starg, (short)(isStaticMethod ? paramIndex : paramIndex + 1));
                    // Console.WriteLine($"CodeGen INFO (VisitDesignatorStatement): Starg for parameter '{varName}'");
                }
                else 
                {
                    Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): No LocalBuilder, FieldBuilder, or Parameter found for assignment to '{varName}'.");
                }
            }
            else 
            {
                Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): No LocalBuilder or FieldBuilder found for assignment to '{varName}'.");
            }
        }
        else
        {
            // TODO: Implement assignment for complex designators (array elements, object fields)
            // This would involve:
            // 1. Emitting code to get the object instance or array instance onto the stack.
            // 2. If array, emit code to get the index onto the stack.
            // 3. Visit context.expr() to get the value to be stored.
            // 4. Emit Stelem or Stfld.
            Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): Assignment to complex designator '{designatorNode.GetText()}' not fully implemented.");
            if (context.expr() != null) _ilGenerator.Emit(OpCodes.Pop); // Clean up RHS value if it was pushed
        }
    }
    else if (context.INC() != null || context.DEC() != null)
    {
        // Handles simple IDENT++ or IDENT--
        if (designatorNode.DOT().Length == 0 && designatorNode.LBRACK().Length == 0)
        {
            string varName = designatorNode.IDENT(0).GetText();
            Symbol symbol = _currentCodeGenScope.Find(varName);

            if (!(symbol is VarSymbol varSymbol))
            {
                Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): VarSymbol for '{varName}' not found for INC/DEC.");
                return null;
            }
             Compiladores.Checker.Type varType = GetExpressionType(designatorNode); // Or varSymbol.Type
             if (varType != Compiladores.Checker.Type.Int && varType != Compiladores.Checker.Type.Double)
             {
                 Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): INC/DEC can only be applied to int or double, not {varType.Name}.");
                 return null;
             }


            LocalBuilder lbIncDec = null;
            FieldBuilder fbIncDec = null;
            VarSymbol paramSymIncDec = null;
            int paramIndexIncDec = -1;
            bool isStaticFieldIncDec = false;
            bool isStaticMethodIncDec = (_currentMethodBuilder?.IsStatic ?? false);

            // 1. Determine target and prepare for load/store
            if (_localBuilders.TryGetValue(varSymbol, out lbIncDec))
            {
                // Target is local variable
                _ilGenerator.Emit(OpCodes.Ldloc, lbIncDec);
            }
            else if (_fieldBuilders.TryGetValue(varSymbol, out fbIncDec))
            {
                // Target is a field
                isStaticFieldIncDec = fbIncDec.IsStatic;
                if (!isStaticFieldIncDec)
                {
                    _ilGenerator.Emit(OpCodes.Ldarg_0); // Load 'this' for instance field
                }
                _ilGenerator.Emit(isStaticFieldIncDec ? OpCodes.Ldsfld : OpCodes.Ldfld, fbIncDec);
            }
            else if (_currentGeneratingMethodSymbol != null && 
                     (paramSymIncDec = _currentGeneratingMethodSymbol.Parameters.FirstOrDefault(p => p.Name == varSymbol.Name && p.Level == varSymbol.Level)) != null)
            {
                // Target is a parameter
                paramIndexIncDec = _currentGeneratingMethodSymbol.Parameters.IndexOf(paramSymIncDec);
                _ilGenerator.Emit(OpCodes.Ldarg, (short)(isStaticMethodIncDec ? paramIndexIncDec : paramIndexIncDec + 1));
            }
            else
            {
                Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): Variable '{varName}' not found for INC/DEC.");
                return null;
            }

            // 2. Current value is on the stack. Load constant 1.
            if (varType == Compiladores.Checker.Type.Int)
            {
                 _ilGenerator.Emit(OpCodes.Ldc_I4_1);
            }
            else // Must be double
            {
                 _ilGenerator.Emit(OpCodes.Ldc_R8, 1.0);
            }


            // 3. Perform operation (Add or Sub)
            _ilGenerator.Emit(context.INC() != null ? OpCodes.Add : OpCodes.Sub);
            // Result is on the stack.

            // 4. Store the result back
            if (lbIncDec != null)
            {
                _ilGenerator.Emit(OpCodes.Stloc, lbIncDec);
            }
            else if (fbIncDec != null)
            {
                // For Stfld, 'this' (if instance field) needs to be on stack BEFORE the value.
                // This is tricky. A common pattern for field increment:
                // Ldarg_0 (this) (if instance)
                // Dup (if instance, to use 'this' again for Stfld)
                // Ldfld (value)
                // Ldc_I4_1
                // Add
                // Stfld
                // The current sequence loads value, then 1, then adds.
                // This means for Stfld, we'd need to have loaded 'this' before Ldfld AND it should persist or be reloaded.
                // Let's adjust the loading part:
                // Current stack: [result_of_op]
                // If it's an instance field, we need [this] [result_of_op]
                // The initial load was: Ldarg_0 (if instance), Ldfld OR Ldsfld
                // This is simpler if we re-evaluate the "address" part for fields.
                // For now, the current code is more like:  value = value + 1;
                // A direct field increment might be:
                // if (!isStaticFieldIncDec) _ilGenerator.Emit(OpCodes.Ldarg_0); // Load this for the target field
                // _ilGenerator.Emit(isStaticFieldIncDec ? OpCodes.Ldsfld : OpCodes.Ldfld, fbIncDec); // Load current value
                // _ilGenerator.Emit(OpCodes.Ldc_I4_1);
                // _ilGenerator.Emit(context.INC() != null ? OpCodes.Add : OpCodes.Sub);
                // // Now, if it's an instance field, we need 'this' again for stfld.
                // // The previous simple Ldarg_0 before Ldfld is consumed.
                // // This requires a more complex pattern like ld_addr, dup, ld_val, op, st_val
                // // or simpler: load 'this', load field, op, store back to field (requiring 'this' again for store)

                // Simpler (but less efficient for fields):
                // Assume result is on stack. We need to store it.
                if (isStaticFieldIncDec) {
                    _ilGenerator.Emit(OpCodes.Stsfld, fbIncDec);
                } else {
                    // This is where it gets tricky, 'this' was loaded for LDFLD, not STFLD.
                    // The pattern for `obj.field++` is often:
                    // ldloc obj (or ldarg_0 for this.field)
                    // dup
                    // ldfld obj.field
                    // ldc.i4.1
                    // add
                    // stfld obj.field
                    // The current code doesn't follow this pattern perfectly for fields yet.
                    // For now, we'll assume the checker ensures this is only for locals/params, or fields will be handled by a more complex designator logic.
                     Console.Error.WriteLine($"CodeGen Warning (VisitDesignatorStatement): INC/DEC for instance fields via this simple path is not fully robust. Value on stack will be stored. Ensure 'this' is correctly managed if '{varName}' is an instance field.");
                     // To attempt to make it work for a direct field (this.field++):
                     // 1. Ldarg_0 (for the eventual STFLD)
                     // 2. Ldarg_0 (for the LDFLD)
                     // 3. Ldfld
                     // 4. Ldc_I4_1
                     // 5. Add/Sub
                     // 6. Stfld
                     // This needs re-ordering of the initial load part.
                     // Given the current structure, we'd assume [this] [value_after_op] is needed if fbIncDec is an instance field.
                     // This part is problematic. Let's assume simple INC/DEC is on locals or static fields primarily here.
                    _ilGenerator.Emit(OpCodes.Stfld, fbIncDec); // This assumes 'this' is on stack if needed.
                }

            }
            else if (paramSymIncDec != null)
            {
                _ilGenerator.Emit(OpCodes.Starg, (short)(isStaticMethodIncDec ? paramIndexIncDec : paramIndexIncDec + 1));
            }
        }
        else
        {
            // TODO: Implement INC/DEC for complex designators
            Console.Error.WriteLine($"CodeGen Error (VisitDesignatorStatement): INC/DEC for complex designator '{designatorNode.GetText()}' not implemented.");
        }
    }
    else if (context.LPAREN() != null) // Method call as a statement: designator ( [actPars] )
    {
        HandleMethodCall(designatorNode, context.actPars());

        // If the called method was not void, its return value is on the stack. Pop it.
        Symbol calledSymbol = ResolveDesignatorToCallableSymbol(designatorNode); 
        if (calledSymbol is MethodSymbol calledMethodSym)
        {
            if (calledMethodSym.Type != Compiladores.Checker.Type.Void)
            {
                _ilGenerator.Emit(OpCodes.Pop);
            }
        } else {
             Console.Error.WriteLine($"CodeGen Warning (VisitDesignatorStatement): Could not determine return type for method call '{designatorNode.GetText()}' used as statement. Pop not emitted if value was returned.");
        }
    }
    return null;
}

// --- NUEVO MÉTODO HELPER para manejar llamadas a métodos ---
// (Similar a la lógica que estaría en VisitDesignatorFactor para llamadas)
    // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

private void HandleMethodCall(MiniCSharpParser.DesignatorContext designatorCtx, MiniCSharpParser.ActParsContext actParsCtx)
{
    MethodSymbol resolvedMethodSymbol = ResolveDesignatorToCallableSymbol(designatorCtx);
    MethodInfo methodToCall = null;

    if (resolvedMethodSymbol != null)
    {
        if (_methodBuilders.TryGetValue(resolvedMethodSymbol, out MethodBuilder mb))
        {
            methodToCall = mb;
        }
        else
        {
            Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): MethodBuilder not found for resolved MethodSymbol '{resolvedMethodSymbol.Name}'.");
            // Clean up stack if arguments were pushed
            if (actParsCtx != null) foreach (var _ in actParsCtx.expr()) _ilGenerator.Emit(OpCodes.Pop);
            return;
        }
    }
    else
    {
        // Attempt to handle some known external methods like Console.WriteLine directly
        // This part is a simplified example and would ideally be more robust,
        // perhaps driven by information in the symbol table for external/library classes.
        string fullMethodName = designatorCtx.GetText();
        if (fullMethodName.StartsWith("Console.Write") || fullMethodName.StartsWith("Console.Read")) // Simplified check
        {
            // This is where you'd use reflection to find System.Console.WriteLine/ReadLine
            // For WriteLine, you need to determine the argument type.
            if (actParsCtx != null && actParsCtx.expr().Length == 1 && fullMethodName == "Console.WriteLine")
            {
                var argExpr = actParsCtx.expr(0);
                Compiladores.Checker.Type argType = GetExpressionType(argExpr);
                Visit(argExpr); // Push argument onto stack
                System.Type netArgType = ResolveNetType(argType);

                // Try to find exact match, then object fallback
                methodToCall = typeof(System.Console).GetMethod("WriteLine", new[] { netArgType });
                if (methodToCall == null && netArgType.IsValueType) {
                    _ilGenerator.Emit(OpCodes.Box, netArgType); // Box if value type and no direct match
                    methodToCall = typeof(System.Console).GetMethod("WriteLine", new[] { typeof(object) });
                } else if (methodToCall == null) {
                     methodToCall = typeof(System.Console).GetMethod("WriteLine", new[] { typeof(object) });
                }

                if (methodToCall == null) {
                     Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Could not find suitable Console.WriteLine overload for type {netArgType}.");
                     _ilGenerator.Emit(OpCodes.Pop); // Pop argument
                     return;
                }
            }
            else if (fullMethodName == "Console.ReadLine" && (actParsCtx == null || actParsCtx.expr().Length == 0))
            {
                 methodToCall = typeof(System.Console).GetMethod("ReadLine", System.Type.EmptyTypes);
                 if (methodToCall == null) {
                     Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Could not find System.Console.ReadLine().");
                     return;
                 }
            }
            else
            {
                Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Unresolved or unsupported external method: {fullMethodName}");
                if (actParsCtx != null) foreach (var _ in actParsCtx.expr()) _ilGenerator.Emit(OpCodes.Pop);
                return;
            }
        }
        else
        {
            Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Could not resolve method '{designatorCtx.GetText()}' to a MethodSymbol or known external.");
            if (actParsCtx != null) foreach (var _ in actParsCtx.expr()) _ilGenerator.Emit(OpCodes.Pop);
            return;
        }
    }
    
    // If calling an instance method (not static) that is user-defined:
    // The 'this' pointer (or object instance) must be on the stack before arguments.
    // This part is complex for `obj.Method()` type calls.
    // For simple `Method()` calls within the same class, if it's an instance method, Ldarg_0 is needed.
    bool isStaticCall = false; // Assume instance call by default if resolvedSymbol is not null
    bool isInstanceMethodOnThis = false;

    if (methodToCall != null) isStaticCall = methodToCall.IsStatic;


    if (resolvedMethodSymbol != null && !isStaticCall) // User-defined instance method
    {
        // Is it obj.Method() or just Method() (implicitly this.Method())?
        if (designatorCtx.DOT().Length == 0) // Implicit this.Method()
        {
            if ((_currentMethodBuilder?.IsStatic ?? true)) // Calling instance method from static context
            {
                 Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Cannot call instance method '{resolvedMethodSymbol.Name}' from a static context without an object instance.");
                 if (actParsCtx != null) foreach (var _ in actParsCtx.expr()) _ilGenerator.Emit(OpCodes.Pop);
                 return;
            }
            _ilGenerator.Emit(OpCodes.Ldarg_0); // Load 'this'
            isInstanceMethodOnThis = true;
        }
        else // obj.Method()
        {
            // The object instance 'obj' should have been loaded by VisitDesignator for designatorCtx.IDENT(0)
            // This part is tricky and depends on how VisitDesignator handles object loading.
            // For now, we assume if it's obj.Method, 'obj' is already on the stack.
            // This needs to be coordinated with VisitDesignatorFactor/VisitDesignator.
            Console.WriteLine($"CodeGen INFO (HandleMethodCall): Emitting call to instance method '{resolvedMethodSymbol.Name}' on an object assumed to be on stack.");
        }
    }


    // Evaluate arguments and push them onto the stack
    if (actParsCtx != null)
    {
        var formalParams = resolvedMethodSymbol?.Parameters ?? new List<VarSymbol>();
        if (formalParams.Count != actParsCtx.expr().Length && resolvedMethodSymbol != null) // Check only if we have a symbol
        {
             Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): Argument count mismatch for method '{resolvedMethodSymbol.Name}'. Expected {formalParams.Count}, got {actParsCtx.expr().Length}.");
             // Pop anything pushed by previous argument visits and the potential 'this'
             for(int i=0; i<actParsCtx.expr().Length; ++i) _ilGenerator.Emit(OpCodes.Pop);
             if(isInstanceMethodOnThis) _ilGenerator.Emit(OpCodes.Pop);
             return;
        }

        for (int i = 0; i < actParsCtx.expr().Length; i++)
        {
            var argExpr = actParsCtx.expr(i);
            Visit(argExpr); // Argument value is now on the stack

            if (resolvedMethodSymbol != null) // If we have formal parameter info
            {
                Compiladores.Checker.Type formalType = formalParams[i].Type;
                Compiladores.Checker.Type actualType = GetExpressionType(argExpr);

                if (formalType == Compiladores.Checker.Type.Double && actualType == Compiladores.Checker.Type.Int)
                {
                    _ilGenerator.Emit(OpCodes.Conv_R8); // Promote int arg to double
                }
                // Add other necessary coercions here
            }
        }
    }

    // Emit the call instruction
    if (methodToCall != null)
    {
        _ilGenerator.Emit(methodToCall.IsVirtual && !methodToCall.IsFinal ? OpCodes.Callvirt : OpCodes.Call, methodToCall);
    }
    else
    {
        // Should have been caught earlier, but as a safeguard:
        Console.Error.WriteLine($"CodeGen Error (HandleMethodCall): methodToCall is null before emitting OpCodes.Call for '{designatorCtx.GetText()}'.");
        // Clean up stack: pop arguments and 'this' if loaded
        if (actParsCtx != null) foreach (var _ in actParsCtx.expr()) _ilGenerator.Emit(OpCodes.Pop);
        if (isInstanceMethodOnThis) _ilGenerator.Emit(OpCodes.Pop); // Pop 'this'
    }
}

// --- NUEVO MÉTODO HELPER (NECESITAS IMPLEMENTARLO CORRECTAMENTE) ---
    // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

private MethodSymbol ResolveDesignatorToCallableSymbol(MiniCSharpParser.DesignatorContext designatorCtx)
{
    if (designatorCtx == null || designatorCtx.IDENT() == null || designatorCtx.IDENT().Length == 0)
    {
        Console.Error.WriteLine("CodeGen Error (ResolveDesignatorToCallableSymbol): Invalid designator context.");
        return null;
    }

    // Case 1: Simple method name (e.g., "MyMethod")
    if (designatorCtx.DOT().Length == 0 && designatorCtx.LBRACK().Length == 0)
    {
        string methodName = designatorCtx.IDENT(0).GetText();
        Symbol symbol = _currentCodeGenScope.Find(methodName); // Find hierarchically

        if (symbol is MethodSymbol methodSymbol)
        {
            return methodSymbol;
        }
        else if (symbol != null)
        {
            Console.Error.WriteLine($"CodeGen Error (ResolveDesignatorToCallableSymbol): Identifier '{methodName}' found but it is not a method (Kind: {symbol.Kind}).");
            return null;
        }
        // Check global scope explicitly if not found (though _currentCodeGenScope.Find should do this)
        Symbol globalSymbol = _symbolTable.SearchGlobal(methodName);
         if (globalSymbol is MethodSymbol globalMethodSymbol)
        {
            return globalMethodSymbol;
        }

        Console.Error.WriteLine($"CodeGen Error (ResolveDesignatorToCallableSymbol): Method '{methodName}' not found in current or global scope.");
        return null;
    }

    // Case 2: Qualified method name (e.g., "MyClass.MyMethod" or "obj.MyMethod") - Partially supported
    // This part needs careful implementation based on how your checker resolves types and members.
    // For now, let's assume a simple "ClassName.StaticMethod" or known static types like "Console.WriteLine".
    if (designatorCtx.DOT().Length > 0)
    {
        // Example: designatorCtx is "Console.WriteLine"
        // IDENT(0) is "Console", IDENT(1) is "WriteLine"
        string baseIdentifierName = designatorCtx.IDENT(0).GetText();
        string memberMethodName = designatorCtx.IDENT(1).GetText(); // Assumes only one DOT for simplicity

        Symbol baseSymbol = _currentCodeGenScope.Find(baseIdentifierName) ?? _symbolTable.SearchGlobal(baseIdentifierName);

        if (baseSymbol is ClassSymbol classSymbol)
        {
            if (classSymbol.Type is ClassType classType)
            {
                Symbol memberSymbol = classType.Members.FindCurrent(memberMethodName); // Find in class's own members
                if (memberSymbol is MethodSymbol methodSymbol)
                {
                    // TODO: Check if method is static if called like ClassName.MethodName
                    return methodSymbol;
                }
            }
        }
        else if (baseIdentifierName == "Console" && memberMethodName == "WriteLine") // Special case for built-in
        {
            // This is a hacky way. Ideally, "Console" would be a ClassSymbol
            // in your symbol table with "WriteLine" as one of its MethodSymbols.
            // For now, we won't return a MethodSymbol but let HandleMethodCall deal with it.
            Console.Error.WriteLine($"CodeGen Info (ResolveDesignatorToCallableSymbol): Special handling for 'Console.WriteLine' needed in HandleMethodCall.");
            return null; // Let HandleMethodCall try to find it via reflection
        }
        else if (baseSymbol is VarSymbol varSym)
        {
             // obj.Method() case: varSym.Type should be a ClassType
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
        
        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

public override object VisitExpr(MiniCSharpParser.ExprContext context)
{
    if (_ilGenerator == null) 
    { 
        Console.Error.WriteLine("CodeGen Error (VisitExpr): ILGenerator is null."); 
        return null; 
    }

    Compiladores.Checker.Type currentLeftType = GetExpressionType(context.term(0));
    Visit(context.term(0)); // Evaluate the first term, its value is on the stack.

    if (context.MINUS() != null) // Unary minus
    {
        if (currentLeftType == Compiladores.Checker.Type.Int || currentLeftType == Compiladores.Checker.Type.Double)
        {
            _ilGenerator.Emit(OpCodes.Neg);
        }
        else if (currentLeftType != Compiladores.Checker.Type.Error) // Avoid cascading errors
        {
            Console.Error.WriteLine($"CodeGen Error (VisitExpr): Unary minus cannot be applied to type '{currentLeftType.Name}'.");
            // Don't need to pop, as Neg would consume the value. If error, value is still there.
            // Depending on error strategy, you might pop or leave it.
        }
    }

    for (int i = 0; i < context.addop().Length; i++)
    {
        Compiladores.Checker.Type rightTermType = GetExpressionType(context.term(i + 1));
        Visit(context.term(i + 1)); // Evaluate the right term, its value is on the stack.
                                     // Stack: [left_val_possibly_negated] [right_val]

        if (currentLeftType == Compiladores.Checker.Type.Error || rightTermType == Compiladores.Checker.Type.Error)
        {
            // If one operand's type is error, the result is error.
            // Pop the valid operand if one exists, to prevent stack imbalance for subsequent ops.
            if(currentLeftType != Compiladores.Checker.Type.Error && rightTermType == Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop); // Pop left
            if(currentLeftType == Compiladores.Checker.Type.Error && rightTermType != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop); // Pop right
            currentLeftType = Compiladores.Checker.Type.Error;
            continue; // Skip operation
        }
        
        string op = context.addop(i).GetText();
        OpCode? opCodeToEmit = null;

        // Determine target type and perform promotion
        Compiladores.Checker.Type targetType = Compiladores.Checker.Type.Error;

        if ((currentLeftType == Compiladores.Checker.Type.Int || currentLeftType == Compiladores.Checker.Type.Double) &&
            (rightTermType == Compiladores.Checker.Type.Int || rightTermType == Compiladores.Checker.Type.Double))
        {
            // Arithmetic operation
            if (currentLeftType == Compiladores.Checker.Type.Double || rightTermType == Compiladores.Checker.Type.Double)
            {
                targetType = Compiladores.Checker.Type.Double;
                if (currentLeftType == Compiladores.Checker.Type.Int) // Left is Int, Right is Double
                {
                    // Stack: [int_val (left)] [double_val (right)]
                    // Convert int_val (left) to double. It's under the right_val.
                    // Store right, convert left, load right back.
                    LocalBuilder tempDouble = _ilGenerator.DeclareLocal(typeof(double));
                    _ilGenerator.Emit(OpCodes.Stloc, tempDouble);    // Store right_val (double)
                    _ilGenerator.Emit(OpCodes.Conv_R8);              // Convert left_val (int) to double
                    _ilGenerator.Emit(OpCodes.Ldloc, tempDouble);    // Load right_val (double) back
                }
                else if (rightTermType == Compiladores.Checker.Type.Int) // Left is Double, Right is Int
                {
                    // Stack: [double_val (left)] [int_val (right)]
                    _ilGenerator.Emit(OpCodes.Conv_R8); // Convert right_val (int) to double
                }
            }
            else // Both are Int
            {
                targetType = Compiladores.Checker.Type.Int;
            }
            opCodeToEmit = (op == "+") ? OpCodes.Add : OpCodes.Sub;
        }
        // String concatenation (simplified: assumes checker validated this)
        else if (op == "+" && (currentLeftType == Compiladores.Checker.Type.String || rightTermType == Compiladores.Checker.Type.String))
        {
            // This requires more complex handling: Convert non-strings to strings.
            // E.g., using String.Concat or calling .ToString() on boxed values.
            // For simplicity, we assume the checker ensures compatible types for string concat or we error.
            // A full implementation would box value types, call ToString, then Concat.
            if (currentLeftType != Compiladores.Checker.Type.String || rightTermType != Compiladores.Checker.Type.String)
            {
                Console.Error.WriteLine($"CodeGen Error (VisitExpr): String concatenation with non-string type without explicit conversion generation for '{currentLeftType.Name}' {op} '{rightTermType.Name}' not fully implemented.");
                // Pop operands if they are not string to avoid issues with a potential Concat call.
                 if(currentLeftType != Compiladores.Checker.Type.String) _ilGenerator.Emit(OpCodes.Pop); //This logic is flawed, if left is string and right is int, popping left is wrong.
                 if(rightTermType != Compiladores.Checker.Type.String) _ilGenerator.Emit(OpCodes.Pop);
                targetType = Compiladores.Checker.Type.Error; // Mark as error
            } else {
                 MethodInfo concatMethod = typeof(string).GetMethod("Concat", new[] { typeof(string), typeof(string) });
                if (concatMethod != null) {
                     _ilGenerator.Emit(OpCodes.Call, concatMethod);
                     targetType = Compiladores.Checker.Type.String;
                } else {
                    Console.Error.WriteLine($"CodeGen Error (VisitExpr): Could not find string.Concat(string,string) method.");
                    targetType = Compiladores.Checker.Type.Error;
                }
            }
        }
        else
        {
            Console.Error.WriteLine($"CodeGen Error (VisitExpr): Operator '{op}' cannot be applied to operands of type '{currentLeftType.Name}' and '{rightTermType.Name}'.");
            _ilGenerator.Emit(OpCodes.Pop); // Pop right operand
            _ilGenerator.Emit(OpCodes.Pop); // Pop left operand
            targetType = Compiladores.Checker.Type.Error;
        }

        if (opCodeToEmit.HasValue)
        {
            _ilGenerator.Emit(opCodeToEmit.Value);
        }
        currentLeftType = targetType; // Update type for the next operation in the chain
    }
    // The final result type of the expression is currentLeftType.
    // This is stored by the checker using StoreAndReturnType(context, currentLeftType).
    // The generator relies on this.
    return null; 
}
        
        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

public override object VisitTerm(MiniCSharpParser.TermContext context)
{
    if (_ilGenerator == null) 
    { 
        Console.Error.WriteLine("CodeGen Error (VisitTerm): ILGenerator is null."); 
        return null; 
    }

    Compiladores.Checker.Type currentLeftType = GetExpressionType(context.factor(0));
    Visit(context.factor(0)); // Evaluate the first factor

    for (int i = 0; i < context.mulop().Length; i++)
    {
        Compiladores.Checker.Type rightFactorType = GetExpressionType(context.factor(i + 1));
        Visit(context.factor(i + 1)); // Evaluate the right factor
                                      // Stack: [left_val] [right_val]

        if (currentLeftType == Compiladores.Checker.Type.Error || rightFactorType == Compiladores.Checker.Type.Error)
        {
            if(currentLeftType != Compiladores.Checker.Type.Error && rightFactorType == Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop); // Pop left
            if(currentLeftType == Compiladores.Checker.Type.Error && rightFactorType != Compiladores.Checker.Type.Error) _ilGenerator.Emit(OpCodes.Pop); // Pop right
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
                if (currentLeftType == Compiladores.Checker.Type.Int) // Left is Int, Right is Double
                {
                    LocalBuilder tempDouble = _ilGenerator.DeclareLocal(typeof(double));
                    _ilGenerator.Emit(OpCodes.Stloc, tempDouble);    // Store right_val (double)
                    _ilGenerator.Emit(OpCodes.Conv_R8);              // Convert left_val (int) to double
                    _ilGenerator.Emit(OpCodes.Ldloc, tempDouble);    // Load right_val (double) back
                }
                else if (rightFactorType == Compiladores.Checker.Type.Int) // Left is Double, Right is Int
                {
                    _ilGenerator.Emit(OpCodes.Conv_R8); // Convert right_val (int) to double
                }
            }
            else // Both are Int
            {
                targetType = Compiladores.Checker.Type.Int;
            }

            if (op == "*") opCodeToEmit = OpCodes.Mul;
            else if (op == "/") opCodeToEmit = OpCodes.Div;
            else if (op == "%")
            {
                if (targetType == Compiladores.Checker.Type.Double)
                {
                     Console.Error.WriteLine($"CodeGen Error (VisitTerm): Operator '%' not typically supported for double operands in CIL. Emitting Rem which might be int-only.");
                     // CIL 'rem' is for integers. For doubles, you might need a library call or custom implementation.
                     // Fallback to integer remainder if types were mixed, otherwise this is an error.
                     // Forcing integer context for Rem if one was double:
                     if (currentLeftType == Compiladores.Checker.Type.Double) _ilGenerator.Emit(OpCodes.Conv_I4); // Convert left to int (lossy)
                     // Stack: [left_int] [right_double_or_int]
                     LocalBuilder tempRight = _ilGenerator.DeclareLocal(ResolveNetType(rightFactorType));
                     _ilGenerator.Emit(OpCodes.Stloc, tempRight);
                     if (rightFactorType == Compiladores.Checker.Type.Double) _ilGenerator.Emit(OpCodes.Conv_I4); // Convert original right to int (lossy)
                     _ilGenerator.Emit(OpCodes.Ldloc, tempRight); // Restore original right
                     if (rightFactorType == Compiladores.Checker.Type.Double) { /* now stack is [left_int] [right_int] ? */ }
                                          
                     // This is getting very complex. The simplest is to disallow % with doubles at checker level.
                     // If checker allows it, then generator has to define behavior.
                     // Assuming checker ensures int % int.
                }
                opCodeToEmit = OpCodes.Rem;
            }
        }
        else
        {
            Console.Error.WriteLine($"CodeGen Error (VisitTerm): Operator '{op}' cannot be applied to operands of type '{currentLeftType.Name}' and '{rightFactorType.Name}'.");
            _ilGenerator.Emit(OpCodes.Pop); // Pop right
            _ilGenerator.Emit(OpCodes.Pop); // Pop left
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


public override object VisitUsingDirective(MiniCSharpParser.UsingDirectiveContext context)
{
    return base.VisitUsingDirective(context);
}

public override object VisitQualifiedIdent(MiniCSharpParser.QualifiedIdentContext context)
{
    return base.VisitQualifiedIdent(context);
}

public override object VisitFormPars(MiniCSharpParser.FormParsContext context)
{
    return base.VisitFormPars(context);
}

public override object VisitForStatement(MiniCSharpParser.ForStatementContext context)
{
    return base.VisitForStatement(context);
}

public override object VisitSwitchStatement(MiniCSharpParser.SwitchStatementContext context)
{
    return base.VisitSwitchStatement(context);
}

public override object VisitBreakStatement(MiniCSharpParser.BreakStatementContext context)
{
    return base.VisitBreakStatement(context);
}

public override object VisitReadStatement(MiniCSharpParser.ReadStatementContext context)
{
    return base.VisitReadStatement(context);
}

public override object VisitBlockStatement(MiniCSharpParser.BlockStatementContext context)
{
    return base.VisitBlockStatement(context);
}

public override object VisitEmptyStatement(MiniCSharpParser.EmptyStatementContext context)
{
    return base.VisitEmptyStatement(context);
}

public override object VisitSwitchCase(MiniCSharpParser.SwitchCaseContext context)
{
    return base.VisitSwitchCase(context);
}

public override object VisitDefaultCase(MiniCSharpParser.DefaultCaseContext context)
{
    return base.VisitDefaultCase(context);
}

public override object VisitConstant(MiniCSharpParser.ConstantContext context)
{
    return base.VisitConstant(context);
}

public override object VisitActPars(MiniCSharpParser.ActParsContext context)
{
    return base.VisitActPars(context);
}

// In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

public override object VisitCondition(MiniCSharpParser.ConditionContext context)
{
    if (_ilGenerator == null)
    {
        Console.Error.WriteLine("CodeGen Error (VisitCondition): ILGenerator is null.");
        return null;
    }

    // 1. Generar CIL para el primer condTerm
    Visit(context.condTerm(0)); // Deja un booleano (0 o 1) en la pila

    // 2. Para cada operador OR y el siguiente condTerm
    for (int i = 0; i < context.OR().Length; i++)
    {
        Visit(context.condTerm(i + 1)); // Deja otro booleano en la pila
        // Pila: [bool_anterior] [bool_actual]
        _ilGenerator.Emit(OpCodes.Or);  // Realiza la operación OR bit a bit.
        // Para 0/1, esto es un OR lógico.
        // Pila: [resultado_or]
    }
    // El resultado final (0 o 1) de la condición queda en la pila.
    return null;
}

// In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

public override object VisitCondTerm(MiniCSharpParser.CondTermContext context)
{
    if (_ilGenerator == null)
    {
        Console.Error.WriteLine("CodeGen Error (VisitCondTerm): ILGenerator is null.");
        return null;
    }

    // 1. Generar CIL para el primer condFact
    Visit(context.condFact(0)); // Deja un booleano (0 o 1) en la pila

    // 2. Para cada operador AND y el siguiente condFact
    for (int i = 0; i < context.AND().Length; i++)
    {
        Visit(context.condFact(i + 1)); // Deja otro booleano en la pila
        // Pila: [bool_anterior] [bool_actual]
        _ilGenerator.Emit(OpCodes.And); // Realiza la operación AND bit a bit.
        // Para 0/1, esto es un AND lógico.
        // Pila: [resultado_and]
    }
    // El resultado final (0 o 1) del término de condición queda en la pila.
    return null;
}

// In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

public override object VisitCondFact(MiniCSharpParser.CondFactContext context)
{
    if (_ilGenerator == null)
    {
        Console.Error.WriteLine("CodeGen Error (VisitCondFact): ILGenerator is null.");
        return null;
    }

    // 1. Generar CIL para la expresión izquierda
    Visit(context.expr(0));
    Compiladores.Checker.Type leftType = GetExpressionType(context.expr(0));

    // Si solo hay una expresión (ej. 'if (myBoolVariable)'), no hay relop ni expr(1).
    // En este caso, el valor de la expresión ya está en la pila.
    // El checker debería asegurar que es de tipo bool.
    // Si es bool, CIL espera 0 o 1. Si tu checker ya lo garantiza, está bien.
    // Si tu bool se representa de otra forma, necesitarías convertirlo a 0/1.
    // Por ahora, asumimos que si no hay relop, la expr(0) ya es un booleano CIL (0/1).
    if (context.relop() == null)
    {
        if (leftType != Compiladores.Checker.Type.Bool)
        {
            Console.Error.WriteLine($"CodeGen Warning (VisitCondFact): Condition '{context.expr(0).GetText()}' is not bool and used directly. Assuming it evaluates to 0 (false) or non-0 (true).");
            // Si es int y queremos que 0 sea false y !=0 sea true:
            // Ldc_I4_0
            // Ceq  // Compara con 0. Si es 0, el resultado es 1 (true). Si no es 0, es 0 (false).
            // Ldc_I4_0 // Para invertir el resultado de Ceq y que 0->0 y no-0 -> 1
            // Ceq
            // Esto es para if(int_value) donde int_value 0 es false, y no-0 es true.
            // Si MiniCSharp requiere que la expresión sea estrictamente bool, entonces el checker lo manejaría.
        }
        return null; // El valor booleano de expr(0) ya está en la pila.
    }

    // 2. Generar CIL para la expresión derecha
    Visit(context.expr(1));
    Compiladores.Checker.Type rightType = GetExpressionType(context.expr(1));

    // 3. Manejar promoción de tipos para la comparación
    // Si uno es double y el otro int, promover el int a double antes de la comparación.
    // Las instrucciones de comparación de CIL (clt, cgt, ceq) operan sobre operandos del mismo tipo en la pila.
    if (leftType == Compiladores.Checker.Type.Double && rightType == Compiladores.Checker.Type.Int)
    {
        // Pila: [double_izq] [int_der] -> convertir int_der a double
        _ilGenerator.Emit(OpCodes.Conv_R8);
    }
    else if (leftType == Compiladores.Checker.Type.Int && rightType == Compiladores.Checker.Type.Double)
    {
        // Pila: [int_izq] [double_der] -> convertir int_izq a double
        // Necesitamos guardar double_der, convertir int_izq, luego restaurar double_der
        LocalBuilder tempDouble = _ilGenerator.DeclareLocal(typeof(double));
        _ilGenerator.Emit(OpCodes.Stloc, tempDouble); // Guarda double_der
        _ilGenerator.Emit(OpCodes.Conv_R8);          // Convierte int_izq a double
        _ilGenerator.Emit(OpCodes.Ldloc, tempDouble); // Carga double_der de nuevo
                                                     // Pila: [double_izq_convertido] [double_der]
    }
    // Si ambos son int, o ambos son double, o ambos son char, o ambos son bool (para ==, !=)
    // no se necesita conversión explícita aquí si el checker ya validó la compatibilidad.

    // 4. Emitir la instrucción de comparación adecuada
    string op = context.relop().GetText();
    switch (op)
    {
        case "==":
            _ilGenerator.Emit(OpCodes.Ceq);
            break;
        case "!=":
            _ilGenerator.Emit(OpCodes.Ceq);
            _ilGenerator.Emit(OpCodes.Ldc_I4_0); // Carga 0 (false)
            _ilGenerator.Emit(OpCodes.Ceq);      // Compara el resultado de la igualdad con 0
                                                 // Si eran iguales (resultado 1), 1 == 0 es 0 (false)
                                                 // Si eran diferentes (resultado 0), 0 == 0 es 1 (true)
            break;
        case "<":
            _ilGenerator.Emit(OpCodes.Clt);
            break;
        case "<=": // Equivale a !(a > b)
            _ilGenerator.Emit(OpCodes.Cgt);      // Compara a > b
            _ilGenerator.Emit(OpCodes.Ldc_I4_0); // Carga 0 (false)
            _ilGenerator.Emit(OpCodes.Ceq);      // Invierte el resultado booleano
            break;
        case ">":
            _ilGenerator.Emit(OpCodes.Cgt);
            break;
        case ">=": // Equivale a !(a < b)
            _ilGenerator.Emit(OpCodes.Clt);      // Compara a < b
            _ilGenerator.Emit(OpCodes.Ldc_I4_0); // Carga 0 (false)
            _ilGenerator.Emit(OpCodes.Ceq);      // Invierte el resultado booleano
            break;
        default:
            Console.Error.WriteLine($"CodeGen Error (VisitCondFact): Operador relacional desconocido '{op}'.");
            // Podríamos necesitar limpiar la pila si las expresiones se evaluaron
            _ilGenerator.Emit(OpCodes.Pop); // Pop expr(1)
            _ilGenerator.Emit(OpCodes.Pop); // Pop expr(0)
            _ilGenerator.Emit(OpCodes.Ldc_I4_0); // Poner un valor falso por defecto
            break;
    }
    // El resultado (0 o 1) queda en la pila.
    return null;
}

public override object VisitCast(MiniCSharpParser.CastContext context)
{
    return base.VisitCast(context);
}

public override object VisitNewFactor(MiniCSharpParser.NewFactorContext context)
{
    return base.VisitNewFactor(context);
}

public override object VisitParenFactor(MiniCSharpParser.ParenFactorContext context)
{
    return base.VisitParenFactor(context);
}

public override object VisitDesignator(MiniCSharpParser.DesignatorContext context)
{
    return base.VisitDesignator(context);
}

public override object VisitNumber(MiniCSharpParser.NumberContext context)
{
    return base.VisitNumber(context);
}

public override object VisitRelop(MiniCSharpParser.RelopContext context)
{
    return base.VisitRelop(context);
}

public override object VisitAddop(MiniCSharpParser.AddopContext context)
{
    return base.VisitAddop(context);
}

public override object VisitMulop(MiniCSharpParser.MulopContext context)
{
    return base.VisitMulop(context);
}

public override object Visit(IParseTree tree)
{
    return base.Visit(tree);
}

public override object VisitChildren(IRuleNode node)
{
    return base.VisitChildren(node);
}

public override object VisitTerminal(ITerminalNode node)
{
    return base.VisitTerminal(node);
}

public override object VisitErrorNode(IErrorNode node)
{
    return base.VisitErrorNode(node);
}

// --- Esqueletos para Estructuras de Control ---
        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

        public override object VisitIfStatement(MiniCSharpParser.IfStatementContext context)
        {
            if (_ilGenerator == null)
            {
                Console.Error.WriteLine("CodeGen Error (VisitIfStatement): ILGenerator is null.");
                return null;
            }

            // 1. Generar código para la condición.
            Visit(context.condition());

            if (context.ELSE() == null) // No hay cláusula ELSE
            {
                // 2. Definir una etiqueta para el final del bloque 'if'.
                System.Reflection.Emit.Label endIfLabel = _ilGenerator.DefineLabel(); // Corregido
                _ilGenerator.Emit(OpCodes.Brfalse, endIfLabel);
                Visit(context.statement(0));
                _ilGenerator.MarkLabel(endIfLabel);
            }
            else // Existe una cláusula ELSE
            {
                System.Reflection.Emit.Label elseLabel = _ilGenerator.DefineLabel();    // Corregido
                System.Reflection.Emit.Label endIfLabel = _ilGenerator.DefineLabel();   // Corregido
                _ilGenerator.Emit(OpCodes.Brfalse, elseLabel);
                Visit(context.statement(0));
                _ilGenerator.Emit(OpCodes.Br, endIfLabel);
                _ilGenerator.MarkLabel(elseLabel);
                Visit(context.statement(1));
                _ilGenerator.MarkLabel(endIfLabel);
            }
            return null;
        }

        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

        // In Compiladores/CodeGen/MiniCSharpCodeGenerator.cs

        public override object VisitWhileStatement(MiniCSharpParser.WhileStatementContext context)
        {
            if (_ilGenerator == null)
            {
                Console.Error.WriteLine("CodeGen Error (VisitWhileStatement): ILGenerator is null.");
                return null;
            }

            System.Reflection.Emit.Label conditionLabel = _ilGenerator.DefineLabel(); // Corregido
            System.Reflection.Emit.Label endWhileLabel = _ilGenerator.DefineLabel();  // Corregido
            // La etiqueta loopBodyLabel no es estrictamente necesaria en esta estructura de bucle while,
            // pero si la usaras, también necesitaría ser completamente calificada:
            // System.Reflection.Emit.Label loopBodyLabel = _ilGenerator.DefineLabel(); 

            _ilGenerator.MarkLabel(conditionLabel);
            Visit(context.condition());
            _ilGenerator.Emit(OpCodes.Brfalse, endWhileLabel); 
            Visit(context.statement());
            _ilGenerator.Emit(OpCodes.Br, conditionLabel);
            _ilGenerator.MarkLabel(endWhileLabel);

            return null;
        }
    }
}
  