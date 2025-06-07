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

        // --- NUEVO: Diccionario para almacenar tipos de nodos de expresión ---
        public Dictionary<IParseTree, Type> ExpressionTypes { get; } = new Dictionary<IParseTree, Type>();

        public TablaSimbolos SymbolTable => _symbolTable;
        public string CurrentFilePath => _currentFilePath; 

        public MiniCSharpChecker(CompilationManager manager, TablaSimbolos symbolTable, string filePath)
        {
            _compilationManager = manager ?? throw new ArgumentNullException(nameof(manager));
            _symbolTable = symbolTable ?? new TablaSimbolos();
            _currentFilePath = filePath;
        }

        // --- NUEVO: Helper para guardar y devolver el tipo de un nodo ---
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
            // ... (sin cambios)
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
            // ... (sin cambios en la lógica principal de VisitProgram) ...
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
                return StoreAndReturnType(context, Type.Error); // Guardar tipo de error
            }

            Scope outerScopeBackup = _symbolTable.CurrentScope; 
            int outerLevelBackup = _symbolTable.CurrentLevel;   
            _symbolTable.SetCurrentScopeTo(classMembersScope, outerLevelBackup + 1); 

            foreach (var decl in context.varDecl()) Visit(decl);
            foreach (var class_decl in context.classDecl()) Visit(class_decl);
            foreach (var method_decl in context.methodDecl()) Visit(method_decl);

            _symbolTable.SetCurrentScopeTo(outerScopeBackup, outerLevelBackup);
            
            return StoreAndReturnType(context, Type.Void); // Program como tal no tiene un "tipo" evaluable
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
            
            return StoreAndReturnType(context, classType); // El "tipo" de una declaración de clase es el ClassType mismo
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
    // VisitVarDecl como tal no es una expresión, así que no necesita StoreAndReturnType para el 'VarDeclContext' mismo.
    // Su propósito es declarar variables en la tabla de símbolos.

    // La siguiente llamada a Visit(context.type()) SÍ es importante.
    // Esta llamada despachará a VisitType(TypeContext), y ese método VisitType
    // DEBE usar StoreAndReturnType para almacenar el tipo del nodo TypeContext.
    Type varType = Visit(context.type()); 

    // Si hubo un error al resolver el tipo (VisitType devolvió Type.Error y lo almacenó para el nodo TypeContext),
    // entonces propagamos el error.
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
            
            // --- INICIO DE LA DEPURACIÓN INTEGRADA ---
            Console.WriteLine($"Checker DEBUG: Intentando insertar '{varName}' de tipo '{varType.Name}' en scope nivel {_symbolTable.CurrentLevel} (HashCode Scope: {_symbolTable.CurrentScope.GetHashCode()}). ¿Scope actual ya contiene '{varName}'? {_symbolTable.CurrentScope.Symbols.ContainsKey(varName)}");
            bool inserted = _symbolTable.Insert(varSymbol);
            if (!inserted)
            {
                // Esta condición es teóricamente redundante si SearchCurrentScope funciona bien,
                // pero es una buena doble comprobación.
                Console.WriteLine($"Checker DEBUG: FALLÓ la inserción de '{varName}'. Símbolo existente en current scope: {_symbolTable.SearchCurrentScope(varName)}");
                // Si la inserción falla aquí, significa que TryInsert en TablaSimbolos.Scope devolvió false.
                // Esto sucede si el símbolo YA EXISTE en el Symbols Dictionary del scope actual.
                // El chequeo _symbolTable.SearchCurrentScope(varName) != null ya debería haberlo atrapado.
                // Si llegas aquí, podría indicar una condición de carrera o un problema sutil si los scopes
                // no se gestionan como se espera entre SearchCurrentScope e Insert.
                 AddError($"Identifier '{varName}' could not be inserted, possibly already declared or an internal symbol table error.", identNode);
            }
            else
            {
                Console.WriteLine($"Checker DEBUG: Insertado '{varName}' con nivel {varSymbol.Level} y tipo '{varSymbol.Type.Name}'.");
            }
            // --- FIN DE LA DEPURACIÓN INTEGRADA ---
        }
    }
    // Una declaración de variable no "devuelve" un tipo en el sentido de una expresión, por eso Type.Void.
    return Type.Void; 
}

       public override Type VisitMethodDecl(MiniCSharpParser.MethodDeclContext context)
{
    string methodName = context.IDENT().GetText();
    // El tipo de retorno se visita. Si VisitType usa StoreAndReturnType, el tipo del nodo TypeContext se guardará.
    Type returnType = (context.type() != null) ? Visit(context.type()) : Type.Void;

    if (returnType.Kind == TypeKind.Error) return Type.Error; // Propagar error si la resolución del tipo falló

    // Verificar si el método ya está declarado en el scope actual (de la clase)
    if (_symbolTable.SearchCurrentScope(methodName) != null)
    {
        AddError($"Identifier '{methodName}' already declared in this class scope.", context.IDENT());
        return Type.Error; // No se almacena tipo para la declaración del método en sí si hay error
    }

    MethodSymbol methodSymbol = new MethodSymbol(methodName, returnType);
    methodSymbol.Decl = context; // Asociar el nodo de declaración con el símbolo

    // Guardar el estado del método actual (para anidamiento o simplemente para rastrear)
    var previousMethod = _currentMethod;
    _currentMethod = methodSymbol;
    
    Scope classScope = _symbolTable.CurrentScope; // Guardar el scope de la clase actual
    // int classLevel = _symbolTable.CurrentLevel; // No se usa classLevel directamente aquí

    // --- MODIFICACIÓN AQUÍ: Abrir scope y asociarlo con el MethodDeclContext ---
    // Este nuevo scope será para los parámetros del método y las declaraciones locales en el bloque del método.
    _symbolTable.OpenScope(context); // <<<< PASANDO EL CONTEXT AL ABRIR EL SCOPE

    // Visitar y procesar parámetros formales (si existen)
    // VisitFormPars poblará la lista _currentMethod.Parameters
    if (context.formPars() != null) 
    { 
        Visit(context.formPars()); 
    }

    // Insertar los parámetros (que ahora están en _currentMethod.Parameters) en el nuevo scope del método
    foreach(var param in _currentMethod.Parameters)
    {
        if(!_symbolTable.Insert(param)) // Insert intenta añadir al _symbolTable.CurrentScope
        { 
            // Esto podría ocurrir si hay nombres de parámetros duplicados, lo cual VisitFormPars ya debería haber chequeado.
            AddError($"Duplicate parameter name '{param.Name}' (or failed to insert) in method '{methodName}'.", param.Decl ?? (IParseTree)context.formPars() ?? context.IDENT());
        }
    }
    
    // Visitar el cuerpo del método (block)
    // VisitBlock abrirá su propio scope ANIDADO dentro del scope de este método
    // y las variables locales declaradas en el bloque se insertarán allí.
    Visit(context.block()); 
    
    // Cerrar el scope del método, volviendo al scope de la clase
    _symbolTable.CloseScope(); 
    
    _currentMethod = previousMethod; // Restaurar el método anterior (si hubiera anidamiento o simplemente para limpiar)
    
    // Insertar el símbolo del método en el scope de la clase
    // classScope fue guardado antes de abrir el scope del método.
    // El _symbolTable.Insert usará el _symbolTable.CurrentScope, que ahora debería ser classScope
    // después del CloseScope() anterior.
    if (!classScope.TryInsert(methodSymbol)) // Intenta insertar en el classScope directamente
    {
        // Esto podría fallar si SearchCurrentScope al inicio no fue suficiente
        // o si la gestión de _currentScope en TablaSimbolos no se restauró como se esperaba.
        // Una alternativa más segura sería:
        // if (!_symbolTable.GetScopeForNode(parentClassContext).TryInsert(methodSymbol)) { ... }
        // pero classScope debería ser correcto aquí.
        AddError($"Could not insert method symbol '{methodName}' into class scope. This might indicate an issue with scope restoration or a duplicate not caught earlier.", context.IDENT());
    }
    // La declaración de un método no es una expresión que tenga un tipo evaluable, por eso Type.Void.
    // No llamamos a StoreAndReturnType para el MethodDeclContext completo.
    return Type.Void;
}

        public override Type VisitFormPars(MiniCSharpParser.FormParsContext context)
        {
            // ... (sin cambios, no es una expresión) ...
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

        // --- Métodos Visit* para EXPRESIONES ---
        // Todos estos deben usar StoreAndReturnType


        public override Type VisitNumberFactor(MiniCSharpParser.NumberFactorContext context)
        {
            Type type;
            if (context.number().INTCONST() != null) type = Type.Int;
            else if (context.number().DOUBLECONST() != null) type = Type.Double;
            else { AddError("Unknown number format.", context); type = Type.Error; }
            return StoreAndReturnType(context, type);
        }

        public override Type VisitCharFactor(MiniCSharpParser.CharFactorContext context) => Type.Char;
        public override Type VisitStringFactor(MiniCSharpParser.StringFactorContext context) => Type.String;
        public override Type VisitBoolFactor(MiniCSharpParser.BoolFactorContext context) => Type.Bool;
        public override Type VisitParenFactor(MiniCSharpParser.ParenFactorContext context)
        {
            Type exprType = Visit(context.expr());
            return StoreAndReturnType(context, exprType); // El tipo del factor entre paréntesis es el tipo de la expresión interna
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
            return StoreAndReturnType(context, currentType); // El tipo del designador es el tipo del último elemento accedido
        }


       public override Type VisitDesignatorFactor(MiniCSharpParser.DesignatorFactorContext context)
        {
            Type designatorType = Visit(context.designator()); // Esto ya llamará a StoreAndReturnType para el designador
            if (context.LPAREN() != null) 
            {
                if (designatorType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);
                Symbol potentialMethodSymbol;
                var designatorCtx = context.designator();
                string methodName;
                if (designatorCtx.DOT().Length > 0) 
                {
                     AddError("Complex method calls like 'object.method()' are not fully supported in this checker stage yet.", context);
                     potentialMethodSymbol = _symbolTable.Search(designatorCtx.IDENT().Last().GetText());
                     methodName = designatorCtx.IDENT().Last().GetText();
                } else { 
                     methodName = designatorCtx.IDENT(0).GetText();
                     potentialMethodSymbol = _symbolTable.Search(methodName);
                }
                if (potentialMethodSymbol == null || !(potentialMethodSymbol is MethodSymbol methodSymbol))
                { AddError($"'{methodName}' is not a method or not declared.", context.designator()); return StoreAndReturnType(context, Type.Error); }
                List<Type> actualParamTypes = new List<Type>();
                if (context.actPars() != null)
                { foreach (var exprCtx in context.actPars().expr()) { actualParamTypes.Add(Visit(exprCtx)); } }
                if (methodSymbol.Parameters.Count != actualParamTypes.Count)
                { IParseTree errorContext = context.actPars() != null ? (IParseTree)context.actPars() : (IParseTree)context.LPAREN(); AddError($"Method '{methodSymbol.Name}' expects {methodSymbol.Parameters.Count} arguments, but got {actualParamTypes.Count}.", errorContext); return StoreAndReturnType(context, Type.Error); }
                for (int i = 0; i < methodSymbol.Parameters.Count; i++)
                {
                    if (actualParamTypes[i].Kind == TypeKind.Error) continue; 
                    if (!AreTypesCompatible(methodSymbol.Parameters[i].Type, actualParamTypes[i]))
                    { AddError($"Type mismatch for argument {i + 1} of method '{methodSymbol.Name}'. Expected '{methodSymbol.Parameters[i].Type}', got '{actualParamTypes[i]}'.", context.actPars().expr(i)); }
                }
                return StoreAndReturnType(context, methodSymbol.Type); // El tipo de una llamada a método es su tipo de retorno
            }
            else 
            {
                 Symbol baseDesignatorSymbol = _symbolTable.Search(context.designator().IDENT(0).GetText());
                 if (baseDesignatorSymbol is MethodSymbol && context.designator().ChildCount == 1) 
                 { AddError($"Method name '{context.designator().GetText()}' used as a value without calling it.", context.designator()); return StoreAndReturnType(context, Type.Error); }
                    return StoreAndReturnType(context, designatorType); // Si es solo un designador, su tipo es el que VisitDesignator determinó
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
                    // Para clases, la compatibilidad nominal (mismo nombre de clase) es suficiente para MiniC#
                    return formalType.Name == actualType.Name;
                }
                return true;
            }
            if ((formalType.Kind == TypeKind.Class || formalType.Kind == TypeKind.Array) && actualType.Kind == TypeKind.Null) return true; // [cite: 132, 133]
            // Permitir int a double
            if (formalType.Kind == TypeKind.Double && actualType.Kind == TypeKind.Int) return true;

            return false;
        }


        public override Type VisitDesignatorStatement(MiniCSharpParser.DesignatorStatementContext context)
        {
            // ... (lógica existente, llama a Visit para sub-expresiones, que almacenarán sus tipos) ...
            // Un statement como tal no tiene un tipo que se propague, así que devolvemos Void.
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
                // ... (lógica de asignación de array) ...
                if (designatorType.Kind == TypeKind.Array) { /* ... */ }
                if (!AreTypesCompatible(designatorType, rhsType)) { AddError($"Cannot assign type '{rhsType}' to '{designatorType}'.", context.expr()); }
            }
            else if (context.LPAREN() != null) { /* ... */ }
            else if (context.INC() != null || context.DEC() != null) 
            {
                if (!isLValue) { AddError("The operand of an increment or decrement operator must be a variable, field, or array element.", context.designator()); }
                if (!(designatorType.Kind == TypeKind.Int || designatorType.Kind == TypeKind.Double)) { AddError($"Operator '{(context.INC() != null ? "++" : "--")}' cannot be applied to operand of type '{designatorType}'.", context.designator()); }
            }
            return Type.Void; // Opcional: StoreAndReturnType(context, Type.Void) si alguna vez necesitas el "tipo" de un statement
        }

        public override Type VisitIfStatement(MiniCSharpParser.IfStatementContext context)
        {
            Type conditionType = Visit(context.condition()); // VisitCondition se encargará de StoreAndReturnType para la condición
            if (conditionType.Kind != TypeKind.Error && conditionType.Kind != TypeKind.Bool)
            { AddError("If condition must be of type 'bool'.", context.condition()); }
            Visit(context.statement(0));
            if (context.ELSE() != null) { Visit(context.statement(1)); }
            return Type.Void;
        }

        public override Type VisitWriteStatement(MiniCSharpParser.WriteStatementContext context)
        {
            Type exprType = Visit(context.expr()); // Visit(expr) almacena su propio tipo
            if (exprType.Kind == TypeKind.Error) return Type.Error;
            if (!(exprType.Kind == TypeKind.Int || exprType.Kind == TypeKind.Double || exprType.Kind == TypeKind.Char || exprType.Kind == TypeKind.Bool || exprType.Kind == TypeKind.String))
            { AddError($"Cannot write expression of type '{exprType}'.", context.expr()); }
            if (context.number() != null)
            {
                Type numberType = Visit(context.number()); // VisitNumber (llamado desde Visit(context.number())) almacena su tipo
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

        // En MiniCSharpChecker.cs

// ... (otros métodos y propiedades de la clase) ...

        public override Type VisitBlock(MiniCSharpParser.BlockContext context)
        {
            bool newScopeWasOpenedByThisBlock = true; 
            _symbolTable.OpenScope(context); // <<< MODIFICACIÓN AQUÍ: Se pasa el context

            if (context.children != null) { // Añadido chequeo de nulidad por si acaso
                foreach (var child in context.children.OfType<IParseTree>())
                {
                    // Solo visitar VarDecl y Statement, no los tokens LBRACE/RBRACE directamente
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

            // Comentado para reducir la salida de consola, como solicitaste
            // Console.WriteLine($"\n--- FLAT SYMBOL TABLE - At End of Block (Current Level: {_symbolTable.CurrentLevel}, Context: {context.GetText().Substring(0, Math.Min(context.GetText().Length, 30))}...) ---");
            // _symbolTable.PrintFlatTable(); 
            // Console.WriteLine($"--- End of FLAT SYMBOL TABLE for Block ---\n");

            if (newScopeWasOpenedByThisBlock) 
            { 
                _symbolTable.CloseScope(); 
            }
            return Type.Void; // Un bloque como statement no tiene un tipo que se propague
        }



        public override Type VisitExpr(MiniCSharpParser.ExprContext context)
        {
            Type currentType = Visit(context.term(0)); // Esto ya llama a StoreAndReturnType para el primer término
            if (context.MINUS() != null && currentType.Kind != TypeKind.Error)
            {
                if (!(currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double))
                { AddError($"Unary minus operator cannot be applied to type '{currentType}'.", context.term(0)); return StoreAndReturnType(context, Type.Error); }
                // El tipo no cambia por el menos unario (int sigue int, double sigue double)
            }
            for (int i = 0; i < context.addop().Length; i++)
            {
                if (currentType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);
                string op = context.addop(i).GetText();
                Type rightType = Visit(context.term(i + 1)); // Esto ya llama a StoreAndReturnType para el término derecho
                if (rightType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);

                if (currentType.Kind == TypeKind.Int && rightType.Kind == TypeKind.Int) { currentType = Type.Int; }
                else if ((currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double) && (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double)) { currentType = Type.Double; }
                else if (op == "+" && currentType.Kind == TypeKind.String && rightType.Kind == TypeKind.String) { currentType = Type.String; }
                else if (op == "+" && currentType.Kind == TypeKind.String && (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double || rightType.Kind == TypeKind.Char || rightType.Kind == TypeKind.Bool)) { currentType = Type.String; }
                else if (op == "+" && (currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double || currentType.Kind == TypeKind.Char || currentType.Kind == TypeKind.Bool) && rightType.Kind == TypeKind.String) { currentType = Type.String; }
                else { AddError($"Operator '{op}' cannot be applied to operands of type '{currentType}' and '{rightType}'.", context.addop(i)); return StoreAndReturnType(context, Type.Error); }
            }
            return StoreAndReturnType(context, currentType); // El tipo de la expresión completa
        }

        public override Type VisitTerm(MiniCSharpParser.TermContext context)
        {
            Type currentType = Visit(context.factor(0)); // Esto ya llama a StoreAndReturnType para el primer factor
            for (int i = 0; i < context.mulop().Length; i++)
            {
                if (currentType.Kind == TypeKind.Error) return StoreAndReturnType(context, Type.Error);
                string op = context.mulop(i).GetText();
                Type rightType = Visit(context.factor(i + 1)); // Esto ya llama a StoreAndReturnType para el factor derecho
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
            return StoreAndReturnType(context, currentType); // El tipo del término completo
        }

        public override Type VisitCondFact(MiniCSharpParser.CondFactContext context)
        {
            Type leftType = Visit(context.expr(0));
            Type rightType = Visit(context.expr(1)); // Visit(expr) se encarga de su propio StoreAndReturnType

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
                // No retornamos Type.Error inmediatamente para poder seguir chequeando los cases.
            }

            HashSet<string> caseValuesTexts = new HashSet<string>(); // Para chequear duplicados por texto
            bool defaultFound = false;

            if (context.switchCase() != null)
            {
                foreach (var caseCtx in context.switchCase())
                {
                    Type caseConstantType = Visit(caseCtx.constant()); // Visita el Constant para obtener su tipo
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
                    // Visita los statements dentro del case
                    foreach (var stmt in caseCtx.statement())
                    {
                        Visit(stmt);
                    }
                }
            }

            if (context.defaultCase() != null)
            {
                // La gramática ya asegura 'defaultCase?' (cero o uno)
                // No es necesario 'defaultFound' para chequear duplicados de 'default' en sí.
                Visit(context.defaultCase());
            }
            
            // Podrías añadir aquí una lógica para verificar si los 'break' están bien puestos,
            // si es un requisito de MiniCSharp (C# requiere break o goto al final de cada case no vacío).

            return Type.Void;
        }

        public override Type VisitSwitchCase(MiniCSharpParser.SwitchCaseContext context)
        {
            // La lógica principal está en VisitSwitchStatement.
            // VisitConstant ya fue llamado desde VisitSwitchStatement.
            // Aquí solo visitamos los statements.
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
            if (context.number() != null) { type = Visit(context.number()); } // VisitNumber se encargará
            else if (context.CHARCONST() != null) { type = Type.Char; }
            else { AddError("Unknown constant type.", context); type = Type.Error;}
            return StoreAndReturnType(context, type);
        }


        public override Type VisitActPars(MiniCSharpParser.ActParsContext context)
        {
            // VisitActPars es para la regla de lista de parámetros actuales.
            // El chequeo real de tipos de parámetros ocurre en VisitDesignatorFactor (llamada a método).
            // Aquí, solo necesitamos asegurarnos de que cada expresión de parámetro sea visitada
            // para que se reporten errores dentro de esas expresiones.
            foreach (var expr in context.expr())
            {
                Visit(expr);
            }
            return Type.Void; // actPars como tal no tiene un tipo único.
        }

        public override Type VisitNumber(MiniCSharpParser.NumberContext context)
        {
            Type type;
            if (context.INTCONST() != null) { type = Type.Int; }
            else if (context.DOUBLECONST() != null) { type = Type.Double; }
            else { AddError("Unknown number literal.", context); type = Type.Error; }
            return StoreAndReturnType(context, type);
        }

        public override Type VisitRelop(MiniCSharpParser.RelopContext context)
        {
            return Type.Void; // No tiene un "tipo" en sí mismo.
        }

        public override Type VisitAddop(MiniCSharpParser.AddopContext context)
        {
            return Type.Void; // No tiene un "tipo" en sí mismo.
        }

        public override Type VisitMulop(MiniCSharpParser.MulopContext context)
        {
            return Type.Void; // No tiene un "tipo" en sí mismo.
        }

        public override Type Visit(IParseTree tree)
        {
            if (tree == null) {
                // Esto puede pasar si se llama Visit(context.optionalChild()) y optionalChild no existe.
                // Devolver Type.Error es una opción segura, o un Type específico si el contexto lo permite.
                // Sin embargo, la clase base AbstractParseTreeVisitor ya maneja tree == null y devuelve DefaultResult.
                // Para nosotros, DefaultResult es null, lo cual es el problema.
                // Aquí podríamos forzar Type.Error si se llama con null, aunque no debería ser común
                // si los llamadores verifican los hijos opcionales.
                // AddError("Attempted to visit a null tree node.", null); // El contexto es null aquí
                return Type.Error; 
            }
            return base.Visit(tree); // Esto despachará al método VisitRuleName específico.
        }

        public override Type VisitChildren(IRuleNode node)
        {
            // El base.VisitChildren visita todos los hijos y devuelve el resultado del último hijo visitado,
            // o DefaultResult (null para nosotros) si no hay hijos o si todos devuelven null.
            // Para la mayoría de los "contenedores" (como un bloque de sentencias), Type.Void tiene sentido.
            // Si una regla DEBE producir un tipo (como una expresión), su VisitRuleName debe calcularlo.
            Type result = Type.Void; // Un valor por defecto razonable
            int n = node.ChildCount;
            for (int i = 0; i < n; i++) {
                if (!ShouldVisitNextChild(node, result)) {
                    break;
                }
                IParseTree c = node.GetChild(i);
                Type childResult = Visit(c); // Llama a nuestro Visit(IParseTree) general
                result = AggregateResult(result, childResult);
            }
            return result;
            // NOTA: Podrías simplemente llamar a base.VisitChildren(node) si no necesitas una lógica
            // de agregación o valor por defecto diferente. El problema es si base.VisitChildren
            // devuelve null y el llamador espera un Type.
            // Pero si todos los VisitRuleName devuelven Tipos no-null, esto es menos problemático.
            // Por seguridad, muchos checkers simplemente se aseguran que sus VisitRuleName devuelvan Type.Void
            // para reglas que son contenedores o secuencias.
        }

        // VisitTerminal y VisitErrorNode pueden quedar como están (llamando a base)
        // ya que no suelen participar en la lógica de tipos directamente de la misma manera.
        public override Type VisitTerminal(ITerminalNode node)
        {
            return base.VisitTerminal(node); // O Type.Void si prefieres
        }

        public override Type VisitErrorNode(IErrorNode node) { AddError("Parser error node encountered during semantic check.", node); return Type.Error; }


        public override Type VisitUsingDirective(MiniCSharpParser.UsingDirectiveContext context)
        {
            if (context.qualifiedIdent() != null)
            {
                string moduleName = context.qualifiedIdent().GetText(); // Ej: "MyUtilities"

                // No necesitamos pasar currentFileDirectory explícitamente si CompilationManager lo puede obtener
                // del 'callingChecker.CurrentFilePath'
                ClassSymbol importedClassSymbol = _compilationManager.GetOrCompileModule(moduleName, this);

                if (importedClassSymbol != null)
                {
                    Scope globalScope = _symbolTable.GetGlobalScope();

                    if (globalScope.FindCurrent(moduleName) != null)
                    {
                        Symbol existing = globalScope.FindCurrent(moduleName);
                        // Es un error si ya existe algo con ese nombre que no sea EXACTAMENTE la misma clase importada
                        // (lo cual es difícil de verificar sin comparar identidades de ClassSymbol de diferentes compilaciones)
                        // Una simplificación: si ya existe y es una ClassSymbol con el mismo nombre de tipo, lo consideramos ok.
                        // Si es otro tipo de símbolo, es un error.
                        if (!(existing is ClassSymbol && existing.Type.Name == importedClassSymbol.Name)) {
                           AddError($"Using directive for '{moduleName}' conflicts with an existing global definition of a different kind/type.", context.qualifiedIdent());
                        }
                        // Si ya fue importado y es el mismo, no hacer nada.
                    }
                    else
                    {
                        // Crear un nuevo ClassSymbol en la tabla de ESTE archivo que representa el tipo importado.
                        // Este nuevo símbolo usa el ClassType del símbolo original (que contiene los miembros).
                        ClassSymbol symbolForCurrentTable = new ClassSymbol(importedClassSymbol.Name, (ClassType)importedClassSymbol.Type);
                        symbolForCurrentTable.Level = _symbolTable.GetGlobalScopeLevel(); // Nivel global (0)
                        symbolForCurrentTable.Decl = context; // La 'declaración' es la propia directiva using

                        if (!globalScope.TryInsert(symbolForCurrentTable))
                        {
                            // Esto no debería pasar si FindCurrent era null, pero por si acaso.
                            AddError($"Failed to make type '{moduleName}' available via using directive (unexpected symbol table insertion error).", context.qualifiedIdent());
                        }
                    }
                }
                // Si importedClassSymbol es null, GetOrCompileModule ya habrá añadido el error a este checker.
            }
            return Type.Void;
        }

        public override Type VisitQualifiedIdent(MiniCSharpParser.QualifiedIdentContext context)
        {
            // Un qualifiedIdent (ej: Namespace.Ident) por sí mismo no tiene un tipo
            // hasta que se resuelve en un contexto (ej: como un nombre de tipo, namespace).
            // Generalmente, el método Visit que contiene un qualifiedIdent
            // usará context.qualifiedIdent().GetText() y lo procesará.
            // Si se visita directamente, no hay mucho que hacer semánticamente aquí.
            return Type.Void; // O podrías devolver Type.Error si no se espera que se visite directamente.
        }


        private bool AreTypesCompatibleForEquality(Type t1, Type t2)
        {
            if (t1.Kind == TypeKind.Error || t2.Kind == TypeKind.Error) return true; // No propagar error
            // Misma clase base de tipo
            if (t1.Kind == t2.Kind)
            {
                if (t1.Kind == TypeKind.Class) return t1.Name == t2.Name; // Mismo nombre de clase
                return true;
            }
            // Compatibilidad con null para tipos referencia
            if ((t1.Kind == TypeKind.Class || t1.Kind == TypeKind.Array || t1.Kind == TypeKind.String) && t2.Kind == TypeKind.Null) return true;
            if (t1.Kind == TypeKind.Null && (t2.Kind == TypeKind.Class || t2.Kind == TypeKind.Array || t2.Kind == TypeKind.String)) return true;
            // Permitir comparar int y double
            if ((t1.Kind == TypeKind.Int && t2.Kind == TypeKind.Double) || (t1.Kind == TypeKind.Double && t2.Kind == TypeKind.Int)) return true;

            return false;
        }

        public override Type VisitCondition(MiniCSharpParser.ConditionContext context)
        {
            Type currentType = Visit(context.condTerm(0)); // Visit(condTerm) se encarga
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
            Type currentType = Visit(context.condFact(0)); // Visit(condFact) se encarga
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
                Type exprType = Visit(context.expr()); // StoreAndReturnType se llamará dentro de Visit(expr)
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

        // --- Métodos Visit... que faltan o son más simples ---

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

            var statements = context.statement(); // Esto es un array de StatementContext
            // El statement de iteración es el primero SI hay dos statements.
            // El cuerpo del bucle es el último statement.
            if (statements != null && statements.Length > 0)
            {
                if (statements.Length > 1)
                {
                    Visit(statements[0]); // Visita el statement de iteración (opcional)
                }
                Visit(statements.Last()); // Visita el cuerpo del bucle (obligatorio)
            } else if (statements != null && statements.Length == 1) {
                Visit(statements[0]); // Visita el cuerpo del bucle (obligatorio), no hay iterador.
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
            // Chequear que 'break' esté dentro de un 'for' o 'while'.
            // Esto requiere mantener un contador de anidamiento de bucles o un flag.
            // Por ahora, lo dejamos pasar.
            return Type.Void;
        }


        public override Type VisitEmptyStatement(MiniCSharpParser.EmptyStatementContext context)
        {
            return Type.Void; // No hay nada que chequear
        }

        public override Type VisitBlockStatement(MiniCSharpParser.BlockStatementContext context)
        {
            return Visit(context.block()); // Delega a VisitBlock
        }
        
        // Los métodos para relop, addop, mulop no son necesarios aquí
        // porque su lógica se maneja dentro de VisitExpr, VisitTerm, VisitCondFact.
        // Lo mismo para actPars y number, que se procesan en sus contextos de uso.
        // El 'cast' se omite según los requisitos. [cite: 146]
         public override Type VisitCast(MiniCSharpParser.CastContext context)
        {
            // El casting está excluido de la implementación de generación de código
            // y no se especifican reglas semánticas para él. Podríamos prohibirlo.
            AddError("Casting is not supported in this version of MiniC#.", context);
            return StoreAndReturnType(context, Type.Error);
        }
         
         
    }
    
    
}