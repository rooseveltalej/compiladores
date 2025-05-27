using System.Xml;
using System;
using System.Collections.Generic;
using System.Linq;
using System.IO; // Necesario para Path.GetDirectoryName
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

        public TablaSimbolos SymbolTable => _symbolTable;
        public string CurrentFilePath => _currentFilePath; // Para que CompilationManager sepa el contexto

        public MiniCSharpChecker(CompilationManager manager, TablaSimbolos symbolTable, string filePath)
        {
            _compilationManager = manager ?? throw new ArgumentNullException(nameof(manager));
            _symbolTable = symbolTable ?? new TablaSimbolos();
            _currentFilePath = filePath;
            // ErrorMessages se inicializa arriba
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
            if (!ErrorMessages.Contains(errorMessage)) // Evitar duplicados exactos si se llama varias veces
            {
                ErrorMessages.Add(errorMessage);
            }
        }
        

        public override Type VisitProgram(MiniCSharpParser.ProgramContext context)
        {
            // El ámbito global de _symbolTable ya fue inicializado por su constructor.
            // Asegurarse de que el nivel actual es el global para procesar 'usings'.
            Scope initialGlobalScope = _symbolTable.GetGlobalScope();
            int initialGlobalLevel = _symbolTable.GetGlobalScopeLevel();
            _symbolTable.SetCurrentScopeTo(initialGlobalScope, initialGlobalLevel);


            // 1. Procesar todas las directivas 'using' ANTES de definir la clase principal del archivo actual.
            //    Estas afectarán al ámbito global de la tabla de símbolos de ESTE archivo.
            if (context.usingDirective() != null)
            {
                foreach (var usingDecl in context.usingDirective())
                {
                    Visit(usingDecl); // Llama a VisitUsingDirective
                }
            }

            // 2. Procesar la clase principal definida en ESTE archivo.
            string programClassName = context.IDENT().GetText();
            
            // Crear el Scope para los miembros de la clase Program. Su Outer es el global.
            var classMembersScope = new Scope(_symbolTable.GetGlobalScope()); 
            var classType = new ClassType(programClassName, classMembersScope); 
            var classSymbol = new ClassSymbol(programClassName, classType);
            classSymbol.Decl = context; // O context.IDENT() para la línea exacta

            // Insertar el ClassSymbol de la clase Program en el ámbito global de ESTE archivo.
            if (!_symbolTable.GetGlobalScope().TryInsert(classSymbol))
            {
                // Esto podría pasar si un 'using' ya hizo un tipo con este nombre disponible globalmente.
                Symbol existing = _symbolTable.GetGlobalScope().FindCurrent(programClassName);
                if (existing != null) {
                     AddError($"Program class name '{programClassName}' conflicts with an imported type or another global definition.", context.IDENT());
                } else {
                    AddError($"Program class '{programClassName}' could not be defined in global scope.", context.IDENT());
                }
                return Type.Error;
            }
            // El nivel de classSymbol se asigna por el Insert en TablaSimbolos, debería ser 0 (global)

            // Guardar el estado del ámbito actual (global) para restaurarlo después.
            Scope outerScopeBackup = _symbolTable.CurrentScope; 
            int outerLevelBackup = _symbolTable.CurrentLevel;   

            // Establecer el ámbito actual para que sea el classMembersScope de la clase Program.
            _symbolTable.SetCurrentScopeTo(classMembersScope, outerLevelBackup + 1); // Nivel de la clase

            // Visitar declaraciones de miembros (campos, clases anidadas, métodos)
            foreach (var decl in context.varDecl()) Visit(decl);
            foreach (var class_decl in context.classDecl()) Visit(class_decl);
            foreach (var method_decl in context.methodDecl()) Visit(method_decl);

            // Restaurar el ámbito global.
            _symbolTable.SetCurrentScopeTo(outerScopeBackup, outerLevelBackup);
            
            return Type.Void;
        }

        public override Type VisitClassDecl(MiniCSharpParser.ClassDeclContext context)
        {
            string className = context.IDENT().GetText();
            // _symbolTable.CurrentScope es el ámbito donde esta clase se está declarando (padre)

            if (_symbolTable.SearchCurrentScope(className) != null)
            {
                AddError($"Class or identifier '{className}' is already declared in this scope.", context.IDENT());
                return Type.Error;
            }

            var classMembersScope = new Scope(_symbolTable.CurrentScope);
            var classType = new ClassType(className, classMembersScope);
            var classSymbol = new ClassSymbol(className, classType);
            classSymbol.Decl = context;

            if (!_symbolTable.Insert(classSymbol)) // Inserta en el scope actual (padre)
            {
                AddError($"Failed to insert class symbol '{className}'.", context.IDENT());
                return Type.Error;
            }
            
            Scope outerScopeBackup = _symbolTable.CurrentScope;
            int outerLevelBackup = _symbolTable.CurrentLevel;
            _symbolTable.SetCurrentScopeTo(classMembersScope, outerLevelBackup + 1); // Entra al scope de la nueva clase
            
            _currentMethod = null;

            foreach (var varDecl in context.varDecl()) Visit(varDecl);
            // Si las clases pueden tener métodos:
            // foreach (var methodDeclCtx in context.methodDecl()) Visit(methodDeclCtx);

            _symbolTable.SetCurrentScopeTo(outerScopeBackup, outerLevelBackup); // Restaura scope padre
            
            return classType;
        }


        public override Type VisitType(MiniCSharpParser.TypeContext context)
        {
            string typeName = context.IDENT().GetText();
            Symbol typeSymbol = _symbolTable.Search(typeName); // Búsqueda global para tipos

            if (typeSymbol == null || !(typeSymbol is TypeDefSymbol || typeSymbol is ClassSymbol))
            {
                AddError($"Type '{typeName}' not defined.", context.IDENT());
                return Type.Error;
            }

            Type baseType = typeSymbol.Type;

            if (context.LBRACK() != null && context.RBRACK() != null) // Array
            {
                if (!(baseType.Kind == TypeKind.Int || baseType.Kind == TypeKind.Char))
                {
                    AddError($"Arrays can only be of type 'int' or 'char', not '{baseType.Name}'.", context);
                    return Type.Error;
                }
                return new ArrayType(baseType);
            }
            return baseType;
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

            // Los métodos se buscan en el scope actual (que es el de la clase)
            if (_symbolTable.SearchCurrentScope(methodName) != null)
            {
                AddError($"Identifier '{methodName}' already declared in this class scope.", context.IDENT());
                return Type.Error;
            }

            MethodSymbol methodSymbol = new MethodSymbol(methodName, returnType);
            methodSymbol.Decl = context;

            // Guardar el estado del método y ámbito actuales
            var previousMethod = _currentMethod;
            _currentMethod = methodSymbol;
            
            Scope classScope = _symbolTable.CurrentScope; // El scope de la clase
            int classLevel = _symbolTable.CurrentLevel;

            _symbolTable.OpenScope(); // Abre un NUEVO scope para parámetros y locales del método. Outer es classScope.

            if (context.formPars() != null)
            {
                Visit(context.formPars()); // Poblará _currentMethod.Parameters
            }

            foreach(var param in _currentMethod.Parameters)
            {
                if(!_symbolTable.Insert(param)) // Insertar params en el nuevo scope del método
                {
                    AddError($"Duplicate parameter name '{param.Name}' in method '{methodName}'.", param.Decl);
                }
            }
            
            // ---- OPCIONAL: DEBUG PRINT para el scope del método ----
            // Console.WriteLine($"\n--- Symbol Table entering method '{methodName}' (Method Scope Level: {_symbolTable.CurrentLevel}) ---");
            // _symbolTable.PrintFlatTable(); 
            // Console.WriteLine($"--- End of Symbol Table for method '{methodName}' entry ---\n");
            // ---- FIN DEBUG ----

            Visit(context.block()); // Visitar cuerpo del método

            _symbolTable.CloseScope(); // Cierra el scope del método, _currentScope vuelve a ser classScope

            _currentMethod = previousMethod; // Restaurar método anterior

            // Insertar el MethodSymbol en el scope de la clase (que ahora es _currentScope de nuevo)
            if (!classScope.TryInsert(methodSymbol)) // Asegurarse de insertar en el scope correcto (el de la clase)
            {
                 // Si _symbolTable.Insert() siempre usa _currentScope, entonces esto es correcto.
                 // O usar directamente: if(!_symbolTable.Insert(methodSymbol)) pero verificando que
                 // _currentScope sea el de la clase. La línea anterior es más explícita.
                 // Dado que CloseScope() restaura _currentScope al de la clase, _symbolTable.Insert() debería funcionar.
                if(!_symbolTable.Insert(methodSymbol)) { // Esta línea es redundante si la de arriba funciona.
                     AddError($"Could not insert method symbol '{methodName}' into class scope.", context.IDENT());
                }
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
                {
                    AddError($"Method parameters cannot be of type 'void'. Found for '{idents[i].GetText()}'.", types[i]);
                    continue;
                }

                string paramName = idents[i].GetText();
                if (_currentMethod.Parameters.Any(p => p.Name == paramName))
                {
                    AddError($"Duplicate parameter name '{paramName}' in method '{_currentMethod.Name}'.", idents[i]);
                    continue;
                }
                // Asignar el ITerminalNode como contexto de declaración para el parámetro
                VarSymbol paramSymbol = new VarSymbol(paramName, paramType) { Decl = idents[i].Payload is IToken ? context : idents[i].Parent as ParserRuleContext ?? types[i] };
                if (idents[i].Payload is IToken identTokenPayload) { // Para obtener la línea del token específico
                    paramSymbol.Decl = new TerminalNodeImpl(identTokenPayload).Parent as ParserRuleContext ?? types[i];
                } else { // Fallback
                    paramSymbol.Decl = types[i];
                }


                _currentMethod.Parameters.Add(paramSymbol);
            }
            return Type.Void;
        }

        public override Type VisitNumberFactor(MiniCSharpParser.NumberFactorContext context)
        {
            if (context.number().INTCONST() != null) return Type.Int;
            if (context.number().DOUBLECONST() != null) return Type.Double;
            AddError("Unknown number format.", context);
            return Type.Error;
        }

        public override Type VisitCharFactor(MiniCSharpParser.CharFactorContext context) => Type.Char;
        public override Type VisitStringFactor(MiniCSharpParser.StringFactorContext context) => Type.String;
        public override Type VisitBoolFactor(MiniCSharpParser.BoolFactorContext context) => Type.Bool;
        public override Type VisitParenFactor(MiniCSharpParser.ParenFactorContext context) => Visit(context.expr());

        public override Type VisitDesignator(MiniCSharpParser.DesignatorContext context)
        {
            _lastDesignatorVarSymbol = null;
            _isDesignatorLValue = false;

            string baseName = context.IDENT(0).GetText();
            Symbol symbol = _symbolTable.Search(baseName);

            if (symbol == null)
            {
                AddError($"Identifier '{baseName}' not declared.", context.IDENT(0)); // [cite: 137]
                return Type.Error;
            }

            Type currentType = symbol.Type;
            // Marcar si el símbolo base es un L-Value
            if (symbol.Kind == SymbolKind.Variable || symbol.Kind == SymbolKind.Constant) // Constantes no son L-Values para asignación
            {
                 if (symbol is VarSymbol vs) _lastDesignatorVarSymbol = vs;
                 _isDesignatorLValue = (symbol.Kind != SymbolKind.Constant); // Solo variables son L-Values
            }


            int identIndex = 0; // Para rastrear los IDENT en el designator
            for (int i = 1; i < context.ChildCount; i++) // Empezamos en 1 para saltar el primer IDENT
            {
                var child = context.GetChild(i);
                bool nextIsLValueCandidate = false;

                if (child is ITerminalNode terminalNode && terminalNode.Symbol.Type == MiniCSharpLexer.DOT)
                {
                    identIndex++;
                    if (i + 1 >= context.ChildCount || !(context.GetChild(i + 1) is ITerminalNode memberIdentNode && memberIdentNode.Symbol.Type == MiniCSharpLexer.IDENT))
                    {
                         AddError($"Expected identifier after '.'.", child); return Type.Error;
                    }
                    string memberName = memberIdentNode.GetText();

                    if (currentType.Kind != TypeKind.Class)
                    {
                        AddError($"'{_symbolTable.Search(baseName)?.Name ?? baseName}' is not a class and cannot have members.", context.IDENT(identIndex-1));
                        return Type.Error;
                    }

                    ClassType classType = (ClassType)currentType;
                    Symbol memberSymbol = classType.Members.Find(memberName);

                    if (memberSymbol == null)
                    {
                        AddError($"Class '{classType.Name}' does not have a member named '{memberName}'.", memberIdentNode);
                        return Type.Error;
                    }
                    currentType = memberSymbol.Type;
                    if (memberSymbol.Kind == SymbolKind.Variable || memberSymbol.Kind == SymbolKind.Constant)
                    {
                        if (memberSymbol is VarSymbol mvs) _lastDesignatorVarSymbol = mvs;
                        nextIsLValueCandidate = (memberSymbol.Kind != SymbolKind.Constant);
                    }
                    i++; // Consumimos el IDENT del miembro
                }
                else if (child is ITerminalNode tNodeLbrack && tNodeLbrack.Symbol.Type == MiniCSharpLexer.LBRACK)
                {
                    if (i + 2 >= context.ChildCount || !(context.GetChild(i + 1) is MiniCSharpParser.ExprContext) || !(context.GetChild(i + 2) is ITerminalNode tNodeRbrack && tNodeRbrack.Symbol.Type == MiniCSharpLexer.RBRACK))
                    {
                        AddError("Invalid array access syntax.", child); return Type.Error;
                    }

                    if (currentType.Kind != TypeKind.Array)
                    {
                        AddError($"Identifier '{_symbolTable.Search(baseName)?.Name ?? baseName}' is not an array.", context.IDENT(identIndex));
                        return Type.Error;
                    }

                    Type indexType = Visit(context.expr(context.expr().ToList().IndexOf((MiniCSharpParser.ExprContext)context.GetChild(i+1))));
                    if (indexType.Kind != TypeKind.Int)
                    {
                        AddError("Array index must be an integer.", context.GetChild(i+1));
                    }
                    currentType = ((ArrayType)currentType).ElementType;
                    _lastDesignatorVarSymbol = null; // Elementos de array no son VarSymbol directamente
                    nextIsLValueCandidate = true; // Elementos de array son L-Values
                    i += 2; // Consumimos expr y RBRACK
                }
                _isDesignatorLValue = nextIsLValueCandidate;
            }
            // Aquí podrías guardar 'symbol' o 'currentType' en el contexto del designador si lo necesitas después.
            // (context as Antlr4.Runtime.RuleContext).misc = currentType; // Ejemplo
            return currentType;
        }


       public override Type VisitDesignatorFactor(MiniCSharpParser.DesignatorFactorContext context)
        {
            Type designatorType = Visit(context.designator());

            if (context.LPAREN() != null) // Es una llamada a método: designator ( [actPars] )
            {
                if (designatorType.Kind == TypeKind.Error) return Type.Error;

                // El designador DEBE resolverse a un símbolo de método.
                // El VisitDesignator, si el último componente fue un ident, NO nos dice si es un método.
                // Necesitamos el símbolo del último identificador del designator.
                // Esto es un poco más complejo. Asumimos que el último IDENT del designator es el nombre del método.
                // Esta lógica necesita refinamiento para designadores complejos como obj.method().

                Symbol potentialMethodSymbol;
                var designatorCtx = context.designator();
                string methodName;

                if (designatorCtx.DOT().Length > 0) // obj.method
                {
                    // Necesitamos el tipo del objeto y buscar el método en sus miembros
                    // Esto requiere que VisitDesignator devuelva más información o que lo recalculemos aquí.
                    // Por ahora, es una simplificación y podría fallar para obj.method()
                     AddError("Complex method calls like 'object.method()' are not fully supported in this checker stage yet.", context);
                     potentialMethodSymbol = _symbolTable.Search(designatorCtx.IDENT().Last().GetText());
                     methodName = designatorCtx.IDENT().Last().GetText();

                } else { // method()
                     methodName = designatorCtx.IDENT(0).GetText();
                     potentialMethodSymbol = _symbolTable.Search(methodName);
                }


                if (potentialMethodSymbol == null || !(potentialMethodSymbol is MethodSymbol methodSymbol))
                {
                    AddError($"'{methodName}' is not a method or not declared.", context.designator());
                    return Type.Error;
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
                    // ----- INICIO DE LA CORRECCIÓN -----
                    // Si actPars es null (no se proveyeron argumentos), el error está cerca del '(',
                    // si no, el error está relacionado con la lista de argumentos provista.
                    IParseTree errorContext = context.actPars() != null ? (IParseTree)context.actPars() : (IParseTree)context.LPAREN();
                    AddError($"Method '{methodSymbol.Name}' expects {methodSymbol.Parameters.Count} arguments, but got {actualParamTypes.Count}.", errorContext);
                    // ----- FIN DE LA CORRECCIÓN -----
                    return Type.Error;
                }

                for (int i = 0; i < methodSymbol.Parameters.Count; i++)
                {
                    if (actualParamTypes[i].Kind == TypeKind.Error) continue; // Error ya reportado al visitar la expresión del argumento
                    if (!AreTypesCompatible(methodSymbol.Parameters[i].Type, actualParamTypes[i]))
                    {
                        AddError($"Type mismatch for argument {i + 1} of method '{methodSymbol.Name}'. Expected '{methodSymbol.Parameters[i].Type}', got '{actualParamTypes[i]}'.", context.actPars().expr(i)); // [cite: 138]
                    }
                }
                return methodSymbol.Type; // Tipo de retorno del método
            }
            else // Es solo un designator (variable, campo, etc.)
            {
                // Si es un designador que se resuelve a un tipo método (ej. 'miMetodo' sin '()')
                // esto podría ser un error si el contexto espera un valor.
                // Por ahora, asumimos que si no hay '()', se espera el valor/variable.
                if (designatorType is MethodSymbol) // Aquí designatorType es el TIPO del símbolo, no el símbolo en sí.
                {
                     // Para ser más precisos, necesitaríamos el SÍMBOLO del designador, no solo su tipo.
                     // Si VisitDesignator guardara el último símbolo resuelto en una variable miembro, podríamos usarlo aquí.
                     // O, si VisitDesignator devuelve el símbolo además del tipo.
                     // Asumimos que si el TIPO es Void (típico de un método que no es función) o si el símbolo era un método
                     // y no se está llamando, es un uso incorrecto.
                     // Esta lógica es compleja, ya que un método PUEDE ser un "valor" en lenguajes con delegados/funciones de primera clase,
                     // pero MiniC# no parece tener eso.

                     // Si el tipo del designador es 'Void' (que es el tipo de retorno de un MethodSymbol si el método es void),
                     // y no estamos en una llamada, no tiene sentido.
                     // Sin embargo, un MethodSymbol puede tener un tipo de retorno no-void.
                     // La clave es: si el SÍMBOLO es un MethodSymbol y no hay '()', es un error.

                     // Re-evaluamos el símbolo base del designator
                     Symbol baseDesignatorSymbol = _symbolTable.Search(context.designator().IDENT(0).GetText());
                     if (baseDesignatorSymbol is MethodSymbol && context.designator().ChildCount == 1) // Es solo 'methodName', sin . o []
                     {
                         AddError($"Method name '{context.designator().GetText()}' used as a value without calling it.", context.designator());
                         return Type.Error;
                     }
                }
                return designatorType;
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
    Type designatorType = Visit(context.designator());
    VarSymbol targetVar = _lastDesignatorVarSymbol; // Obtenido de VisitDesignator
    bool isLValue = _isDesignatorLValue;         // Obtenido de VisitDesignator

    if (designatorType.Kind == TypeKind.Error) return Type.Error;

    if (context.ASSIGN() != null) // Es una asignación: designator = expr
    {
        if (!isLValue)
        {
            AddError("The left-hand side of an assignment must be a variable, field, or array element.", context.designator());
            return Type.Error;
        }
        if (targetVar != null && targetVar.Kind == SymbolKind.Constant)
        {
            // Si targetVar es null (ej. para un elemento de array, donde _lastDesignatorVarSymbol se anula),
            // esta comprobación no aplica o necesita un manejo diferente si los elementos de array pudieran ser const.
            // Para MiniC#, los elementos de array no son VarSymbol directamente, así que esto es para variables/campos.
            AddError($"Cannot assign to '{targetVar.Name}' because it is a constant.", context.designator());
            // Considera retornar Type.Error aquí si es un error fatal para la asignación.
        }

        var rhsExprCtx = context.expr(); // La expresión del lado derecho
        if (rhsExprCtx == null) 
        {
            // Esto no debería ocurrir si la gramática es designator ASSIGN expr SEMI,
            // pero es una buena práctica verificarlo.
            AddError("Missing expression on the right-hand side of assignment.", context.ASSIGN());
            return Type.Error;
        }

        Type rhsType = Visit(rhsExprCtx); // Obtenemos el tipo del lado derecho
        if (rhsType.Kind == TypeKind.Error) return Type.Error;

        // Lógica específica para la asignación a arrays
        if (designatorType.Kind == TypeKind.Array)
        {
            bool rhsIsNewArrayAllocation = false;
            // Verificamos si la estructura de la expresión RHS es "new IDENT [ expr ]"
            // Esto significa que la expresión no tiene operadores de mayor precedencia
            // y se reduce a un solo factor, que es un NewFactor de array.
            if (rhsExprCtx.MINUS() == null && 
                rhsExprCtx.cast() == null &&
                rhsExprCtx.addop().Length == 0 &&
                rhsExprCtx.term().Length == 1)
            {
                var termCtx = rhsExprCtx.term(0);
                if (termCtx.mulop().Length == 0 &&
                    termCtx.factor().Length == 1)
                {
                    var factorCtx = termCtx.factor(0);
                    // Comprobamos si el factor es un NewFactor y si tiene LBRACK (es decir, es new T[...])
                    if (factorCtx is MiniCSharpParser.NewFactorContext newFactorCtx && newFactorCtx.LBRACK() != null)
                    {
                        rhsIsNewArrayAllocation = true;
                    }
                }
            }

            if (!rhsIsNewArrayAllocation)
            {
                // Si el lado izquierdo es un array, pero el lado derecho NO ES una expresión "new Tipo[tamaño]",
                // entonces aplicamos la regla de "no asignación directa".
                // Esto cubriría casos como array1 = array2; o array1 = funcQueDevuelveArray();
                AddError("Direct assignment to array variables is only allowed using a 'new' array allocation expression (e.g., arr = new int[5];). Assigning array elements is allowed (e.g., arr[0] = 10;).", context.designator());
                // Podrías decidir si esto es un error que detiene la compatibilidad de tipos o no.
                // Por ahora, se reporta y la comprobación de tipos sigue.
            }
            // Si rhsIsNewArrayAllocation == true, significa que es arrayVar = new Tipo[tamaño];
            // En este caso, no emitimos el error de "asignación directa no permitida" y
            // procedemos a la verificación de compatibilidad de tipos más abajo.
        }

        // Verificación general de compatibilidad de tipos para la asignación
        if (!AreTypesCompatible(designatorType, rhsType))
        {
            AddError($"Cannot assign type '{rhsType}' to '{designatorType}'.", context.expr());
        }
    }
    else if (context.LPAREN() != null) // Llamada a método como statement: designator ( [actPars] )
    {
        // La validación de la llamada a método ya está (o debería estar) en VisitDesignatorFactor.
        // Visit(context.designator()) ya evaluó el designator, que si es una llamada,
        // su tipo (el tipo de retorno del método) fue determinado.
        // Aquí solo nos aseguramos de que la llamada en sí sea válida si no lo fue ya.
        // Si designatorType es Error, el error ya fue reportado.
        // Si no, la llamada se considera un statement válido.
    }
    else if (context.INC() != null || context.DEC() != null) // ++ o --
    {
        if (!isLValue)
        {
            AddError("The operand of an increment or decrement operator must be a variable, field, or array element.", context.designator());
        }
        // La comprobación de tipo (int o double) ya la tienes en tu código original.
        if (!(designatorType.Kind == TypeKind.Int || designatorType.Kind == TypeKind.Double))
        {
            AddError($"Operator '{(context.INC() != null ? "++" : "--")}' cannot be applied to operand of type '{designatorType}'.", context.designator());
        }
    }
    return Type.Void;
}

        public override Type VisitIfStatement(MiniCSharpParser.IfStatementContext context)
        {
            Type conditionType = Visit(context.condition());
            if (conditionType.Kind != TypeKind.Error && conditionType.Kind != TypeKind.Bool)
            {
                AddError("If condition must be of type 'bool'.", context.condition());
            }
            Visit(context.statement(0));
            if (context.ELSE() != null)
            {
                Visit(context.statement(1));
            }
            return Type.Void;
        }

        public override Type VisitWriteStatement(MiniCSharpParser.WriteStatementContext context)
        {
            Type exprType = Visit(context.expr());
            if (exprType.Kind == TypeKind.Error) return Type.Error;

            if (!(exprType.Kind == TypeKind.Int ||
                  exprType.Kind == TypeKind.Double ||
                  exprType.Kind == TypeKind.Char ||
                  exprType.Kind == TypeKind.Bool ||
                  exprType.Kind == TypeKind.String))
            {
                AddError($"Cannot write expression of type '{exprType}'.", context.expr());
            }

            if (context.number() != null)
            {
                Type numberType = Visit(context.number());
                if (numberType.Kind != TypeKind.Int)
                {
                     AddError("Format specifier for 'write' (if present) must be an integer.", context.number());
                }
            }
            return Type.Void;
        }

        public override Type VisitReturnStatement(MiniCSharpParser.ReturnStatementContext context)
        {
            if (_currentMethod == null)
            {
                AddError("Return statement found outside of a method.", context); // [cite: 139] (implícito)
                return Type.Error;
            }

            if (context.expr() != null)
            {
                Type returnExprType = Visit(context.expr());
                if (returnExprType.Kind == TypeKind.Error) return Type.Error;

                if (_currentMethod.Type.Kind == TypeKind.Void)
                {
                    AddError($"Method '{_currentMethod.Name}' is void and cannot return a value.", context.expr()); // [cite: 139]
                }
                else if (!AreTypesCompatible(_currentMethod.Type, returnExprType))
                {
                    AddError($"Cannot return type '{returnExprType}' from method '{_currentMethod.Name}' expecting '{_currentMethod.Type}'.", context.expr()); // [cite: 140]
                }
            }
            else // return;
            {
                if (_currentMethod.Type.Kind != TypeKind.Void)
                {
                    AddError($"Method '{_currentMethod.Name}' expects a return value of type '{_currentMethod.Type}'.", context); // [cite: 140]
                }
            }
            return Type.Void;
        }

        public override Type VisitBlock(MiniCSharpParser.BlockContext context)
        {
            // Determinar si este bloque debe abrir un nuevo ámbito.
            // Si este VisitBlock es para el cuerpo principal de un método, el ámbito para
            // parámetros y variables locales directas del método ya fue abierto en VisitMethodDecl.
            // Un bloque {} anidado sí debería abrir un nuevo ámbito.

            // Lógica simplificada actual: cada bloque abre un ámbito.
            // Esto significa que el cuerpo de un método (que es un bloque) también abrirá un
            // ámbito anidado dentro del ámbito que VisitMethodDecl ya pudo haber abierto para parámetros.
            bool newScopeWasOpenedByThisBlock = true; // Asumimos que siempre abrimos para CUALQUIER bloque
            _symbolTable.OpenScope(); 

            foreach (var child in context.children.OfType<IParseTree>())
            {
                if (child is MiniCSharpParser.VarDeclContext || child is MiniCSharpParser.StatementContext)
                {
                    Visit(child); // Las variables locales se insertan en el ámbito recién abierto
                }
            }

            // ----- INICIO DE LA IMPRESIÓN DE DEBUG DENTRO DEL BLOQUE -----
            // Imprimir la tabla de símbolos ANTES de cerrar el ámbito de este bloque.
            // _currentScope y _currentLevel apuntan ahora al ámbito de este bloque.
            Console.WriteLine($"\n--- FLAT SYMBOL TABLE - At End of Block (Current Level: {_symbolTable.CurrentLevel}, Context: {context.GetText().Substring(0, Math.Min(context.GetText().Length, 30))}...) ---");
            _symbolTable.PrintFlatTable(); // Debería mostrar locales de este bloque y luego los externos
            Console.WriteLine($"--- End of FLAT SYMBOL TABLE for Block ---\n");
            // ----- FIN DE LA IMPRESIÓN DE DEBUG -----

            if (newScopeWasOpenedByThisBlock) // O simplemente `_symbolTable.CloseScope();` si siempre abres
            {
                _symbolTable.CloseScope(); // Cerrar el ámbito que este bloque abrió
            }
    
            return Type.Void;
        }


        public override Type VisitExpr(MiniCSharpParser.ExprContext context)
        {
            Type currentType = Visit(context.term(0));

            // Manejar operador unario '-'
            if (context.MINUS() != null && currentType.Kind != TypeKind.Error)
            {
                if (!(currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double))
                {
                    AddError($"Unary minus operator cannot be applied to type '{currentType}'.", context.term(0));
                    return Type.Error;
                }
            }

            for (int i = 0; i < context.addop().Length; i++)
            {
                if (currentType.Kind == TypeKind.Error) return Type.Error;

                string op = context.addop(i).GetText();
                Type rightType = Visit(context.term(i + 1));

                if (rightType.Kind == TypeKind.Error) return Type.Error;

                if (currentType.Kind == TypeKind.Int && rightType.Kind == TypeKind.Int)
                {
                    currentType = Type.Int; // [cite: 123]
                }
                else if ((currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double) &&
                         (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double))
                {
                    currentType = Type.Double;
                }
                else if (op == "+" && currentType.Kind == TypeKind.String && rightType.Kind == TypeKind.String)
                {
                    currentType = Type.String;
                }
                 // Permitir string + cualquier_tipo_simple (se convierte a string)
                else if (op == "+" && currentType.Kind == TypeKind.String && (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double || rightType.Kind == TypeKind.Char || rightType.Kind == TypeKind.Bool))
                {
                    currentType = Type.String;
                }
                else if (op == "+" && (currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double || currentType.Kind == TypeKind.Char || currentType.Kind == TypeKind.Bool) && rightType.Kind == TypeKind.String)
                {
                    currentType = Type.String;
                }
                else
                {
                    AddError($"Operator '{op}' cannot be applied to operands of type '{currentType}' and '{rightType}'.", context.addop(i));
                    return Type.Error;
                }
            }
            return currentType;
        }

        public override Type VisitTerm(MiniCSharpParser.TermContext context)
        {
            Type currentType = Visit(context.factor(0));

            for (int i = 0; i < context.mulop().Length; i++)
            {
                if (currentType.Kind == TypeKind.Error) return Type.Error;

                string op = context.mulop(i).GetText();
                Type rightType = Visit(context.factor(i + 1));

                if (rightType.Kind == TypeKind.Error) return Type.Error;

                if (currentType.Kind == TypeKind.Int && rightType.Kind == TypeKind.Int)
                {
                    // Para '/', la división de enteros podría dar double o int. La definición semántica dice "devuelve valores enteros".
                    // Para MiniC#, asumiremos que int/int da int (división entera).
                    currentType = Type.Int; // [cite: 123]
                }
                else if ((currentType.Kind == TypeKind.Int || currentType.Kind == TypeKind.Double) &&
                         (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double))
                {
                    if (op == "%" && (currentType.Kind == TypeKind.Double || rightType.Kind == TypeKind.Double))
                    {
                        AddError("Operator '%' is not typically defined for double operands in this context.", context.mulop(i));
                        return Type.Error;
                    }
                    currentType = Type.Double;
                }
                else
                {
                    AddError($"Operator '{op}' cannot be applied to operands of type '{currentType}' and '{rightType}'.", context.mulop(i));
                    return Type.Error;
                }
            }
            return currentType;
        }

        public override Type VisitCondFact(MiniCSharpParser.CondFactContext context)
        {
            Type leftType = Visit(context.expr(0));
            Type rightType = Visit(context.expr(1));

            if (leftType.Kind == TypeKind.Error || rightType.Kind == TypeKind.Error) return Type.Error;

            var relopNode = context.relop();
            bool compatible = false;

            if (relopNode.EQUAL() != null || relopNode.NOTEQUAL() != null)
            {
                compatible = AreTypesCompatibleForEquality(leftType, rightType); // [cite: 125]
            }
            else // <, <=, >, >=
            {
                if ((leftType.Kind == TypeKind.Int || leftType.Kind == TypeKind.Double || leftType.Kind == TypeKind.Char) &&
                    (rightType.Kind == TypeKind.Int || rightType.Kind == TypeKind.Double || rightType.Kind == TypeKind.Char))
                {
                    // Permitir comparación entre números y/o char (se comparan sus valores ordinales)
                    compatible = true; // [cite: 125]
                }
            }

            if (!compatible)
            {
                AddError($"Cannot compare types '{leftType}' and '{rightType}' with operator '{relopNode.GetText()}'.", context.relop());
                return Type.Error;
            }
            return Type.Bool; // [cite: 126]
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
            if (context.number() != null)
            {
                return Visit(context.number()); // Delega a VisitNumber
            }
            if (context.CHARCONST() != null)
            {
                return Type.Char;
            }
            AddError("Unknown constant type.", context); // No debería llegar aquí con la gramática actual
            return Type.Error;
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
            // Este método es llamado por VisitConstant y VisitNumberFactor
            if (context.INTCONST() != null)
            {
                return Type.Int;
            }
            if (context.DOUBLECONST() != null)
            {
                return Type.Double;
            }
            AddError("Unknown number literal.", context); // No debería llegar aquí
            return Type.Error;
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

        public override Type VisitErrorNode(IErrorNode node)
        {
            // Podrías devolver Type.Error aquí explícitamente.
            AddError("Parser error node encountered during semantic check.", node);
            return Type.Error;
        }

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
            Type currentType = Visit(context.condTerm(0));
            for (int i = 0; i < context.OR().Length; i++)
            {
                if (currentType.Kind != TypeKind.Error && currentType.Kind != TypeKind.Bool)
                {
                    AddError("Left operand of '||' must be of type bool.", context.condTerm(i));
                }
                Type rightType = Visit(context.condTerm(i + 1));
                if (rightType.Kind != TypeKind.Error && rightType.Kind != TypeKind.Bool)
                {
                    AddError("Right operand of '||' must be of type bool.", context.condTerm(i + 1));
                }
                if (currentType.Kind == TypeKind.Bool && rightType.Kind == TypeKind.Bool)
                {
                    currentType = Type.Bool; // [cite: 126]
                }
                else
                {
                    currentType = Type.Error; // Si alguno no es bool (y no es error ya), el resultado es error
                }
            }
            return currentType;
        }

        public override Type VisitCondTerm(MiniCSharpParser.CondTermContext context)
        {
            Type currentType = Visit(context.condFact(0));
            for (int i = 0; i < context.AND().Length; i++)
            {
                if (currentType.Kind != TypeKind.Error && currentType.Kind != TypeKind.Bool)
                {
                    AddError("Left operand of '&&' must be of type bool.", context.condFact(i));
                }
                Type rightType = Visit(context.condFact(i + 1));
                if (rightType.Kind != TypeKind.Error && rightType.Kind != TypeKind.Bool)
                {
                    AddError("Right operand of '&&' must be of type bool.", context.condFact(i + 1));
                }

                if (currentType.Kind == TypeKind.Bool && rightType.Kind == TypeKind.Bool)
                {
                    currentType = Type.Bool; // [cite: 126]
                }
                else
                {
                    currentType = Type.Error;
                }
            }
            return currentType;
        }

         public override Type VisitNewFactor(MiniCSharpParser.NewFactorContext context)
        {
            string typeName = context.IDENT().GetText();
            Symbol typeSymbol = _symbolTable.Search(typeName);

            if (typeSymbol == null)
            {
                AddError($"Type '{typeName}' not found for 'new' operator.", context.IDENT());
                return Type.Error;
            }

            if (context.LBRACK() != null) // new ident [ expr ]  (Creación de arreglo)
            {
                if (!(typeSymbol.Type.Kind == TypeKind.Int || typeSymbol.Type.Kind == TypeKind.Char))
                {
                     AddError($"Can only create arrays of 'int' or 'char', not '{typeName}'.", context.IDENT()); // [cite: 129]
                     return Type.Error;
                }
                Type exprType = Visit(context.expr());
                if(exprType.Kind != TypeKind.Int)
                {
                    AddError("Array size specifier must be an integer.", context.expr());
                }
                return new ArrayType(typeSymbol.Type);
            }
            else // new ident () o new ident (Creación de clase)
            {
                if (!(typeSymbol is ClassSymbol))
                {
                    AddError($"'{typeName}' is not a class type and cannot be instantiated with 'new'.", context.IDENT());
                    return Type.Error;
                }
                // La gramática permite new ident (sin paréntesis) para clases, o new ident LPAREN RPAREN
                // El chequeo de constructores (si existieran explícitamente) iría aquí.
                // MiniC# no define constructores explícitos, así que solo instanciamos.
                return typeSymbol.Type; // Retorna el ClassType
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
            return Type.Error;
        }
         
         
    }
    
    
}