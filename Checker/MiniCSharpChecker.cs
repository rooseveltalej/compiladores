using System.Xml;
using Antlr4.Runtime.Tree;
using generated;

namespace Compiladores.Checker
{
    public class MiniCSharpChecker : MiniCSharpParserBaseVisitor<Type>
    {
        private readonly TablaSimbolos _symbolTable;
        public List<string> ErrorMessages { get; } = new List<string>();
        private MethodSymbol _currentMethod = null;
        private VarSymbol _lastDesignatorVarSymbol = null;
        private bool _isDesignatorLValue = false;

        public MiniCSharpChecker()
        {
            _symbolTable = new TablaSimbolos();
        }

        private void AddError(string message, IParseTree context)
        {
            // Podrías mejorar esto para obtener línea/columna:
            // string position = "";
            // if (context is Antlr4.Runtime.ParserRuleContext prc && prc.Start != null)
            // {
            //     position = $" (Line {prc.Start.Line}, Col {prc.Start.Column})";
            // }
            ErrorMessages.Add($"SEMANTIC ERROR: {message} (near \"{context.GetText()}\")");
        }
        
        
        

        public override Type VisitProgram(MiniCSharpParser.ProgramContext context)
        {
            // El ámbito global ya se abrió e inicializó en el constructor de TablaSimbolos.

            string programClassName = context.IDENT().GetText();
            // El programa es una clase, así que creamos un scope para ella.
            // El scope de la clase principal será hijo del scope global.
            var classMembersScope = new Scope(_symbolTable.CurrentScope);
            var classType = new ClassType(programClassName, classMembersScope);
            var classSymbol = new ClassSymbol(programClassName, classType);

            if (!_symbolTable.Insert(classSymbol))
            {
                // Esto sería un error muy extraño si el ámbito global está vacío
                AddError($"Program class '{programClassName}' could not be defined.", context.IDENT());
                return Type.Error;
            }

            _symbolTable.OpenScope(); // Entramos al ámbito de la clase principal

            foreach (var decl in context.varDecl())
            {
                Visit(decl);
            }
            foreach (var class_decl in context.classDecl())
            {
                Visit(class_decl);
            }
            foreach (var method_decl in context.methodDecl())
            {
                Visit(method_decl);
            }

            _symbolTable.CloseScope(); // Salimos del ámbito de la clase principal
            return Type.Void;
        }

        public override Type VisitClassDecl(MiniCSharpParser.ClassDeclContext context)
        {
            string className = context.IDENT().GetText();

            if (_symbolTable.SearchCurrentScope(className) != null)
            {
                AddError($"Class or identifier '{className}' is already declared in this scope.", context.IDENT());
                return Type.Error;
            }

            var classMembersScope = new Scope(_symbolTable.CurrentScope);
            var classType = new ClassType(className, classMembersScope);
            var classSymbol = new ClassSymbol(className, classType);

            if (!_symbolTable.Insert(classSymbol))
            {
                AddError($"Failed to insert class symbol '{className}'.", context.IDENT());
                return Type.Error;
            }

            _symbolTable.OpenScope();
            _currentMethod = null;

            foreach (var varDecl in context.varDecl())
            {
                Visit(varDecl);
            }

            _symbolTable.CloseScope();
            return classType;
        }


        public override Type VisitType(MiniCSharpParser.TypeContext context)
        {
            string typeName = context.IDENT().GetText();
            Symbol typeSymbol = _symbolTable.Search(typeName);

            if (typeSymbol == null || !(typeSymbol is TypeDefSymbol || typeSymbol is ClassSymbol))
            {
                AddError($"Type '{typeName}' not defined.", context.IDENT());
                return Type.Error;
            }

            Type baseType = typeSymbol.Type;

            if (context.LBRACK() != null && context.RBRACK() != null)
            {
                if (!(baseType.Kind == TypeKind.Int || baseType.Kind == TypeKind.Char))  // [cite: 129]
                {
                    AddError($"Arrays can only be of type 'int' or 'char', not '{baseType.Name}'.", context); // [cite: 129]
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
                    AddError($"Identifier '{varName}' already declared in this scope.", identNode); // [cite: 136]
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
                AddError($"Identifier '{methodName}' already declared in this scope.", context.IDENT()); // [cite: 136]
                return Type.Error;
            }

            MethodSymbol methodSymbol = new MethodSymbol(methodName, returnType);
            methodSymbol.Decl = context;

            var previousMethod = _currentMethod; // Guardar método anterior si hay anidamiento (aunque MiniC# lo prohíbe) [cite: 146]
            _currentMethod = methodSymbol;
            _symbolTable.OpenScope();

            if (context.formPars() != null)
            {
                Visit(context.formPars()); // Poblará _currentMethod.Parameters
            }

            foreach(var param in _currentMethod.Parameters)
            {
                if(!_symbolTable.Insert(param))
                {
                    AddError($"Duplicate parameter name '{param.Name}' in method '{methodName}'.", param.Decl);
                }
            }

            Visit(context.block());

            _symbolTable.CloseScope();
            _currentMethod = previousMethod; // Restaurar método anterior

            if (!_symbolTable.Insert(methodSymbol)) // Insertar en el scope de la clase
            {
                AddError($"Could not insert method symbol '{methodName}'.", context.IDENT());
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
                VarSymbol paramSymbol = new VarSymbol(paramName, paramType) { Decl = idents[i].Symbol as Antlr4.Runtime.ParserRuleContext ?? types[i] };
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
            // Primero, evaluamos el designador para obtener su tipo y determinar si es un L-Value.
            // VisitDesignator ya se encarga de setear _lastDesignatorVarSymbol y _isDesignatorLValue.
            Type designatorType = Visit(context.designator());
            VarSymbol targetVar = _lastDesignatorVarSymbol;
            bool isLValue = _isDesignatorLValue;

            if (designatorType.Kind == TypeKind.Error) return Type.Error;

            if (context.ASSIGN() != null) // Asignación: designator = expr
            {
                if (!isLValue)
                {
                    AddError("The left-hand side of an assignment must be a variable, field, or array element.", context.designator());
                    return Type.Error;
                }
                if (targetVar != null && targetVar.Kind == SymbolKind.Constant)
                {
                    AddError($"Cannot assign to '{targetVar.Name}' because it is a constant.", context.designator());
                }
                if (designatorType.Kind == TypeKind.Array && context.expr() != null)
                {
                     // Chequeo: No se permite asignación directa de arreglos [cite: 131]
                    AddError("Direct assignment to arrays is not allowed. Assign to elements or use 'new'.", context.designator());
                }


                Type exprType = Visit(context.expr());
                if (exprType.Kind == TypeKind.Error) return Type.Error;

                if (!AreTypesCompatible(designatorType, exprType))
                {
                    AddError($"Cannot assign type '{exprType}' to '{designatorType}'.", context.expr());
                }
            }
            else if (context.LPAREN() != null) // Llamada a método como statement: designator ( [actPars] )
            {
                 // La lógica de la llamada a método ya fue manejada en VisitDesignatorFactor
                 // que es llamado por VisitDesignator si es un factor.
                 // Aquí el designator es la llamada completa. El 'designatorType' es el tipo de retorno.
                 // No hay chequeo adicional necesario aquí para el statement.
            }
            else if (context.INC() != null || context.DEC() != null) // ++ o --
            {
                if (!isLValue)
                {
                    AddError("The operand of an increment or decrement operator must be a variable, field, or array element.", context.designator());
                }
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
            bool openedScope = (_currentMethod != null || _symbolTable.CurrentScope.Outer != null); // Abrir scope si estamos en método o clase anidada (no el global)
            if (openedScope)
            {
                _symbolTable.OpenScope();
            }

            foreach (var child in context.children.OfType<IParseTree>())
            {
                if (child is MiniCSharpParser.VarDeclContext || child is MiniCSharpParser.StatementContext)
                {
                    Visit(child);
                }
            }

            if (openedScope)
            {
                _symbolTable.CloseScope();
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
            return base.VisitSwitchStatement(context);
        }

        public override Type VisitSwitchCase(MiniCSharpParser.SwitchCaseContext context)
        {
            return base.VisitSwitchCase(context);
        }

        public override Type VisitDefaultCase(MiniCSharpParser.DefaultCaseContext context)
        {
            return base.VisitDefaultCase(context);
        }

        public override Type VisitConstant(MiniCSharpParser.ConstantContext context)
        {
            return base.VisitConstant(context);
        }

        public override Type VisitActPars(MiniCSharpParser.ActParsContext context)
        {
            return base.VisitActPars(context);
        }

        public override Type VisitNumber(MiniCSharpParser.NumberContext context)
        {
            return base.VisitNumber(context);
        }

        public override Type VisitRelop(MiniCSharpParser.RelopContext context)
        {
            return base.VisitRelop(context);
        }

        public override Type VisitAddop(MiniCSharpParser.AddopContext context)
        {
            return base.VisitAddop(context);
        }

        public override Type VisitMulop(MiniCSharpParser.MulopContext context)
        {
            return base.VisitMulop(context);
        }

        public override Type Visit(IParseTree tree)
        {
            return base.Visit(tree);
        }

        public override Type VisitChildren(IRuleNode node)
        {
            return base.VisitChildren(node);
        }

        public override Type VisitTerminal(ITerminalNode node)
        {
            return base.VisitTerminal(node);
        }

        public override Type VisitErrorNode(IErrorNode node)
        {
            return base.VisitErrorNode(node);
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
            _symbolTable.OpenScope(); // Scope para la inicialización del for (si hubiera var decl ahí)

            if (context.expr() != null) Visit(context.expr()); // expr de inicialización
            if (context.condition() != null)
            {
                Type condType = Visit(context.condition());
                if (condType.Kind != TypeKind.Error && condType.Kind != TypeKind.Bool)
                {
                    AddError("For loop condition must be of type 'bool'.", context.condition());
                }
            }
            // El tercer componente del for (ej. i++) es un statement, que se visita.
            // El parser lo tiene como statement? -> "for" "(" expr ";" [ condition ] ";" [ statement ] ")" statement
            // Sí, [statement] es el iterador.

            // No hay una forma directa de acceder al 'statement' opcional del encabezado del for
            // si solo tenemos context.statement() que devuelve el cuerpo.
            // El parser actual tiene: FOR LPAREN expr? SEMI condition? SEMI statement? RPAREN statement
            // Asumiremos que context.statement(0) es el iterador y context.statement(1) es el cuerpo,
            // pero ANTLR genera context.statement() que devuelve una lista si hay múltiples.
            // En nuestro caso, el parser tiene un statement opcional y luego uno obligatorio.
            // Los accederemos por índice si ANTLR los pone en una lista, o directamente.

            // Vamos a asumir que context.statement() da el CUERPO.
            // El 'statement' opcional del header se visitaría si `context.statement().Length > 1` o similar
            // Pero la regla es: `statement? RPAREN statement`.
            // Esto significa que `context.statement(0)` es el opcional y `context.statement(1)` es el cuerpo,
            // o si el opcional no está, `context.statement(0)` es el cuerpo.
            // Esto es ambiguo con la forma en que ANTLR numera.
            // Es más seguro si etiquetamos las partes en el parser:
            // FOR LPAREN initExpr=expr? SEMI cond=condition? SEMI iterStmt=statement? RPAREN bodyStmt=statement

            // Por ahora, solo visitamos el cuerpo. El statement de iteración lo ignoramos en el checker
            // o asumimos que no tiene chequeos semánticos especiales más allá de ser un statement válido.
            Visit(context.statement().Last()); // Asumimos que el último es el cuerpo

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