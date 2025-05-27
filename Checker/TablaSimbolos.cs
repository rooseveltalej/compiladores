// Archivo: Compiladores/Checker/TablaSimbolos.cs

using System.Collections.Generic;
using System.Text; 
using System.Linq; 
using Antlr4.Runtime; 
using System; 

namespace Compiladores.Checker
{
    // --- Representación de Tipos ---

    public enum TypeKind { Int, Double, Char, Bool, String, Array, Class, Void, Null, Error }

    public abstract class Type
    {
        public TypeKind Kind { get; protected set; }
        public string Name { get; protected set; }

        protected Type(string name) { Name = name; }

        public static readonly Type Int = new PrimitiveType("int", TypeKind.Int);
        public static readonly Type Double = new PrimitiveType("double", TypeKind.Double);
        public static readonly Type Char = new PrimitiveType("char", TypeKind.Char);
        public static readonly Type Bool = new PrimitiveType("bool", TypeKind.Bool);
        public static readonly Type String = new PrimitiveType("string", TypeKind.String);
        public static readonly Type Void = new PrimitiveType("void", TypeKind.Void);
        public static readonly Type Null = new PrimitiveType("null", TypeKind.Null);
        public static readonly Type Error = new PrimitiveType("error", TypeKind.Error);

        public override string ToString() => Name;
    }

    public class PrimitiveType : Type
    {
        public PrimitiveType(string name, TypeKind kind) : base(name)
        {
            Kind = kind;
        }
    }

    public class ArrayType : Type
    {
        public Type ElementType { get; }

        public ArrayType(Type elementType) : base($"{elementType.Name}[]")
        {
            Kind = TypeKind.Array;
            ElementType = elementType;
        }
    }

    public class ClassType : Type
    {
        public Scope Members { get; } 

        public ClassType(string name, Scope members) : base(name)
        {
            Kind = TypeKind.Class;
            Members = members;
        }
    }

    // --- Representación de Símbolos ---

    public enum SymbolKind { Variable, Constant, Method, Class, TypeDef }

    public abstract class Symbol
    {
        public string Name { get; }
        public Type Type { get; }
        public SymbolKind Kind { get; }
        public int Level { get; set; } 
        public ParserRuleContext Decl { get; set; } 

        protected Symbol(string name, Type type, SymbolKind kind)
        {
            Name = name;
            Type = type;
            Kind = kind;
        }
        public override string ToString() => $"{Kind} {Name}: {Type}";
    }

    public class VarSymbol : Symbol
    {
        // isConst se infiere del SymbolKind.Kind en la clase base si se establece en el constructor.
        public VarSymbol(string name, Type type, bool isConst = false)
            : base(name, type, isConst ? SymbolKind.Constant : SymbolKind.Variable) { }
    }

    public class MethodSymbol : Symbol
    {
        public List<VarSymbol> Parameters { get; } = new List<VarSymbol>(); 

        public MethodSymbol(string name, Type returnType)
            : base(name, returnType, SymbolKind.Method) { }
    }

    public class ClassSymbol : Symbol
    {
        public ClassSymbol(string name, ClassType type)
            : base(name, type, SymbolKind.Class) { }
    }
     public class TypeDefSymbol : Symbol
    {
        public TypeDefSymbol(string name, Type type)
            : base(name, type, SymbolKind.TypeDef) { }
    }


    // --- Manejo de Ámbitos (Scopes) ---

    public class Scope
    {
        public Scope Outer { get; } 
        public Dictionary<string, Symbol> Symbols { get; } = new Dictionary<string, Symbol>(); 

        public Scope(Scope outer = null)
        {
            Outer = outer;
        }

        public bool TryInsert(Symbol symbol)
        {
            if (Symbols.ContainsKey(symbol.Name))
            {
                return false; 
            }
            Symbols.Add(symbol.Name, symbol);
            return true;
        }

        public Symbol Find(string name)
        {
            if (Symbols.TryGetValue(name, out Symbol symbol))
            {
                return symbol;
            }
            return Outer?.Find(name); 
        }

        public Symbol FindCurrent(string name)
        {
             Symbols.TryGetValue(name, out Symbol symbol);
             return symbol;
        }
    }

    // --- La Tabla de Símbolos Principal ---

    public class TablaSimbolos
    {
        private Scope _currentScope;
        private int _currentLevel;
        private Scope _globalScope; 

        public int CurrentLevel => _currentLevel;
        public Scope CurrentScope => _currentScope;
        
        // Método para establecer el ámbito actual explícitamente (usado por el Checker)
        public void SetCurrentScopeTo(Scope specificScope, int newLevel)
        {
            _currentScope = specificScope;
            _currentLevel = newLevel;
        }

        public TablaSimbolos()
        {
            _currentScope = null;
            _currentLevel = -1;
            Init();
        }

        private void Init()
        {
            OpenScope(); 
            _globalScope = _currentScope; 

            Insert(new TypeDefSymbol("int", Type.Int));
            Insert(new TypeDefSymbol("double", Type.Double));
            Insert(new TypeDefSymbol("char", Type.Char));
            Insert(new TypeDefSymbol("bool", Type.Bool));
            Insert(new TypeDefSymbol("string", Type.String));
            Insert(new TypeDefSymbol("void", Type.Void));
            Insert(new VarSymbol("null", Type.Null, isConst: true));
            var chrMethod = new MethodSymbol("chr", Type.Char);
            chrMethod.Parameters.Add(new VarSymbol("i", Type.Int));
            Insert(chrMethod);
            var ordMethod = new MethodSymbol("ord", Type.Int);
            ordMethod.Parameters.Add(new VarSymbol("ch", Type.Char));
            Insert(ordMethod);
            var lenMethod = new MethodSymbol("len", Type.Int);
            lenMethod.Parameters.Add(new VarSymbol("a", new ArrayType(Type.Error)));
            Insert(lenMethod);
        }

        public bool Insert(Symbol symbol)
        {
            if (_currentScope == null) return false; 
            if (!_currentScope.TryInsert(symbol))
            {
                return false;
            }
            symbol.Level = _currentLevel; 
            return true;
        }

        public Symbol Search(string name)
        {
            return _currentScope?.Find(name);
        }

        public Symbol SearchCurrentScope(string name)
        {
            return _currentScope?.FindCurrent(name);
        }

        public void OpenScope()
        {
            _currentScope = new Scope(_currentScope); 
            _currentLevel++; 
        }

        public void CloseScope()
        {
            if (_currentScope != null)
            {
                _currentScope = _currentScope.Outer; 
                _currentLevel--; 
            }
        }
        
        // --- NUEVO MÉTODO DE IMPRESIÓN PLANA ---
        public void PrintFlatTable()
        {
            Console.WriteLine("===== FLAT SYMBOL TABLE (Visible from Current Scope, Level: " + _currentLevel + ") =====");
            Console.WriteLine("{0,-20} {1,-10} {2,-15} {3,-15} {4,-10}", "Nombre", "Nivel Decl", "Tipo", "Clasificación", "Linea Decl");
            Console.WriteLine(new string('-', 75)); // Línea separadora

            Scope currentVisibleScope = _currentScope;
            List<string> printedSymbols = new List<string>(); // Para evitar duplicados si un nombre sombrea a otro

            while (currentVisibleScope != null)
            {
                // Imprimir símbolos del scope actual si no han sido sombreados por un scope más interno
                foreach (var symbol in currentVisibleScope.Symbols.Values.OrderBy(s => s.Name))
                {
                    if (!printedSymbols.Contains(symbol.Name)) // Solo imprimir si no ha sido impreso (no sombreado)
                    {
                        string nombre = symbol.Name;
                        int nivel = symbol.Level; // Nivel donde fue declarado
                        string tipo = symbol.Type?.Name ?? "desconocido";
                        string clasificacion = symbol.Kind switch
                        {
                            SymbolKind.Constant => "Constante",
                            SymbolKind.Variable => "Variable",
                            SymbolKind.Method => "Método",
                            SymbolKind.Class => "Clase",
                            SymbolKind.TypeDef => "TipoDef",
                            _ => "Desconocido"
                        };
                        string lineaDecl = symbol.Decl?.Start?.Line.ToString() ?? "N/A";

                        Console.WriteLine("{0,-20} {1,-10} {2,-15} {3,-15} {4,-10}", nombre, nivel, tipo, clasificacion, lineaDecl);
                        printedSymbols.Add(symbol.Name); // Marcar como impreso
                    }
                }
                currentVisibleScope = currentVisibleScope.Outer; // Mover al ámbito exterior
            }

            Console.WriteLine(new string('=', 75));
        }
        
        public Symbol SearchGlobal(string name) {
            if (_globalScope != null) {
                return _globalScope.FindCurrent(name); // Busca solo en el nivel 0
            }
            return null;
        }
        
        public Scope GetGlobalScope() => _globalScope;
        public int GetGlobalScopeLevel() => 0; // Asumiendo que el nivel global es 0


        // --- MÉTODO DE IMPRESIÓN JERÁRQUICA (COMENTADO O PUEDES MANTENERLO) ---
        /*
        public void PrintToConsole()
        {
            Console.WriteLine("===== HIERARCHICAL SYMBOL TABLE (C#) =====");
            if (_globalScope != null)
            {
                PrintScopeRecursiveToConsole(_globalScope, "");
            }
            else
            {
                Scope tempScope = _currentScope;
                if (tempScope != null) {
                    while(tempScope.Outer != null) {
                        tempScope = tempScope.Outer;
                    }
                    PrintScopeRecursiveToConsole(tempScope, "");
                } else {
                    Console.WriteLine("Symbol table current scope is null and global scope not found.");
                }
            }
            Console.WriteLine("===========================");
        }

        private void PrintScopeRecursiveToConsole(Scope scope, string indent)
        {
            if (scope == null) return;

            int level = 0;
            Scope temp = scope;
            while (temp.Outer != null)
            {
                level++;
                temp = temp.Outer;
            }

            Console.WriteLine($"{indent}--- Scope Level: {level} (ID: {scope.GetHashCode()}) ---");

            if (!scope.Symbols.Any())
            {
                Console.WriteLine($"{indent}  (No symbols in this direct scope)");
            }

            foreach (var symbol in scope.Symbols.Values.OrderBy(s => s.Kind).ThenBy(s => s.Name))
            {
                string lineText = symbol.Decl?.Start?.Line.ToString() ?? "N/A";
                Console.Write($"{indent}  > {symbol.Kind} '{symbol.Name}' : {symbol.Type.Name} (Declared at Level: {symbol.Level}, Line: {lineText})");
                Console.WriteLine(); 

                if (symbol is MethodSymbol methodSymbol)
                {
                    if (methodSymbol.Parameters.Any())
                    {
                        Console.WriteLine($"{indent}    Parameters:");
                        foreach (var param in methodSymbol.Parameters)
                        {
                             string paramLine = param.Decl?.Start?.Line.ToString() ?? "N/A";
                             Console.WriteLine($"{indent}      - {param.Kind} '{param.Name}' : {param.Type.Name} (Line: {paramLine})");
                        }
                    }
                    else
                    {
                        Console.WriteLine($"{indent}    Parameters: None");
                    }
                }
                else if (symbol is ClassSymbol classSymbol && classSymbol.Type is ClassType classType)
                {
                    Console.WriteLine($"{indent}    Members of class '{classSymbol.Name}':");
                    PrintScopeRecursiveToConsole(classType.Members, indent + "    ");
                }
            }
        }
        */
    }
}