// Archivo: Compiladores/Checker/TablaSimbolos.cs

// Archivo: Compiladores/Checker/TablaSimbolos.cs

// Archivo: Compiladores/Checker/TablaSimbolos.cs

using System.Collections.Generic;
using System.Text; 
using System.Linq; 
using Antlr4.Runtime;
using Antlr4.Runtime.Tree; // Necesario para IParseTree

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

        // --- NUEVO: Diccionario para mapear nodos del AST a sus Scopes ---
        private Dictionary<IParseTree, Scope> _nodeScopes = new Dictionary<IParseTree, Scope>();

        public int CurrentLevel => _currentLevel;
        public Scope CurrentScope => _currentScope;
        
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
            OpenScope(); // Abre el scope global (_globalScope)
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

            var addMethod = new MethodSymbol("add", Type.Void); 
            addMethod.Parameters.Add(new VarSymbol("list", new ArrayType(Type.Error))); 
            addMethod.Parameters.Add(new VarSymbol("element", Type.Error)); 
            Insert(addMethod);

            var delMethod = new MethodSymbol("del", Type.Void); 
            delMethod.Parameters.Add(new VarSymbol("list", new ArrayType(Type.Error))); 
            delMethod.Parameters.Add(new VarSymbol("index", Type.Int)); 
            Insert(delMethod);
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

        // --- MODIFICADO: OpenScope ahora puede asociar el nuevo scope con un nodo del AST ---
        public void OpenScope(IParseTree nodeContext = null)
        {
            _currentScope = new Scope(_currentScope); 
            _currentLevel++; 
            if (nodeContext != null)
            {
                // Solo añadir si no existe ya para este nodo (evitar problemas si se llama dos veces para el mismo contexto)
                if (!_nodeScopes.ContainsKey(nodeContext))
                {
                    _nodeScopes.Add(nodeContext, _currentScope);
                }
                else
                {
                    // Esto podría indicar un problema en la lógica del checker si intenta abrir un scope para el mismo nodo dos veces.
                    // O podría ser benigno si se espera que el scope se "re-asocie" (aunque es menos común).
                    // Por ahora, podemos actualizarlo o registrar una advertencia.
                    _nodeScopes[nodeContext] = _currentScope; // Actualiza al scope más recientemente abierto para este nodo
                    // Console.WriteLine($"Warning: Scope for node {nodeContext.GetText().Substring(0, Math.Min(20, nodeContext.GetText().Length))} re-associated.");
                }
            }
        }

        public void CloseScope()
        {
            if (_currentScope != null)
            {
                // No removemos de _nodeScopes al cerrar, porque el CodeGenerator
                // necesitará acceder a esos scopes después de que el Checker haya terminado.
                _currentScope = _currentScope.Outer; 
                _currentLevel--; 
            }
        }

        // --- NUEVO: Método para obtener el Scope asociado con un nodo del AST ---
        public Scope GetScopeForNode(IParseTree nodeContext)
        {
            _nodeScopes.TryGetValue(nodeContext, out Scope scope);
            return scope; // Devuelve el scope encontrado o null si no hay ninguno asociado
        }
        
        public void PrintFlatTable()
        {
            // ... (sin cambios en PrintFlatTable) ...
            Console.WriteLine("===== FLAT SYMBOL TABLE (Visible from Current Scope, Level: " + _currentLevel + ") =====");
            Console.WriteLine("{0,-20} {1,-10} {2,-15} {3,-15} {4,-10}", "Nombre", "Nivel Decl", "Tipo", "Clasificación", "Linea Decl");
            Console.WriteLine(new string('-', 75)); 

            Scope currentVisibleScope = _currentScope;
            List<string> printedSymbols = new List<string>(); 

            while (currentVisibleScope != null)
            {
                foreach (var symbol in currentVisibleScope.Symbols.Values.OrderBy(s => s.Name))
                {
                    if (!printedSymbols.Contains(symbol.Name)) 
                    {
                        string nombre = symbol.Name;
                        int nivel = symbol.Level; 
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
                        printedSymbols.Add(symbol.Name); 
                    }
                }
                currentVisibleScope = currentVisibleScope.Outer; 
            }
            Console.WriteLine(new string('=', 75));
        }
        
        public Symbol SearchGlobal(string name) {
            if (_globalScope != null) {
                return _globalScope.FindCurrent(name); 
            }
            return null;
        }
        
        public Scope GetGlobalScope() => _globalScope;
        public int GetGlobalScopeLevel() => 0;
    }
}