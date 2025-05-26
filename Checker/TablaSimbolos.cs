
using System.Collections.Generic;

namespace Compiladores.Checker
{
    // --- Representación de Tipos ---

    public enum TypeKind { Int, Double, Char, Bool, String, Array, Class, Void, Null, Error }

    public abstract class Type
    {
        public TypeKind Kind { get; protected set; }
        public string Name { get; protected set; }

        protected Type(string name) { Name = name; }

        // Tipos predefinidos para fácil acceso
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
        public Scope Members { get; } // Las clases tienen su propio ámbito de miembros

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
        public int Level { get; set; } // Nivel de anidamiento
        public Antlr4.Runtime.ParserRuleContext Decl { get; set; } // Referencia al nodo de declaración

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
        public Scope Outer { get; } // Ámbito exterior
        public Dictionary<string, Symbol> Symbols { get; } = new Dictionary<string, Symbol>();

        public Scope(Scope outer = null)
        {
            Outer = outer;
        }

        public bool TryInsert(Symbol symbol)
        {
            if (Symbols.ContainsKey(symbol.Name))
            {
                return false; // Ya existe en este ámbito
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
            return Outer?.Find(name); // Busca en el ámbito exterior si no lo encuentra aquí
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

        public int CurrentLevel => _currentLevel;
        public Scope CurrentScope => _currentScope;


        public TablaSimbolos()
        {
            _currentScope = null;
            _currentLevel = -1;
            Init();
        }

        /// <summary>
        /// Inicializa la tabla con los tipos y funciones predefinidas.
        /// </summary>
        private void Init()
        {
            OpenScope(); // Ámbito Global

            // Tipos básicos [cite: 73]
            Insert(new TypeDefSymbol("int", Type.Int));
            Insert(new TypeDefSymbol("double", Type.Double));
            Insert(new TypeDefSymbol("char", Type.Char));
            Insert(new TypeDefSymbol("bool", Type.Bool));
            Insert(new TypeDefSymbol("string", Type.String));
            Insert(new TypeDefSymbol("void", Type.Void));

            // Constante null [cite: 132]
            Insert(new VarSymbol("null", Type.Null, isConst: true));

            // Funciones predefinidas [cite: 133, 134]
            var chrMethod = new MethodSymbol("chr", Type.Char);
            chrMethod.Parameters.Add(new VarSymbol("i", Type.Int));
            Insert(chrMethod);

            var ordMethod = new MethodSymbol("ord", Type.Int);
            ordMethod.Parameters.Add(new VarSymbol("ch", Type.Char));
            Insert(ordMethod);

            // len puede aplicar a arreglos, necesitaríamos un tipo 'any_array' o chequearlo
            // en el checker. Por ahora, lo definimos con un tipo genérico o lo dejamos
            // para manejarlo específicamente en el visitor. Aquí lo definimos como
            // si esperara un arreglo de int (habrá que generalizarlo).
            var lenMethod = new MethodSymbol("len", Type.Int);
            lenMethod.Parameters.Add(new VarSymbol("a", new ArrayType(Type.Error))); // Tipo 'Error' como placeholder
            Insert(lenMethod);

             // NOTA: add y del se omiten según los requisitos de generación de código[cite: 146].
        }


        /// <summary>
        /// Inserta un símbolo en el ámbito actual.
        /// </summary>
        /// <param name="symbol">Símbolo a insertar.</param>
        /// <returns>True si se insertó, False si ya existía.</returns>
        public bool Insert(Symbol symbol)
        {
            if (_currentScope == null) return false; // No debería pasar después de Init()

            if (!_currentScope.TryInsert(symbol))
            {
                // Error: Símbolo ya definido en este ámbito.
                // El Checker se encargará de reportar esto.
                return false;
            }
            symbol.Level = _currentLevel;
            return true;
        }

        /// <summary>
        /// Busca un símbolo desde el ámbito actual hacia afuera.
        /// </summary>
        /// <param name="name">Nombre del símbolo.</param>
        /// <returns>El símbolo encontrado o null.</returns>
        public Symbol Search(string name)
        {
            return _currentScope?.Find(name);
        }

        /// <summary>
        /// Busca un símbolo SÓLO en el ámbito actual.
        /// </summary>
        /// <param name="name">Nombre del símbolo.</param>
        /// <returns>El símbolo encontrado o null.</returns>
        public Symbol SearchCurrentScope(string name)
        {
            return _currentScope?.FindCurrent(name);
        }


        /// <summary>
        /// Abre un nuevo nivel de ámbito.
        /// </summary>
        public void OpenScope()
        {
            _currentScope = new Scope(_currentScope);
            _currentLevel++;
        }

        /// <summary>
        /// Cierra el nivel de ámbito actual.
        /// </summary>
        public void CloseScope()
        {
            if (_currentScope != null)
            {
                _currentScope = _currentScope.Outer;
                _currentLevel--;
            }
        }
    }
}