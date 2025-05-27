// Archivo: Compiladores/Checker/CompilationManager.cs
using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;
using generated; // Para MiniCSharpLexer, MiniCSharpParser

namespace Compiladores.Checker
{
    public class CompilationManager
    {
        private readonly Dictionary<string, ClassSymbol> _compiledModulesCache = new Dictionary<string, ClassSymbol>();
        private readonly HashSet<string> _currentlyCompiling = new HashSet<string>(); // Para detectar dependencias circulares simples
        private readonly string _initialDirectory; // Directorio del archivo principal para resolver otros

        public CompilationManager(string mainFilePath)
        {
            _initialDirectory = Path.GetDirectoryName(mainFilePath);
            if (string.IsNullOrEmpty(_initialDirectory))
            {
                _initialDirectory = Directory.GetCurrentDirectory(); // Fallback si la ruta no tiene directorio
            }
        }

        /// <summary>
        /// Intenta compilar un módulo (archivo .mcs) por su nombre y devuelve su ClassSymbol principal.
        /// Devuelve null si no se encuentra, hay errores, o dependencia circular.
        /// El 'callingChecker' es el checker del archivo que contiene la directiva 'using',
        /// para reportar errores en su contexto.
        /// </summary>
        public ClassSymbol GetOrCompileModule(string moduleName, MiniCSharpChecker callingChecker)
        {
            if (string.IsNullOrEmpty(moduleName)) return null;

            if (_compiledModulesCache.TryGetValue(moduleName, out ClassSymbol cachedSymbol))
            {
                return cachedSymbol;
            }

            if (_currentlyCompiling.Contains(moduleName))
            {
                callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Circular dependency detected for module '{moduleName}'.");
                return null; 
            }

            string moduleFileName = moduleName + ".mcs";
            // Intenta buscar primero relativo al archivo que hace el 'using'
            string moduleFilePath = Path.Combine(Path.GetDirectoryName(callingChecker.CurrentFilePath), moduleFileName);

            if (!File.Exists(moduleFilePath))
            {
                // Si no se encuentra, intenta relativo al directorio del archivo principal original
                moduleFilePath = Path.Combine(_initialDirectory, moduleFileName);
                if (!File.Exists(moduleFilePath))
                {
                    callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Source file '{moduleFileName}' for using directive '{moduleName}' not found. Searched in '{Path.GetDirectoryName(callingChecker.CurrentFilePath)}' and '{_initialDirectory}'.");
                    return null;
                }
            }

            _currentlyCompiling.Add(moduleName);
            ClassSymbol resultSymbol = null;

            try
            {
                Console.WriteLine($"--- Compiling dependent module: {moduleFilePath} ---");
                ICharStream stream = CharStreams.fromPath(moduleFilePath);
                MiniCSharpLexer lexer = new MiniCSharpLexer(stream);
                CommonTokenStream tokens = new CommonTokenStream(lexer);
                MiniCSharpParser parser = new MiniCSharpParser(tokens);

                MyErrorListener moduleErrorListener = new MyErrorListener();
                lexer.RemoveErrorListeners();
                parser.RemoveErrorListeners();
                lexer.AddErrorListener(moduleErrorListener);
                parser.AddErrorListener(moduleErrorListener);

                MiniCSharpParser.ProgramContext tree = parser.program();

                if (moduleErrorListener.HasErrors)
                {
                    callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Parser errors in module '{moduleName}':");
                    foreach(var err in moduleErrorListener.ErrorMessages) callingChecker.ErrorMessages.Add($"  [Module {moduleName}] {err}");
                    return null;
                }
                
                // Cada módulo se compila con su propia tabla de símbolos nueva.
                TablaSimbolos moduleSymbolTable = new TablaSimbolos();
                // Y su propio checker, que también necesita este CompilationManager para sus propios 'using'.
                MiniCSharpChecker moduleChecker = new MiniCSharpChecker(this, moduleSymbolTable, moduleFilePath); 
                
                moduleChecker.Visit(tree); 

                if (moduleChecker.ErrorMessages.Count > 0)
                {
                    callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Semantic errors in module '{moduleName}':");
                    foreach(var err in moduleChecker.ErrorMessages) callingChecker.ErrorMessages.Add($"  [Module {moduleName}] {err}");
                    // Incluso si hay errores, podríamos querer cachear parcialmente, pero por ahora retornamos null.
                    return null;
                }

                string mainClassNameInModuleFile = tree.IDENT().GetText();
                if (mainClassNameInModuleFile != moduleName)
                {
                     callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Module file '{moduleFileName}' was expected to define class '{moduleName}', but defines class '{mainClassNameInModuleFile}'.");
                     return null;
                }

                Symbol foundSymbol = moduleSymbolTable.SearchGlobal(mainClassNameInModuleFile); 
                
                if (foundSymbol is ClassSymbol mainClassOfModule)
                {
                    _compiledModulesCache[moduleName] = mainClassOfModule;
                    resultSymbol = mainClassOfModule;
                }
                else
                {
                    callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Main class symbol '{mainClassNameInModuleFile}' not found after compiling module '{moduleName}'. This indicates an internal issue with the module's symbol table initialization.");
                }
            }
            catch (IOException ioEx)
            {
                 callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: IOException while processing module '{moduleName}': {ioEx.Message}");
            }
            catch (Exception ex)
            {
                callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Unexpected error while compiling module '{moduleName}': {ex.ToString()}"); // ToString() para más detalle
            }
            finally
            {
                _currentlyCompiling.Remove(moduleName);
            }
            
            if (resultSymbol != null) {
                 Console.WriteLine($"--- Successfully compiled and cached module: {moduleName} ---");
            } else {
                 Console.WriteLine($"--- Failed to compile or process module: {moduleName} ---");
            }
            return resultSymbol;
        }
    }
}