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
        internal readonly Dictionary<string, Tuple<ClassSymbol, System.Type>> _compiledModulesCache = new Dictionary<string, Tuple<ClassSymbol, System.Type>>();
        private readonly HashSet<string> _currentlyCompiling = new HashSet<string>(); 
        private readonly string _initialDirectory; 

        public CompilationManager(string mainFilePath)
        {
            _initialDirectory = Path.GetDirectoryName(mainFilePath);
            if (string.IsNullOrEmpty(_initialDirectory))
            {
                _initialDirectory = Directory.GetCurrentDirectory(); 
            }
        }

      
        public Tuple<ClassSymbol, System.Type> GetOrCompileModule(string moduleName, MiniCSharpChecker callingChecker)
        {
            if (string.IsNullOrEmpty(moduleName)) return null;

            if (_compiledModulesCache.TryGetValue(moduleName, out var cachedModule))
            {
                return cachedModule;
            }

            if (_currentlyCompiling.Contains(moduleName))
            {
                callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Circular dependency detected for module '{moduleName}'.");
                return null;
            }

            string moduleFileName = moduleName + ".mcs";
            string moduleFilePath = Path.Combine(Path.GetDirectoryName(callingChecker.CurrentFilePath), moduleFileName);

            if (!File.Exists(moduleFilePath))
            {
                moduleFilePath = Path.Combine(_initialDirectory, moduleFileName);
                if (!File.Exists(moduleFilePath))
                {
                    callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Source file '{moduleFileName}' for using directive '{moduleName}' not found.");
                    return null;
                }
            }

            _currentlyCompiling.Add(moduleName);
            try
            {
                Console.WriteLine($"--- Compiling dependent module: {moduleFilePath} ---");
                ICharStream stream = CharStreams.fromPath(moduleFilePath);
                var lexer = new MiniCSharpLexer(stream);
                var tokens = new CommonTokenStream(lexer);
                var parser = new MiniCSharpParser(tokens);

                var moduleErrorListener = new MyErrorListener();
                lexer.RemoveErrorListeners();
                parser.RemoveErrorListeners();
                lexer.AddErrorListener(moduleErrorListener);
                parser.AddErrorListener(moduleErrorListener);

                MiniCSharpParser.ProgramContext tree = parser.program();

                if (moduleErrorListener.HasErrors)
                {
                    callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Parser errors in module '{moduleName}'.");
                    foreach (var err in moduleErrorListener.ErrorMessages) callingChecker.ErrorMessages.Add($"  [Module {moduleName}] {err}");
                    return null;
                }

                var moduleSymbolTable = new TablaSimbolos();
                var moduleChecker = new MiniCSharpChecker(this, moduleSymbolTable, moduleFilePath);
                moduleChecker.Visit(tree);

                if (moduleChecker.ErrorMessages.Count > 0)
                {
                    callingChecker.ErrorMessages.Add($"SEMANTIC ERROR: Semantic errors in module '{moduleName}'.");
                    foreach (var err in moduleChecker.ErrorMessages) callingChecker.ErrorMessages.Add($"  [Module {moduleName}] {err}");
                    Console.WriteLine($"--- Failed to compile module: {moduleName} (Semantic Errors) ---");
                    return null;
                }

                var moduleCodeGenerator = new CodeGen.MiniCSharpCodeGenerator(
                    this,
                    moduleSymbolTable,
                    moduleFilePath,
                    $"Module_{moduleName}",
                    moduleChecker.ExpressionTypes
                );

                var compiledType = moduleCodeGenerator.GenerateAssemblyAndGetMainType(tree);

                if (compiledType != null)
                {
                    var mainClassSymbol = moduleSymbolTable.SearchGlobal(moduleName) as ClassSymbol;
                    var result = new Tuple<ClassSymbol, System.Type>(mainClassSymbol, compiledType);
                    _compiledModulesCache[moduleName] = result;
                    Console.WriteLine($"--- Successfully compiled and generated module: {moduleName} ---");
                    return result;
                }
                else
                {
                    Console.WriteLine($"--- Failed to generate code for module: {moduleName} ---");
                    callingChecker.ErrorMessages.Add($"CODEGEN ERROR: Failed to generate type for module '{moduleName}'.");
                    return null;
                }
            }
            catch (Exception ex)
            {
                callingChecker.ErrorMessages.Add($"FATAL ERROR while compiling module '{moduleName}': {ex.Message}");
                return null;
            }
            finally
            {
                _currentlyCompiling.Remove(moduleName);
            }
        }
    }
}