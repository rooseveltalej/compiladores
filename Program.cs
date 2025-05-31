// See https://aka.ms/new-console-template for more information

// Program.cs
// Program.cs
using generated;
using System;
using System.IO;
using System.Reflection; // Necesario para BindingFlags y MethodInfo
using Antlr4.Runtime;
using Compiladores.Checker;
using Compiladores.CodeGen;

namespace Compiladores
{
    public class Compiler
    {
        public static void Main(string[] args)
        {
            string filePath = @"C:\Users\Bayron\RiderProjects\compiladores\MyUtilities.mcs"; // O tu ruta

            if (args.Length > 0) { 
                filePath = args[0];
            }
            
            if (!File.Exists(filePath)) {
                Console.WriteLine($"Error: Input file not found at '{filePath}'");
                return;
            }
            Console.WriteLine($"Compiling main file: {filePath}");
            
            try
            {
                CompilationManager compilationManager = new CompilationManager(filePath);
                
                ICharStream stream = CharStreams.fromPath(filePath);
                MiniCSharpLexer lexer = new MiniCSharpLexer(stream);
                CommonTokenStream tokens = new CommonTokenStream(lexer);
                MiniCSharpParser parser = new MiniCSharpParser(tokens);
                MyErrorListener errorListener = new MyErrorListener();

                lexer.RemoveErrorListeners();
                parser.RemoveErrorListeners();
                lexer.AddErrorListener(errorListener);
                parser.AddErrorListener(errorListener);

                MiniCSharpParser.ProgramContext tree = parser.program();

                if (errorListener.HasErrors)
                {
                    Console.WriteLine("Compilation (Lexer/Parser) FAILED with errors:");
                    Console.WriteLine(errorListener.ToString());
                }
                else
                {
                    Console.WriteLine("Lexer and Parser finished successfully!");
                    Console.WriteLine("Proceeding to Semantic Analysis...");
                    
                    TablaSimbolos mainSymbolTable = new TablaSimbolos();
                    MiniCSharpChecker semanticChecker = new MiniCSharpChecker(compilationManager, mainSymbolTable, filePath);
                    
                    semanticChecker.Visit(tree);
                    
                    if (semanticChecker.ErrorMessages.Count > 0)
                    {
                        Console.WriteLine($"Semantic Analysis FAILED with {semanticChecker.ErrorMessages.Count} error(s):");
                        foreach (string error in semanticChecker.ErrorMessages)
                        {
                            Console.WriteLine(error);
                        }
                    }
                    else
                    {
                        Console.WriteLine("Semantic Analysis for main file (and its dependencies) finished successfully! No errors found.");
                        // mainSymbolTable.PrintFlatTable(); // Comentado

                        Console.WriteLine("\nProceeding to Code Generation and Execution...");
                        try
                        {
                            MiniCSharpCodeGenerator codeGenerator = new MiniCSharpCodeGenerator(
                                mainSymbolTable, 
                                filePath, // Pasando filePath
                                "InMemoryAssembly", 
                                semanticChecker.ExpressionTypes // <<< PASAR EL DICCIONARIO DE TIPOS
                            );
                            // <<< MODIFICADO: Llamar a GenerateAssemblyAndGetMainType >>>
                            System.Type mainClassType = codeGenerator.GenerateAssemblyAndGetMainType(tree); 

                            if (mainClassType != null)
                            {
                                Console.WriteLine("Code generation successful. Attempting to execute Main()...");
                                MethodInfo mainMethod = mainClassType.GetMethod("Main", BindingFlags.Public | BindingFlags.Static);
                                
                                if (mainMethod != null)
                                {
                                    if (mainMethod.GetParameters().Length == 0 && mainMethod.ReturnType == typeof(void))
                                    {
                                        Console.WriteLine("\n--- Output from dynamically executed MiniCSharp code ---");
                                        mainMethod.Invoke(null, null); // null para 'this' (método estático), null para parámetros
                                        Console.WriteLine("--- End of MiniCSharp code output ---");
                                    }
                                    else
                                    {
                                        Console.WriteLine("Error: El método 'Main' generado no tiene la firma esperada (public static void Main()).");
                                    }
                                }
                                else
                                {
                                    Console.WriteLine("Error: No se pudo encontrar un método 'Main' público y estático en la clase generada.");
                                }
                            }
                            else
                            {
                                Console.WriteLine("Code generation did not produce a usable main class type.");
                            }
                        }
                        catch (NotImplementedException nie)
                        {
                            Console.WriteLine($"Code Generation SKIPPED for a part: {nie.Message}");
                        }
                        catch (Exception cgEx)
                        {
                            Console.WriteLine($"Code Generation FAILED with an unexpected error: {cgEx.Message}");
                            Console.WriteLine(cgEx.StackTrace);
                        }
                    }
                }
            }
            catch (IOException ex)
            {
                Console.WriteLine($"Error reading file: {ex.Message}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"An unexpected error occurred: {ex.Message}");
                Console.WriteLine(ex.StackTrace);
            }
        }
    }
}