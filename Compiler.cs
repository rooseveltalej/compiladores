using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Compiladores.Checker;
using Compiladores.CodeGen;
using generated;

namespace Compiladores
{
    public partial class Compiler
    {
        public static void Compile(string[] args)
        {
            string filePath = @"C:\Users\Bayron\RiderProjects\compiladores\finalIntegrationTest.mcs";

            if (args.Length > 0)
                filePath = args[0];

            if (!File.Exists(filePath))
            {
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
                    return;
                }

                Console.WriteLine("Lexer and Parser finished successfully!");
                Console.WriteLine("Proceeding to Semantic Analysis...");

                TablaSimbolos mainSymbolTable = new TablaSimbolos();
                MiniCSharpChecker semanticChecker = new MiniCSharpChecker(compilationManager, mainSymbolTable, filePath);
                semanticChecker.Visit(tree);

                if (semanticChecker.ErrorMessages.Count > 0)
                {
                    Console.WriteLine($"Semantic Analysis FAILED with {semanticChecker.ErrorMessages.Count} error(s):");
                    foreach (var error in semanticChecker.ErrorMessages)
                        Console.WriteLine(error);
                }
                else
                {
                    Console.WriteLine("Semantic Analysis finished successfully! No errors found.");
                    Console.WriteLine("Proceeding to Code Generation and Execution...");

                    try
                    {
                        var codeGenerator = new MiniCSharpCodeGenerator(
                            compilationManager,
                            mainSymbolTable,
                            filePath,
                            "InMemoryAssembly",
                            semanticChecker.ExpressionTypes
                        );


                        var mainClassType = codeGenerator.GenerateAssemblyAndGetMainType(tree);

                        if (mainClassType != null)
                        {
                            var mainMethod = mainClassType.GetMethod("Main", BindingFlags.Public | BindingFlags.Static);
                            if (mainMethod?.GetParameters().Length == 0 && mainMethod.ReturnType == typeof(void))
                            {
                                Console.WriteLine("\n--- Output from dynamically executed MiniCSharp code ---");
                                mainMethod.Invoke(null, null);
                                Console.WriteLine("--- End of MiniCSharp code output ---");
                            }
                            else
                            {
                                Console.WriteLine("Error: El método 'Main' generado no tiene la firma esperada.");
                            }
                        }
                        else
                        {
                            Console.WriteLine("Code generation did not produce a usable main class type.");
                        }
                    }
                    catch (NotImplementedException nie)
                    {
                        Console.WriteLine($"Code Generation SKIPPED: {nie.Message}");
                    }
                    catch (TargetInvocationException tie)
                    {
                        Console.WriteLine($"RUNTIME ERROR in generated code: {tie.Message}");
                        if (tie.InnerException != null)
                        {
                            Console.WriteLine("--- Inner Exception ---");
                            Console.WriteLine(tie.InnerException.ToString());
                            Console.WriteLine("--- End Inner Exception ---");
                        }
                        else
                        {
                            Console.WriteLine("No Inner Exception details available.");
                        }
                        Console.WriteLine(tie.StackTrace);
                    }
                    catch (Exception cgEx)
                    {
                        Console.WriteLine($"Code Generation or other FAILED: {cgEx.Message}");
                        Console.WriteLine(cgEx.StackTrace);
                    }
                }
            }
            catch (IOException ex)
            {
                Console.WriteLine($"Error reading file: {ex.Message}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Unexpected error: {ex.Message}");
                Console.WriteLine(ex.StackTrace);
            }
        }
    }
}