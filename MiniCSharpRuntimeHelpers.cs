// Archivo: MiniCSharpRuntimeHelpers.cs
using System;
using System.Linq; // Necesario para System.Array.Empty<T>() en algunas versiones o para claridad

namespace Compiladores // Asegúrate que este namespace coincida con el using en MiniCSharpCodeGenerator.cs
{
    public static class MiniCSharpRuntimeHelpers
    {
        // Helper para add(int[], int)
        public static int[] AddIntElement(int[] array, int element)
        {
            int originalLength = (array == null) ? 0 : array.Length;
            int[] newArray = new int[originalLength + 1];
            if (array != null)
            {
                System.Array.Copy(array, newArray, originalLength);
            }
            newArray[originalLength] = element;
            return newArray;
        }

        // Helper para add(char[], char)
        public static char[] AddCharElement(char[] array, char element)
        {
            int originalLength = (array == null) ? 0 : array.Length;
            char[] newArray = new char[originalLength + 1];
            if (array != null)
            {
                System.Array.Copy(array, newArray, originalLength);
            }
            newArray[originalLength] = element;
            return newArray;
        }

        // Helper para add(double[], double)
        public static double[] AddDoubleElement(double[] array, double element)
        {
            int originalLength = (array == null) ? 0 : array.Length;
            double[] newArray = new double[originalLength + 1];
            if (array != null)
            {
                System.Array.Copy(array, newArray, originalLength);
            }
            newArray[originalLength] = element;
            return newArray;
        }

        // NUEVOS HELPERS PARA 'del'

        // Helper para del(int[], int)
        public static int[] DeleteIntElementAt(int[] array, int index)
        {
            if (array == null || array.Length == 0)
            {
                return array ?? System.Array.Empty<int>(); // Devolver array original o vacío si es nulo/vacío
            }
            if (index < 0 || index >= array.Length)
            {
                // Opción 1: Lanzar excepción (estándar en C#)
                throw new IndexOutOfRangeException("Index was outside the bounds of the array for del operation.");
                // Opción 2: Devolver el array original sin cambios (más permisivo)
                // return array; 
            }

            int[] newArray = new int[array.Length - 1];
            if (index > 0)
            {
                System.Array.Copy(array, 0, newArray, 0, index);
            }
            if (index < array.Length - 1)
            {
                System.Array.Copy(array, index + 1, newArray, index, array.Length - index - 1);
            }
            return newArray;
        }

        // Helper para del(char[], int)
        public static char[] DeleteCharElementAt(char[] array, int index)
        {
            if (array == null || array.Length == 0)
            {
                return array ?? System.Array.Empty<char>();
            }
            if (index < 0 || index >= array.Length)
            {
                throw new IndexOutOfRangeException("Index was outside the bounds of the array for del operation.");
            }

            char[] newArray = new char[array.Length - 1];
            if (index > 0)
            {
                System.Array.Copy(array, 0, newArray, 0, index);
            }
            if (index < array.Length - 1)
            {
                System.Array.Copy(array, index + 1, newArray, index, array.Length - index - 1);
            }
            return newArray;
        }

        // Helper para del(double[], int)
        public static double[] DeleteDoubleElementAt(double[] array, int index)
        {
            if (array == null || array.Length == 0)
            {
                return array ?? System.Array.Empty<double>();
            }
            if (index < 0 || index >= array.Length)
            {
                throw new IndexOutOfRangeException("Index was outside the bounds of the array for del operation.");
            }

            double[] newArray = new double[array.Length - 1];
            if (index > 0)
            {
                System.Array.Copy(array, 0, newArray, 0, index);
            }
            if (index < array.Length - 1)
            {
                System.Array.Copy(array, index + 1, newArray, index, array.Length - index - 1);
            }
            return newArray;
        }

        // Podrías añadir sobrecargas para double[] si lo necesitas:
        // public static double[] DeleteDoubleElementAt(double[] array, int index) { ... }
    }
}