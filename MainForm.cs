using System;
using System.IO;
using System.Windows.Forms;
using Compiladores; // Tu clase Compiler está aquí

namespace Compiladores
{
    public partial class MainForm : Form
    {
        private TextBox txtEditor;
        private Button btnCompile;
        private TextBox txtOutput;

        private string filePath = @"C:\Users\antho\programming\compiladores\MyUtilities.mcs";

        public MainForm()
        {
            Text = "MiniCSharp Editor";
            Width = 1000;
            Height = 700;

            txtEditor = new TextBox()
            {
                Multiline = true,
                ScrollBars = ScrollBars.Both,
                WordWrap = false,
                Left = 10,
                Top = 10,
                Width = 960,
                Height = 400,
                Font = new System.Drawing.Font("Consolas", 10)
            };

            btnCompile = new Button()
            {
                Text = "Guardar y Compilar",
                Left = 10,
                Top = 420,
                Width = 200
            };
            btnCompile.Click += BtnCompile_Click;

            txtOutput = new TextBox()
            {
                Multiline = true,
                ScrollBars = ScrollBars.Vertical,
                ReadOnly = true,
                Left = 10,
                Top = 460,
                Width = 960,
                Height = 180,
                Font = new System.Drawing.Font("Consolas", 9)
            };

            Controls.Add(txtEditor);
            Controls.Add(btnCompile);
            Controls.Add(txtOutput);

            if (File.Exists(filePath))
                txtEditor.Text = File.ReadAllText(filePath);
        }

        private void BtnCompile_Click(object sender, EventArgs e)
        {
            try
            {
                File.WriteAllText(filePath, txtEditor.Text);
                var writer = new StringWriter();
                Console.SetOut(writer);
                Compiler.Compile(new[] { filePath });
                writer.Flush();
                txtOutput.Text = writer.ToString();
            }
            catch (Exception ex)
            {
                txtOutput.Text = $"Error: {ex.Message}";
            }
        }
    }
}
