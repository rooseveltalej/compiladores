using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace Compiladores
{
    public partial class MainForm : Form
    {
        private TextBox txtEditor;
        private Button btnCompile;
        private TextBox txtOutput;
        private Label lblLineInfo;
        private Panel sidebar;

        private string filePath = @"C:\Users\Bayron\RiderProjects\compiladores\testArraysAccess.mcs";

        public MainForm()
        {
            Text = "MiniCSharp Editor";
            WindowState = FormWindowState.Maximized;
            BackColor = Color.FromArgb(30, 30, 30); // Modo oscuro

            // Sidebar
            sidebar = new Panel()
            {
                Location = new Point(10, 10),
                Width = 90,
                Height = ClientSize.Height - 20,
                BackColor = Color.FromArgb(45, 45, 45)
            };

            lblLineInfo = new Label()
            {
                Text = "Línea: 0",
                Location = new Point(10, 10),
                AutoSize = true,
                ForeColor = Color.White,
                Font = new Font("Segoe UI", 10)
            };
            sidebar.Controls.Add(lblLineInfo);

            // Editor
            txtEditor = new TextBox()
            {
                Multiline = true,
                ScrollBars = ScrollBars.Both,
                WordWrap = false,
                Location = new Point(110, 10),
                Width = ClientSize.Width - 130,
                Height = ClientSize.Height - 200,
                Font = new Font("Consolas", 11),
                BackColor = Color.FromArgb(25, 25, 25),
                ForeColor = Color.White,
                BorderStyle = BorderStyle.FixedSingle
            };
            txtEditor.KeyUp += TxtEditor_SelectionChanged;
            txtEditor.MouseUp += TxtEditor_SelectionChanged;

            // Botón
            btnCompile = new Button()
            {
                Text = "💾 Guardar y Compilar",
                Location = new Point(110, ClientSize.Height - 170),
                Width = 200,
                Height = 35,
                FlatStyle = FlatStyle.Flat,
                BackColor = Color.FromArgb(57, 67, 159),
                ForeColor = Color.White,
                Font = new Font("Segoe UI", 10, FontStyle.Bold)
            };
            btnCompile.FlatAppearance.BorderSize = 0;
            btnCompile.Click += BtnCompile_Click;

            // Output
            txtOutput = new TextBox()
            {
                Multiline = true,
                ScrollBars = ScrollBars.Vertical,
                ReadOnly = true,
                Location = new Point(110, ClientSize.Height - 120),
                Width = ClientSize.Width - 130,
                Height = 100,
                Font = new Font("Consolas", 9),
                BackColor = Color.FromArgb(20, 20, 20),
                ForeColor = Color.LightGray,
                BorderStyle = BorderStyle.FixedSingle
            };

            Controls.Add(sidebar);
            Controls.Add(txtEditor);
            Controls.Add(btnCompile);
            Controls.Add(txtOutput);
            Resize += (_, __) => AdjustLayout();

            if (File.Exists(filePath))
                txtEditor.Text = File.ReadAllText(filePath);
        }

        private void AdjustLayout()
        {
            txtEditor.Width = ClientSize.Width - 130;
            txtEditor.Height = ClientSize.Height - 200;

            btnCompile.Location = new Point(110, ClientSize.Height - 170);
            txtOutput.Location = new Point(110, ClientSize.Height - 120);
            txtOutput.Width = ClientSize.Width - 130;
        }

        private void TxtEditor_SelectionChanged(object sender, EventArgs e)
        {
            int index = txtEditor.SelectionStart;
            int line = txtEditor.GetLineFromCharIndex(index)+1;
            lblLineInfo.Text = $"Línea: {Math.Max(0, line)}";
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
