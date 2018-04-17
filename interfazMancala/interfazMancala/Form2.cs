using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.IO;


namespace interfazMancala
{
    public partial class Form2 : Form
    {
        public Form1 f2 = new Form1();
        public Form2()
        {
            InitializeComponent();
        }
        

        private void button1_Click(object sender, EventArgs e)
        {
            this.Hide();
            
            f2.Show();

            //string path = @"C:\Users\Pablo\Desktop";
            //string archivo = Path.GetFileName(path);
            //File.SetAttributes(archivo, FileAttributes.Normal);
            //File.WriteAllText(path, "F");
            //TextWriter tsw = new StreamWriter(@"C:\Users\Pablo\Desktop");//cambiar ruta para txt
            //tsw.WriteLine("F");
            //tsw.Close();
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.Hide();
            Form1 f2 = new Form1();
            f2.Show();

            //string path = @"C:\Users\Pablo\Desktop";
            //string archivo = Path.GetFileName(path);
            //File.SetAttributes(archivo, FileAttributes.Normal);
            //File.WriteAllText(path, "M");

            //TextWriter tsw = new StreamWriter(@"C:\Users\Pablo\Desktop");//cambiar ruta para txt
            //tsw.WriteLine("M");
            //tsw.Close();

        }

        private void button3_Click(object sender, EventArgs e)
        {
            this.Hide();
            Form1 f2 = new Form1();
            f2.Show();

            //string path = @"C:\Users\Pablo\Desktop";
            //string archivo = Path.GetFileName(path);
            //File.SetAttributes(archivo, FileAttributes.Normal);
            //File.WriteAllText(path, "D");
            //TextWriter tsw = new StreamWriter(@"C:\Users\Pablo\Desktop");//cambiar ruta para txt
            //tsw.WriteLine("D");
            //tsw.Close();
        }
    }
}
