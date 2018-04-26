using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace interfazMancala
{
    public partial class Form1 : Form
    {
        String jugada;

        public Form1()
        {
            InitializeComponent();
        }

        private void btnMenu_Click(object sender, EventArgs e)
        {
            Popup pop = new Popup();
            //this.Hide();
            //show form2:
            pop.ShowDialog();
        }

        private void setTablero(string edoActual)//formato (0 5 3...)
        {
            string nums = edoActual.Substring(1,edoActual.Length-2);
            string []tablero = nums.Split(' ');
            casilla1.Text = tablero[0]; casilla2.Text = tablero[1];
            casilla3.Text = tablero[2]; casilla4.Text = tablero[3];
            casilla5.Text = tablero[4]; casilla6.Text = tablero[5];
            casilla7.Text = tablero[6]; casilla8.Text = tablero[7];
            casilla9.Text = tablero[8]; casilla10.Text = tablero[9];
            casilla11.Text = tablero[10]; casilla12.Text = tablero[11];
            casilla13.Text = tablero[12]; casilla14.Text = tablero[13];
        }

        private void casilla12_Click(object sender, EventArgs e)
        {

        }

        private void button1_Click(object sender, EventArgs e)
        {
            button7.Enabled = true;
            jugada = "";
            jugada = "(1 ";
        }

        private void button4_Click(object sender, EventArgs e)
        {
            button7.Enabled = true;
            jugada = "";
            jugada = "(4 ";
        }

        private void button2_Click(object sender, EventArgs e)
        {
            button7.Enabled = true;
            jugada = "";
            jugada = "(2 ";
        }

        private void button3_Click(object sender, EventArgs e)
        {
            button7.Enabled = true;
            jugada = "";
            jugada = "(3 ";
        }

        private void button5_Click(object sender, EventArgs e)
        {
            button7.Enabled = true;
            jugada = "";
            jugada = "(5 ";
        }

        private void button6_Click(object sender, EventArgs e)
        {
            button7.Enabled = true;
            jugada = "";
            jugada = "(6 ";
        }

        private void button7_Click(object sender, EventArgs e)
        {
            jugada = jugada + "(" + casilla1.Text + " " + casilla2.Text + " " + casilla3.Text + " " + casilla4.Text + " " + casilla5.Text + " " + casilla6.Text + " " + casilla7.Text + " " + casilla8.Text + " " + casilla9.Text + " " + casilla10.Text + " " + casilla11.Text + " " + casilla12.Text + " " + casilla13.Text + " " + casilla14.Text + "))";

            using (StreamWriter outputFile = new StreamWriter(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\jugada.txt"))
            {
                    outputFile.WriteLine(jugada);
            }

            Task.Delay(3000).Wait();

            System.Diagnostics.Process process = new System.Diagnostics.Process();
            System.Diagnostics.ProcessStartInfo startInfo = new System.Diagnostics.ProcessStartInfo();
            startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
            startInfo.FileName = "cmd.exe";
            startInfo.Arguments = "/C clisp realiza-tirada.lisp > tirada-hecha.txt";
            process.StartInfo = startInfo;
            process.Start();
            process.WaitForExit();
            string readText = File.ReadAllText(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt");
            //label3.Text = readText.Substring(3,1);

            Task.Delay(3000).Wait();
            string[] ikers = readText.Substring(3,readText.Length-4).Split(' ');

            casilla1.Text = ikers[0];
            casilla2.Text = ikers[1];
            casilla3.Text = ikers[2];
            casilla4.Text = ikers[3];
            casilla5.Text = ikers[4];
            casilla6.Text = ikers[5];
            casilla7.Text = ikers[6];
            casilla8.Text = ikers[7];
            casilla9.Text = ikers[8];
            casilla10.Text = ikers[9];
            casilla11.Text = ikers[10];
            casilla12.Text = ikers[11];
            casilla13.Text = ikers[12];
            casilla14.Text = ikers[13];
            //label3.Text = readText.Substring(29, 1);

            jugada = "";
            label3.Text = ikers[14];

            if (Int32.Parse(ikers[6]) > 24)
            {
                label3.Text = "¡Ganaste!";
                return;
            } else if (Int32.Parse(ikers[13]) > 24)
            {
                label3.Text = "Perdiste :(";
                return;
            }

            int i = 0;

            while (ikers[14].Equals("M)"))
            {
                if (i > 0)
                {
                    if (File.ReadLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\maquina-tira.txt").First().Equals(""))
                    {
                        File.WriteAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt", File.ReadAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\maquina-tira.txt").Skip(1));
                    }
                    else
                    {
                        File.WriteAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt", File.ReadAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\maquina-tira.txt"));
                    }
                }

                //label3.Text = "Tira Maquina";
                //if(File.ReadAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt").Skip(1).Equals(""))

                if (File.ReadLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt").First().Equals(""))
                {
                    File.WriteAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt", File.ReadAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt").Skip(1));
                }
                else
                {
                    File.WriteAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt", File.ReadAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt"));
                }

                Task.Delay(3000).Wait();

                System.Diagnostics.Process process2 = new System.Diagnostics.Process();
                System.Diagnostics.ProcessStartInfo startInfo2 = new System.Diagnostics.ProcessStartInfo();
                startInfo2.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
                startInfo2.FileName = "cmd.exe";
                startInfo2.Arguments = "/C clisp mancala.lisp > maquina-tira.txt";
                process2.StartInfo = startInfo2;
                process2.Start();
                process2.WaitForExit();

                Task.Delay(3000).Wait();

                string readTextM = File.ReadAllText(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\maquina-tira.txt");
                label3.Text = "Tira maquina";
                Task.Delay(3000).Wait();

                if (readTextM.Equals(""))
                {
                    File.WriteAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt", File.ReadAllLines(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\tirada-hecha.txt"));
                    Task.Delay(3000).Wait();
                    System.Diagnostics.Process process3 = new System.Diagnostics.Process();
                    System.Diagnostics.ProcessStartInfo startInfo3 = new System.Diagnostics.ProcessStartInfo();
                    startInfo3.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
                    startInfo3.FileName = "cmd.exe";
                    startInfo3.Arguments = "/C clisp mancala.lisp > maquina-tira.txt";
                    process3.StartInfo = startInfo3;
                    process3.Start();
                    process3.WaitForExit();
                    Task.Delay(3000).Wait();
                    readTextM = File.ReadAllText(@"C:\Users\marco\OneDrive\Documentos\ITAM\Inteligencia Artificial\mancala\interfazMancala\interfazMancala\bin\Debug\maquina-tira.txt");
                    label3.Text = "Tira maquina" + readTextM;
                    Task.Delay(3000).Wait();
                }

                label3.Text = readTextM;
                string[] ikers2 = readTextM.Substring(3, readText.Length - 4).Split(' ');

                casilla1.Text = ikers2[0];
                casilla2.Text = ikers2[1];
                casilla3.Text = ikers2[2];
                casilla4.Text = ikers2[3];
                casilla5.Text = ikers2[4];
                casilla6.Text = ikers2[5];
                casilla7.Text = ikers2[6];
                casilla8.Text = ikers2[7];
                casilla9.Text = ikers2[8];
                casilla10.Text = ikers2[9];
                casilla11.Text = ikers2[10];
                casilla12.Text = ikers2[11];
                casilla13.Text = ikers2[12];
                casilla14.Text = ikers2[13];
                label3.Text = ikers2[14];

                ikers[14] = ikers2[14]; //

                if (ikers[14].Equals("M)"))
                {
                    i++;
                }

                if (Int32.Parse(ikers2[6]) > 24)
                {
                    label3.Text = "¡Ganaste!";
                    return;
                }
                else if (Int32.Parse(ikers2[13]) > 24)
                {
                    label3.Text = "Perdiste :(";
                    return;
                }


                Task.Delay(3000).Wait();
            }
            i = 0;
        }
        
    }
}
