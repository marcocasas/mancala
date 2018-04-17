using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace interfazMancala
{
    public partial class Form1 : Form
    {
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

        
    }
}
