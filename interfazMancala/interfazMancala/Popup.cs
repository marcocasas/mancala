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
    public partial class Popup : Form
    {
        public Popup()
        {
            InitializeComponent();
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void btnConf_Click(object sender, EventArgs e)
        {
            this.Close();
            var first = Application.OpenForms.OfType<Form1>().FirstOrDefault();
            if (first != null)
                first.Close();
            Form2 f2 = new Form2();
            f2.Show();

        }
    }
}
