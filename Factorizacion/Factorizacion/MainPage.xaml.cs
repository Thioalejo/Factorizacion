using FactorizacionMovil;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xamarin.Forms;

namespace Factorizacion
{
    // Learn more about making custom code visible in the Xamarin.Forms previewer
    // by visiting https://aka.ms/xamarinforms-previewer
    [DesignTimeVisible(false)]
    public partial class MainPage : ContentPage
    {
        public MainPage()
        {
            InitializeComponent();
        }

        private void BtnCalcular_Clicked(object sender, EventArgs e)
        {
            Factorizar factorizar = new Factorizar();
            double A, B, C;
            try
            {
                A = Convert.ToDouble(txtA.Text);
                B = Convert.ToDouble(txtB.Text);
                C = Convert.ToDouble(txtC.Text);
                lblResultado.Text = factorizar.FactorizarPorFormulaGeneral(A, B, C);
            }
            catch (Exception E)
            {
                lblResultado.Text = "Ha ingresado valores invalidos revisar" + E.Message;
            }

        }
    }
}
