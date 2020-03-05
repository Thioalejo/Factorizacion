using System;
using System.Collections.Generic;
using System.Text;

namespace FactorizacionMovil
{
    public class Factorizar
    {
        public string FactorizarPorFormulaGeneral(Double a, Double b, Double c)
        {
            Double x1, x2;

            x1 = (-b - (Math.Sqrt((Math.Pow(b, 2)) - (4 * a * c)))) / (2 * a);
            x2 = (-b + (Math.Sqrt((Math.Pow(b, 2)) - (4 * a * c)))) / (2 * a);

            
            return "Resultado: x1 = " + x2 +"\n" + "Resultado: x2 = " + x1;
        }
    }
}
