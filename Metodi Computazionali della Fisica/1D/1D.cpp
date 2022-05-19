#include <iostream>
#include <cmath>
#include <complex>
#include <fstream>

using namespace std;

void solve_tridiagonal(int n, complex<double>* d, complex<double>* u, complex<double>* l, complex<double>* b, complex<double>* a) {

    complex<double>* alfa = new complex<double>[n];
    complex<double>* beta = new complex<double>[n];

    alfa[0] = -d[0] / u[0];
    beta[0] = b[0] / u[0];

    for (int i = 1; i < n; i++) {
        alfa[i] = (-l[i - 1] / (u[i] * alfa[i - 1]) - d[i] / u[i]);
        beta[i] = b[i] / u[i] + l[i - 1] * beta[i - 1] / (u[i] * alfa[i - 1]);

    }


    a[n - 1] = (b[n - 1] / l[n - 2] + beta[n - 2] / alfa[n - 2]) / (1. / alfa[n - 2] + d[n - 1] / l[n - 2]);

    for (int i = n - 1; i > 0; i--) {
        a[i - 1] = a[i] / alfa[i - 1] - beta[i - 1] / alfa[i - 1];
    }


    free(alfa);
    free(beta);


}


int main(int argc, const char* argv[])
{
    double Lx=500.0, x0=200.0, sigmax=20.0, a = 250.0, b = 260.0;
    double Ly=500.0, y0=200.0, sigmay = 20.0;
    double qx, qy, V0;
    double dt;
    int Nx, Ny, Nsteps, Nprint;
    complex<double> pic(0., 1.0);

    cout << "Valori suggeriti: p_x,y = 2 \tV0 = 1.7V \tN_x,y >> 1000 \tdt = 0.1s" << endl;
    cout << "Momento della particella lungo x: ";
    cin >> qx;
    cout << "Momento della particella lungo y: ";
    cin >> qy;
    cout << "Energia della particella lungo x (J): " << pow(qx, 2) * 0.5 << endl;
    cout << "Energia della particella lungo y (J): " << pow(qy, 2) * 0.5 << endl;
    cout << "Valore potenziale V0 (V): ";
    cin >> V0;
    cout << "Numero punti asse x Nx: ";
    cin >> Nx;
    cout << "Numero punti asse y Ny: ";
    cin >> Ny;
    cout << "Time step dt (s): ";
    cin >> dt;
    cout << "Numero time steps: ";
    cin >> Nsteps;
    cout << "Stampa ogni numero passi: ";
    cin >> Nprint;

    double h = L / (Nx - 1);
    int Nmat = Nx - 2;
    double norm;

    complex<double>* psi0 = new complex<double>[Nmat];
    complex<double>* psi1 = new complex<double>[Nmat];
    complex<double>* d = new complex<double>[Nmat];
    complex<double>* u = new complex<double>[Nmat];
    complex<double>* l = new complex<double>[Nmat];
    complex<double>* f = new complex<double>[Nmat];

    norm = 0.; // initialising the wave function
    for (int i = 1; i < Nx - 1; i++) {
        double x = h * i;

        psi0[i - 1] = exp(pic * q * x) * exp(-pow(x - x0, 2) / (2 * pow(sigma, 2)));
        norm += pow(abs(psi0[i - 1]), 2);
    }

    norm = norm * L / Nx;
    norm = sqrt(norm);
    cout << "Norma " << norm << "\n";
    for (int i = 0; i < Nmat; i++) {
        psi0[i] = psi0[i] / norm;
    }

    for (int i = 0; i < Nmat; i++) {
        u[i] = 1.; // above diagonal
        l[i] = 1.; // below diagonal
    }



    for (int i = 0; i < Nmat; i++) {
        double Vx;
        double x = h * (i + 1);
        if (x >= a && x <= b) {
            Vx = V0;
        }
        else {
            Vx = 0;
        }
        d[i] = pic * 4. * pow(h, 2) / dt - 2. - 2 * pow(h, 2) * Vx; // diagonal
    }


    char* docdir; // print section
    size_t size;

    _dupenv_s(&docdir, &size, "USERPROFILE");

    string path(docdir);
    path += "\\OneDrive\\Desktop\\S2D.dat";

    ofstream fileg;
    fileg.open(path, ios::out);
    fileg.precision(10);


    for (int n = 0; n < Nsteps; n++) { // loop cicle
        for (int i = 1; i < Nmat - 1; i++) {
            double Vx;
            double x = h * (i + 1);

            if (x >= a && x <= b) {
                Vx = V0;
            }
            else {
                Vx = 0;
            }
            f[i] = -psi0[i + 1] + 2. * psi0[i] - psi0[i - 1] + (pic * 4. * pow(h, 2) / dt) * psi0[i] + 2. * pow(h, 2) * Vx * psi0[i];

        }
        f[0] = -psi0[1] + 2. * psi0[0] + (pic * 4. * pow(h, 2) / dt) * psi0[0];
        f[Nmat - 1] = -psi0[Nmat - 2] + 2. * psi0[Nmat - 1] + (pic * 4. * pow(h, 2) / dt) * psi0[Nmat - 1];
        solve_tridiagonal(Nmat, d, u, l, f, psi1);


        if (n % Nprint == 0) {
            /* ofstream file;
             string nome;
             nome = (string) "psi"+to_string(((double)n)*dt) +(string)".dat";
             file.open(nome,ios::out);
             file.precision(10);*/
            for (int i = 0; i < Nmat; i++) {
                double x = h * (i + 1);
                //   file << x << "  " << pow(abs(psi0[i]),2) << '\n';
                fileg << x << "\t" << pow(abs(psi0[i]), 2) << "\n";

            }

            //   file.close();
            fileg << '\n';

        }


        norm = 0.;
        for (int i = 0; i < Nmat; i++) {
            psi0[i] = psi1[i];
            norm += pow(abs(psi0[i]), 2); // unitary operator maintains the norm
        }
        norm = norm * L / Nx;
        cout << "Passo " << n << "\t" << "Norma " << norm << '\n';

    }

    fileg.close();
    free(psi0);
    free(psi1);
    free(d);
    free(u);
    free(l);
    free(f);

    return 0;
}
