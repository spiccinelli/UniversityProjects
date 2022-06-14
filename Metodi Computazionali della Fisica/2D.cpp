// Schroedinger's equation in 2D with potential barrier and time dependency
// Samuele Piccinelli, March 2020

#define _USE_MATH_DEFINES
#include <iostream>
#include <cmath>
#include <complex>
#include <fstream>

using namespace std;

void tridiag(int n, complex<double>* d, complex<double>* u, complex<double>* l, complex<double>* b, complex<double>* a) {

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
    double Lx = 100.0, x0 = 30.0, sigmax = 4.0, a = 50.0, b = 52.0;
    double Ly = 100.0, y0 = 50.0, sigmay = 4.0;
    double qx, qy, V0;
    double dt;
    int Nx, Ny, Nsteps, Nprint;
    complex<double> pic(0., 1.0); // imaginary unit

    cout << "Valori suggeriti: \tp_x = 2 \tV0 >= 0 \tN_x,y >= 200 \tdt = 0.1" << endl;
    cout << "Si utilizzano unita' atomiche, hbar = 1, m = 1" << endl;
    cout << "Momento della particella lungo x: ";
    cin >> qx;
    cout << "Energia della particella lungo x: " << pow(qx, 2) * 0.5 << endl;
    cout << "Momento della particella lungo y: ";
    cin >> qy;
    cout << "Energia della particella lungo y: " << pow(qy, 2) * 0.5 << endl;
    cout << "Valore potenziale V0: ";
    cin >> V0;
    cout << "Numero punti asse x Nx: ";
    cin >> Nx;
    cout << "Numero punti asse y Ny: ";
    cin >> Ny;
    cout << "Time step dt: ";
    cin >> dt;
    cout << "Numero iterazioni: ";
    cin >> Nsteps;
    cout << "Stampa ogni numero passi: ";
    cin >> Nprint;

    double hx = Lx / (Nx - 1);
    int Nmatx = Nx - 2;
    double normx=0.;
    double hy = Ly / (Ny - 1);
    int Nmaty = Ny - 2;
    double normy = 0.;

    complex<double>* psi0x = new complex<double>[Nmatx];
    complex<double>* psi1x = new complex<double>[Nmatx];
    complex<double>* dx = new complex<double>[Nmatx];
    complex<double>* ux = new complex<double>[Nmatx];
    complex<double>* lx = new complex<double>[Nmatx];
    complex<double>* fx = new complex<double>[Nmatx];

    complex<double>* psi0y = new complex<double>[Nmaty];
    complex<double>* psi1y = new complex<double>[Nmaty];
    complex<double>* dy = new complex<double>[Nmaty];
    complex<double>* uy = new complex<double>[Nmaty];
    complex<double>* ly = new complex<double>[Nmaty];
    complex<double>* fy = new complex<double>[Nmaty];


    for (int i = 1; i < Nx - 1; i++) { // initialising the wave function along x
        double x = hx * i;

        psi0x[i - 1] = exp(pic * qx * x) * exp(-pow(x - x0, 2) / (2 * pow(sigmax, 2))); // gaussian wave packet along x
        // psi0x[i - 1] = sin(qx * x * M_PI * 0.01); // eigenfunction along x; qx stands for the quantum number nx
        normx += pow(abs(psi0x[i - 1]), 2);
    }

    for (int i = 1; i < Ny - 1; i++) { // initialising the wave function along y
        double y = hy * i;

        psi0y[i - 1] = exp(pic * qy * y) * exp(-pow(y - y0, 2) / (2 * pow(sigmay, 2))); // gaussian wave packet along y
        // psi0y[i - 1] = sin(qy * y * M_PI * 0.01); // eigenfunction along y; qy stands for the quantum number ny
        normy += pow(abs(psi0y[i - 1]), 2);
    }

    normx = normx * Lx / Nx;
    normx = sqrt(normx);
    cout << "Norma per la funzione d'onda lungo x: " << normx << "\n";
    for (int i = 0; i < Nmatx; i++) {
        psi0x[i] = psi0x[i] / normx;
    }

    normy = normy * Ly / Ny;
    normy = sqrt(normy);
    cout << "Norma per la funzione d'onda lungo y: " << normy << "\n";
    for (int i = 0; i < Nmaty; i++) {
        psi0y[i] = psi0y[i] / normy;
    }

    for (int i = 0; i < Nmatx; i++) { // Mx matrix
        ux[i] = 1.; // above diagonal
        lx[i] = 1.; // below diagonal
    }

    for (int i = 0; i < Nmaty; i++) { // My matrix
        uy[i] = 1.; // above diagonal
        ly[i] = 1.; // below diagonal
    }

    for (int i = 0; i < Nmatx; i++) {
        double Vx;
        double x = hx * (i + 1);
        if (x >= a && x <= b) {
            Vx = V0;
        }
        else {
            Vx = 0;
        }
        dx[i] = pic * 4. * pow(hx, 2) / dt - 2. - 2 * pow(hx, 2) * Vx; // diagonal
    }

    for (int i = 0; i < Nmaty; i++) {
        double y = hy * (i + 1);
        dy[i] = pic * 4. * pow(hy, 2) / dt - 2.; // diagonal
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
        for (int i = 1; i < Nmatx - 1; i++) {
            double Vx;
            double x = hx * (i + 1);

            if (x >= a && x <= b) {
                Vx = V0;
            }
            else {
                Vx = 0;
            }
            fx[i] = -psi0x[i + 1] + 2. * psi0x[i] - psi0x[i - 1] + (pic * 4. * pow(hx, 2) / dt) * psi0x[i] + 2. * pow(hx, 2) * Vx * psi0x[i];

        }
        fx[0] = -psi0x[1] + 2. * psi0x[0] + (pic * 4. * pow(hx, 2) / dt) * psi0x[0];
        fx[Nmatx - 1] = -psi0x[Nmatx - 2] + 2. * psi0x[Nmatx - 1] + (pic * 4. * pow(hx, 2) / dt) * psi0x[Nmatx - 1];
        tridiag(Nmatx, dx, ux, lx, fx, psi1x);

        for (int i = 1; i < Nmaty - 1; i++) {
            double y = hy * (i + 1);

            fy[i] = -psi0y[i + 1] + 2. * psi0y[i] - psi0y[i - 1] + (pic * 4. * pow(hy, 2) / dt) * psi0y[i];

        }
        fy[0] = -psi0y[1] + 2. * psi0y[0] + (pic * 4. * pow(hy, 2) / dt) * psi0y[0];
        fy[Nmaty - 1] = -psi0y[Nmaty - 2] + 2. * psi0y[Nmaty - 1] + (pic * 4. * pow(hy, 2) / dt) * psi0y[Nmaty - 1];
        tridiag(Nmaty, dy, uy, ly, fy, psi1y);

        if (n % Nprint == 0) {
            for (int i = 0; i < Nmatx; i++) {
                double x = hx * (i + 1);
                for (int j = 0; j < Nmaty; j++) {
                    double y = hy * (j + 1);
                    double mod;

                    mod = pow(abs(psi0x[i]), 2) * pow(abs(psi0y[j]), 2);

                    if (log10(mod) >= -4) {
                        fileg << x << "\t" << y << "\t" << mod << "\n";
                    }
                    else {}
                }
            }
            fileg << '\n';
        }

        normx = 0.;
        for (int i = 0; i < Nmatx; i++) {
            psi0x[i] = psi1x[i];
            normx += pow(abs(psi0x[i]), 2); // unitary operator maintains the norm
        }
        normx = normx * Lx / Nx;

        normy = 0.;
        for (int i = 0; i < Nmaty; i++) {
            psi0y[i] = psi1y[i];
            normy += pow(abs(psi0y[i]), 2); // unitary operator maintains the norm
        }
        normy = normy * Ly / Ny;

        cout << "Passo: " << n << " -> " << "Norma lungo x: " << normx << "\t" << "Norma lungo y: " << normy << "\n";
    }

    fileg.close();
    free(psi0x);
    free(psi1x);
    free(dx);
    free(ux);
    free(lx);
    free(fx);
    free(psi0y);
    free(psi1y);
    free(dy);
    free(uy);
    free(ly);
    free(fy);

    return 0;
}
