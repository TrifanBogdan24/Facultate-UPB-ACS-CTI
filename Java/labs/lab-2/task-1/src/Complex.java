import java.util.*;

class Complex {

    private int real, imaginary;

    // TODO: Add constructors

    // primul constructor
    Complex(int real, int imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    Complex() {
        this(0, 0);
    }

    Complex(Complex number) {
        this.real = number.real;
        this.imaginary = number.imaginary;
    }

    // TODO: Add getters and setters
    int getReal() {
        return this.real;
    }

    void setReal(int real) {
        this.real = real;
    }

    int getImaginary() {
        return this.imaginary;
    }

    void setImaginary(int imaginary) {
        this.imaginary = imaginary;
    }

    // TODO: Add addWithComplex
    void addWithComplex(Complex number) {
        this.real += number.real;
        this.imaginary += number.imaginary;
    }

    // TODO: Add showNumber
    void showNumber() {
        int a = this.real;
        int b = this.imaginary;

        if (b > 0) {
            System.out.println(a + " + i * " + b);
        } else if (b < 0) {
            System.out.println(a + " - i * " + (-b));
        } else {
            // b == 0
            System.out.println(a);
        }
    }
}
