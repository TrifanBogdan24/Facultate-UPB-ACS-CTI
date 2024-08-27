import java.util.Arrays;
import java.util.Collection;
interface CalculatorBase {
    class NullParameterException extends RuntimeException {
    }

    class UnderflowException extends RuntimeException {
    }

    class OverflowException extends RuntimeException {
    }

    Double add(Double nr1, Double nr2);

    Double divide(Double nr1, Double nr2);

    Double average(Collection<Double> numbers);
}

class Calculator implements CalculatorBase {

    // default constructor
    public Calculator() {

    }

    @Override
    public Double add(final Double nr1, final Double nr2) {
        if (nr1 == null || nr2 == null) {
            throw new NullParameterException();
        }

        if (nr1 + nr2 == Double.POSITIVE_INFINITY) {
            throw new OverflowException();
        }

        if (nr1 + nr2 == Double.NEGATIVE_INFINITY) {
            throw new UnderflowException();
        }

        return (nr1 + nr2);
    }

    @Override
    public Double divide(final Double nr1, final Double nr2) {
        if (nr1 == null || nr2 == null) {
            throw new NullParameterException();
        }

        if (nr2 == 0) {
            if (nr1 >= 0) {
                throw new OverflowException();
            }
            throw new UnderflowException();
        }

        return (nr1 / nr2);
    }

    @Override
    public Double average(final Collection<Double> numbers) {
        if (numbers == null) {
            throw new NullParameterException();
        }

        Double sum = 0.0;

        for (Double el: numbers) {
            sum = this.add(sum, el);
        }

        return this.divide(sum, (double) numbers.size());
    }
}


public class Main {
    public static void main(String[] args) {
        // TODO: Initialize the calculator
        CalculatorBase calculator = new Calculator();

        // TODO: Uncomment after implementing the task
        System.out.println(calculator.add(2d, 3d));
        System.out.println(calculator.divide(9d, 4d));
        System.out.println(calculator.average(Arrays.asList(1d, 2d, 3d, 4d)));


        try {
            calculator.add(null, Double.MAX_VALUE);
        } catch (CalculatorBase.NullParameterException e) {
            System.out.println("Null Parameter Exception thrown!");
        }

        try {
            calculator.add(Double.MAX_VALUE, null);
        } catch (CalculatorBase.NullParameterException e) {
            System.out.println("Null Parameter Exception thrown!");
        }

        try {
            calculator.add(null, null);
        } catch (CalculatorBase.NullParameterException e) {
            System.out.println("Null Parameter Exception thrown!");
        }

        try {
            calculator.add(Double.MAX_VALUE, Double.MAX_VALUE);
        } catch (CalculatorBase.OverflowException e) {
            System.out.println("Overflow Exception thrown!");
        }

        try {
            calculator.add(-Double.MAX_VALUE, -Double.MAX_VALUE);
        } catch (CalculatorBase.UnderflowException e) {
            System.out.println("Underflow Exception thrown!");
        }

        try {
            calculator.divide(null, Double.MAX_VALUE);
        } catch (CalculatorBase.NullParameterException e) {
            System.out.println("Null Parameter Exception thrown!");
        }

        try {
            calculator.divide(Double.MAX_VALUE, null);
        } catch (CalculatorBase.NullParameterException e) {
            System.out.println("Null Parameter Exception thrown!");
        }

        try {
            calculator.divide(null, null);
        } catch (CalculatorBase.NullParameterException e) {
            System.out.println("Null Parameter Exception thrown!");
        }

        try {
            calculator.divide(null, null);
        } catch (CalculatorBase.NullParameterException e) {
            System.out.println("Null Parameter Exception thrown!");
        }

        try {
            calculator.divide(1d, 0d);
        } catch (CalculatorBase.OverflowException e) {
            System.out.println("Overflow Exception thrown!");
        }

        try {
            calculator.divide(-1d, 0d);
        } catch (CalculatorBase.UnderflowException e) {
            System.out.println("Underflow Exception thrown!");
        }

        try {
            calculator.average(Arrays.asList(1d, 2d, Double.MAX_VALUE, Double.MAX_VALUE));
        } catch (CalculatorBase.OverflowException e) {
            System.out.println("Overflow Exception thrown!");
        }

        try {
            calculator.average(Arrays.asList(1d, -100d, -Double.MAX_VALUE, -Double.MAX_VALUE));
        } catch (CalculatorBase.UnderflowException e) {
            System.out.println("Underflow Exception thrown!");
        }
    }
}
