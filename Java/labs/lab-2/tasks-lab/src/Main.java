import java.util.Objects;
import java.util.Scanner;

class Form {

    // TODO[0]: Add fields
    private String color;

    // TODO[1]: Add constructors
    public Form() {
        this.color = "white";
    }

    public Form(String color) {
        this.color = color;
    }


    // ad setter and getter for the color field
    void setColor(String color) {
        this.color = color;
    }

    String getColor() {
        return this.color;
    }

    // TODO[2]: Add the function getArea
    public float getArea() {
        return 0;
    }

    // TODO[3]: Override toString
    public String toString() {
        return ("This form has the color " + this.color);
    }
}

class Triangle extends Form {
    // TODO[0]: Add fields
    private float height, base;

    // setters and getters
    @Override
    void setColor(String color) {
        super.setColor(color);
    }

    @Override
    String getColor() {
        return super.getColor();
    }
    void setHeight(float height) {
        this.height = height;
    }

    float getHeight() {
        return this.height;
    }

    void setBase(float base) {
        this.base = base;
    }

    float getBase() {
        return this.base;
    }
    // TODO[1]: Add constructors
    public Triangle() {
        super();
    }

    public Triangle(String color, float height, float base) {
        super(color);
        this.height = height;
        this.base = base;
    }

    public Triangle(float height, float base) {
        super();
        this.height = height;
        this.base = base;
    }


    // TODO[2]: Override toString
    @Override
    public String toString() {
        return ("Triangle: This form has the color " + super.getColor());
    }

    // TODO[3]: Override getArea
    @Override
    public float getArea() {
        return ((this.height * this.base) / 2);
    }

    // TODO[4]: equals() method

    /*
    pot face urmatoarele observatii asupra codului completata automat de catre IntelliJ
    furnieaza doua metode, suprascrise (deci cel mai probabil acestea deja exista pentru orice clasa
    daca cele doua obiecte sunt identice, pointeaza catre aceeasi zona de memorie, atunci este logic ca intoarce true
    clasa poate fi apelata cu orice tip de obiect / alta clasa (pentru alte clase va intoarce logic 0, false)

    daca am trecut de aceasta verificare si se apeleaza cu o alta instanta a aceleasi clase, atunci
    vom verifica egalitatea dintre campurile acestora

    observ ca IntelliJ mai genereaza o alta metoda - hashCode - chiar daca nu este apelata in metoda equals
     */
    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Triangle triangle = (Triangle) o;
        return (this.height == triangle.height && this.base == triangle.base && super.getColor() == triangle.getColor());
    }

    @Override
    public int hashCode() {
        return Objects.hash(height, base);
    }

    // TODO[5] : Downcasting
    public String printTriangleDimensions() {
        return ("Height: " + this.height + " Base: " + this.base);
    }
}

class Square extends Form {

    // TODO[0]: Add fields
    private float side;

    // setters and getters
    @Override
    void setColor(String color) {
        super.setColor(color);
    }

    @Override
    String getColor() {
        return super.getColor();
    }

    void setSide(float side) {
        this.side = side;
    }

    float getSide() {
        return this.side;
    }

    // TODO[1]: Add constructors
    public Square() {
        super();
    }

    public Square(String color, float side) {
        super(color);
        this.side = side;
    }

    Square(float side) {
        super();
        this.side = side;
    }

    // TODO[2]: Override toString
    @Override
    public String toString() {
        return ("Square: This form has the color " + super.getColor());
    }

    // TODO[3]: Override getArea
    @Override
    public float getArea() {
        return (this.side * this.side);
    }

    // TODO[5] : Downcasting
    public String printSquareDimensions() {
        return("Side: " + this.side);
    }

}

class Circle extends Form {
    // TODO[0]: Add fields
    private float radius;

    // TODO[1]: Add constructors
    public Circle() {
        super();
    }

    public Circle(String color, float radius) {
        super(color);
        this.radius = radius;
    }


    // setters and getters
    @Override
    void setColor(String color) {
        super.setColor(color);
    }

    @Override
    String getColor() {
        return super.getColor();
    }

    void setRadius(float Radius) {
        this.radius = radius;
    }

    float getRadius() {
        return this.radius;
    }


    // TODO[2]: Override toString
    @Override
    public String toString() {
        return ("Circle: This form has the color " + super.getColor());
    }

    // TODO[3]: Override getArea
    @Override
    public float getArea() {
        // implicit Math.PI are tipul double, si trebuie sa facem casting (conversie) la float
        // aria cercului = pi * R^2
        return ((float)(Math.PI) * this.radius * this.radius);
    }

    // TODO[5] : Downcasting
    public String getCircleDimensions() {
        return ("Radius: " + this.radius);
    }

    public String printCircleDimensions() {
        return ("Radius: " + this.radius);
    }
}

public class Main {
    public static void main(String[] args) {

        Scanner scanner = new Scanner(System.in);
        int task = scanner.nextInt();
        // TODO: Uncomment the code after implementing the task.
        // Task 1:

        Form form1 = new Form();
        Form form2 = new Form("blue");

        // Task 2:
        Triangle triangle1 = new Triangle("red", 4, 3);
        Triangle triangle2 = new Triangle();
        Square square1 = new Square("yellow", 4);
        Square square2 = new Square();
        Circle circle1 = new Circle("green",10);
        Circle circle2 = new Circle();

        // array of forms
        Form[] forme = new Form[100];
        for (int i = 0; i < 100; i++)
            forme[i] = null;
        forme[0] = new Triangle("red" , 4, 3);
        forme[1] = new Triangle("white", 0, 0);
        forme[2] = new Square("yellow", 4);
        forme[3] = new Square("white", 0);
        forme[4] = new Circle("green", 10);
        forme[5] = new Circle("white", 0);

//        Task 4: add in this order the elements in the shape vector: triangle1, triangle2,
//                  square1, square2, circle1, circle2

//        Form [] forms = new Form[6];

        switch(task) {
            case 1:
//                Task 1:
                System.out.println(form1);
                System.out.println(form2);
                break;
            case 2:
//                Task 2:
                System.out.println(triangle1);
                System.out.println("The Area is: " + triangle1.getArea());
                System.out.println(triangle2);
                System.out.println("The Area is: " + triangle2.getArea());
                System.out.println(square1);
                System.out.println("The Area is: " + square1.getArea());
                System.out.println(square2);
                System.out.println("The Area is: " + square2.getArea());
                System.out.println(circle1);
                System.out.println("The Area is: " + circle1.getArea());
                System.out.println(circle2);
                System.out.println("The Area is: " + circle2.getArea());
                break;
            case 3:
//                Task 3:
                Triangle triangle3 = new Triangle("yellow", 4, 3);
                Triangle triangle4 = new Triangle("red", 4, 3);
                System.out.println(triangle1.equals(triangle3));
                System.out.println(triangle1.equals(triangle4));
                System.out.println(triangle1.equals(square1));
                break;
            case 4:
                // Task 4: for each element of the vector call the toString function
                for (int i = 0; i < forme.length; i++)
                    if (forme[i] != null)
                        System.out.println(forme[i].toString());
                /*
                metoda toString va fi apelata din interiorul clasei copil (adica face overriding pe metoda)
                daca ar fi fost
                    forme[6] = new Form("white");
                    System.out.println(forme[6].toString());
                normal ca se va apela din superclasa
                Practic, foloseste metoda din (subclasa) cea mai de jos daca am privi ca un arbore
                 */
                break;

            case 5:
                // Task 5: Loop through the vector from the previous exercise and, using downcasting to the appropriate class, call

                // methods specific to each class (printTriangleDimensions for Triangle, printCircleDimensions for Circle
                // printSquareDimensions for Square)

                for (int i = 0; i < forme.length; i++) {
                    if (forme[i] == null)
                        continue;
                    if (forme[i] instanceof Triangle) {
                        Triangle casted = (Triangle)forme[i];
                        System.out.println(casted.printTriangleDimensions());
                    } else if (forme[i] instanceof Square) {
                        Square casted = (Square)forme[i];
                        System.out.println(casted.printSquareDimensions());
                    } else if (forme[i] instanceof Circle) {
                        Circle casted = (Circle)forme[i];
                        System.out.println(casted.printCircleDimensions());
                    }
                }

                break;
            case 6:
                for (int i = 0; i < forme.length; i++) {
                    if (forme[i] == null)
                        continue;
                    if (forme[i].getClass() == Triangle.class) {
                        Triangle casted = (Triangle)forme[i];
                        System.out.println(casted.printTriangleDimensions());
                    } else if (forme[i].getClass() == Square.class) {
                        Square casted = (Square)forme[i];
                        System.out.println(casted.printSquareDimensions());
                    } else if (forme[i].getClass() == Circle.class) {
                        Circle casted = (Circle)forme[i];
                        System.out.println(casted.printCircleDimensions());
                    }
                }
                //Task 6: Show shape sizes without using instanceof
                break;
        }
    }
}
