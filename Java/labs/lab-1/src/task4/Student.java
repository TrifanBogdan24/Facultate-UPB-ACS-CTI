package task4;
// cand am copiat de la task2
// a modificat automat numele pachetelului

public class Student {
    private String name;

    private double grade;

    Student() {
        // se declara un nou student
    }

    Student(String name, double grade) {
        // se instaniaza un student
        // si se initializeaza
        this.name = name;
        this.grade = grade;
    }

    void set_name(String name) {
        // setter pentru name
        this.name = name;
    }

    String get_name() {
        // getter pentru name
        return this.name;
    }

    void set_grade(double grade) {
        // setter pentru grade
        this.grade = grade;
    }

    double get_grade() {
        // getter pentru grade
        return this.grade;
    }


    // metoda pentru afisarea campurilor
    void get_info() {

        if (this == null) {
            System.out.println("null student");
        }


        System.out.println(this.name + " has the grade " + this.grade);
    }
}
