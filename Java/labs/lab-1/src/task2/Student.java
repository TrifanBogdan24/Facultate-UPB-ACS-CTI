package task2;

public class Student {
    String name;

    double grade;

    Student() {
        // se declara un nou student
    }

    Student(String name, double grade) {
        // se instaniaza un student
        // si se initializeaza
        this.name = name;
        this.grade = grade;
    }

    // metoda pentru afisarea campurilor
    void get_info() {

        if (this == null) {
            System.out.println("null student");
        }


        System.out.println(this.name + " has the grade " + this.grade);
    }
}
