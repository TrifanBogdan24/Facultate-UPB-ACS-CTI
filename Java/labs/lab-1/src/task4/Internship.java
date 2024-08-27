package task4;
// cand am copiat de la task2
// a modificat automat numele pachetelului

public class Internship {
    private String name;

    private String minGrade;

    private Student[] students = new Student[100];

    Internship() {
        // creeaza o instata a clasei
    }

    Internship(String name, String minGrade, Student[] students) {
        // creaza o instata a clasei
        // si o initializeaza
        this.name = name;
        this.minGrade = minGrade;
        this.students = students;
    }

    void set_name(String name) {
        // setter pentru name
        this.name = name;
    }

    String get_name() {
        // getter pentru name
        return this.name;
    }

    void set_minGrade(String minGrade) {
        // setter pentru minGrade
        this.minGrade = minGrade;
    }

    String get_minGrade() {
        // getter pentru minGrade
        return this.minGrade;
    }

    void set_all_students(Student[] students) {
        // setter pentru toti studentii
        this.students = students;
    }

    Student[] get_all_students() {
        // getter pentru toti studenti
        return this.students;
    }
    Student chooseCandidateRandomly() {
        int idx = 0, rep = 100;

        do {
            idx = (int) (Math.random() * this.students.length);
            rep--;
        } while (this.students[idx].get_grade() == 0 && rep > 0);

        // daca toti studenti sunt NULL
        // va returna NULL
        return this.students[idx];

    }

    void chooseCandidatesForInterview() {

        for (int i = 0; i < this.students.length; i++) {

            double student_grade = this.students[i].get_grade();
            String student_name = this.students[i].get_name();

            if (student_grade == 0) {
                continue;
            }


            if (student_grade > Double.parseDouble(this.minGrade)) {
                System.out.print("Candidate ");
                System.out.print(student_name);
                System.out.print(" got a phone interview at ");
                System.out.println(this.name);
            }
        }
    }
}
