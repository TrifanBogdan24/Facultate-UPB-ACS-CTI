package task2;

import java.util.Random;
import java.lang.Math;

public class Internship {
    String name;

    String minGrade;

    Student[] students = new Student[100];

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
    Student chooseCandidateRandomly() {
        int idx = 0, rep = 100;

        do {
            idx = (int) (Math.random() * this.students.length);
            rep--;
        } while (students[idx] == null && rep > 0);

        // daca toti studenti sunt NULL
        // va returna NULL
        return students[idx];
    }

    void chooseCandidatesForInterview() {


        for (int i = 0; i < this.students.length; i++) {

            if (this.students[i] == null) {
                continue;
            }


            if (this.students[i].grade > Double.parseDouble(this.minGrade)) {
                System.out.print("Candidate ");
                System.out.print(this.students[i].name);
                System.out.print(" got a phone interview at ");
                System.out.println(this.name);
            }
        }
    }
}
