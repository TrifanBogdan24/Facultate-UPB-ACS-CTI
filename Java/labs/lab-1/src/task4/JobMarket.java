package task4;
// cand am copiat de la task2
// a modificat automat numele pachetelului

public class JobMarket {
    public static void main(String[] args) {

        // pentru testarea functionalitatii clasei Student

        // task 3
        Student Gigel = new Student();
        Gigel.set_name("Gigel");
        Gigel.set_grade(9.70);
        System.out.println(Gigel.get_name() + " has the grade " + Gigel.get_grade());

        Student Dorel = new Student("Dorel", 7.90);
        Dorel.get_info();

        Student Marcel = new Student("Marcel", 1.20);
        Marcel.set_grade(5.60);
        Marcel.get_info();
        double x = Marcel.get_grade();

        Student Ionel = new Student("Ionel", 10);
        Ionel.get_info();


        // array de studenti
        Student[] studenti = new Student[100];
        studenti[0] = new Student("Bogdan", 9.5);
        studenti[1] = new Student("Cristi", 6.0);
        studenti[2] = new Student("Alex", 8.95);
        studenti[3] = new Student("Raluca", 9.10);
        studenti[4] = new Student("Anda", 10);
        studenti[5] = new Student("Andrei", 7);
        studenti[6] = new Student("Darius", 8.05);
        studenti[7] = new Student("Daria", 8);
        studenti[8] = new Student("Octavian", 9);
        studenti[9] = new Student("Matei", 6);

        /* asa nu va merge
        studenti[10].set_name("Georgian");
        studenti[10].set_grade(9.9);

        studenti[11].set_name("Nicoleta");
        studenti[11].set_grade(10);
        */

        for (int i = 10; i < 100; i++) {
            studenti[i] = new Student();
        }

        Student random_student = null;
        System.out.println();

        // pentru testarea functionalitatii clasei Internsip

        // pentru Google :
        System.out.println("Google :");
        Internship Google = new Internship();
        Google.set_name("Google");
        Google.set_minGrade("8.50");
        Google.set_all_students(studenti);
        Google.chooseCandidatesForInterview();
        random_student = Google.chooseCandidateRandomly();
        System.out.print("Random student for Google : ");
        random_student.get_info();
        System.out.println();

        // pentru Amazon
        System.out.println("Amazon :");
        Internship Amazon = new Internship();
        Amazon.set_name("Amazon");
        Amazon.set_minGrade("8");
        Amazon.set_all_students(studenti);
        random_student = Amazon.chooseCandidateRandomly();
        System.out.print("Random student for Amazon : ");
        random_student.get_info();
        System.out.println();

        // pentru Facebook
        System.out.println("Facebook :");
//        Internship Facebook = new Internship("Facebook", "8.20", studenti);
        Internship Facebook = new Internship();
        Facebook.set_name("Facebook");
        Facebook.set_minGrade("8.20");
        Facebook.set_all_students(studenti);
        Facebook.set_all_students(studenti);
        Facebook.chooseCandidatesForInterview();
        random_student = Facebook.chooseCandidateRandomly();
        System.out.print("Random student for Facebook : ");
        random_student.get_info();
        System.out.println();

        // pentru Microsoft
        System.out.println("Microsoft :");
        Internship Microsoft = new Internship("Microsoft", "9", studenti);
        Microsoft.set_all_students(studenti);
        Microsoft.chooseCandidatesForInterview();
        random_student = Microsoft.chooseCandidateRandomly();
        System.out.print("Random student for Microsoft : ");
        random_student.get_info();
        System.out.println();

        /* sper ca "pe rand" din enuntula de 2. nu insemna
        Google.chooseCandidatesForInterview();
        Amazon.chooseCandidatesForInterview();
        FaceBook.chooseCandidatesForInterview();
        Microsoft.chooseCandidatesForInterview();
         */

        // task 3
        Student s1 = new Student("Bogdan", 9.0);
        Student s2 = new Student("Bogdan", 9.0);

        if (s1.equals(s2)) {
            System.out.println("Instantele aceleasi clase, cu aceleasi valori la fielduri coincid");
        } else {
            // o sa intre pe aceasta ramura
            System.out.println("NU. Instantele aceleasi clase, cu aceleasi valori la fielduri NU COINCID");
        }

        s2 = s1;
        if (s1.equals(s2)) {
            // o sa intre pe aceasta ramura
            System.out.println("Instantele aceleasi clase, cu aceleasi valori la fielduri coincid");
        } else {
            System.out.println("NU. Instantele aceleasi clase, cu aceleasi valori la fielduri NU COINCID");
        }

        /*
        din observatiile mele, metoda equals verifica
        daca doua instante pointeaza catre aceeas zona de memorie

        iar faptul ca fieldurile sunt egale nu este sufiecient ca cele doua variabile
        sa ocupa o unica zona de memorie
         */

        System.out.println();
        System.out.println("Acesta a fost task 4");
    }
}
