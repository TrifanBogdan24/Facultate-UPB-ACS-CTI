import java.sql.SQLOutput;
import java.util.*;

interface Visitable {
    public float accept(Visitor visitor);
}

interface Visitor {
    public float visit(Ciocolata ciocolata);
    public float visit(Suc suc);
    public float visit(Fruct fruct);

    // TODO 2: Adaugati metoda visit() pentru legume
    public float visit(Leguma leguma);
}

class Fruct implements Visitable {
    private float pretKilogram;
    private float greutate;
    private String nume;

    // TODO 1: implementati metoda accept()
    @Override
    public float accept(Visitor visitor) {
        return visitor.visit(this);
    }

    public Fruct(float pretKilogram, float greutate, String nume) {
        this.pretKilogram = pretKilogram;
        this.greutate = greutate;
        this.nume = nume;
    }

    public Fruct() {

    }

    // TODO 1: adaugati getteri si setteri pt fiecare camp


    public float getPretKilogram() {
        return pretKilogram;
    }

    public void setPretKilogram(float pretKilogram) {
        this.pretKilogram = pretKilogram;
    }
    public float getGreutate() {
        return greutate;
    }
    public void setGreutate(float greutate) {
        this.greutate = greutate;
    }

    public String getNume() {
        return nume;
    }

    public void setNume(String nume) {
        this.nume = nume;
    }
}

class Ciocolata implements Visitable {
    private float pretBucata;
    private int nrBucati;
    private float reducere;
    private String nume;

    // TODO 1: implementati metoda accept()
    @Override
    public float accept(Visitor visitor) {
        return visitor.visit(this);
    }

    public Ciocolata(float pretBucata, int nrBucati, float reducere, String nume) {
        this.pretBucata = pretBucata;
        this.nrBucati = nrBucati;
        this.reducere = reducere;
        this.nume = nume;
    }

    public Ciocolata() {

    }

    // TODO 1: adaugati getteri si setteri pt fiecare camp

    public float getPretBucata() {
        return pretBucata;
    }

    public void setPretBucata(float pretBucata) {
        this.pretBucata = pretBucata;
    }

    public int getNrBucati() {
        return nrBucati;
    }

    public void setNrBucati(int nrBucati) {
        this.nrBucati = nrBucati;
    }

    public float getReducere() {
        return reducere;
    }

    public void setReducere(float reducere) {
        this.reducere = reducere;
    }

    public String getNume() {
        return nume;
    }

    public void setNume(String nume) {
        this.nume = nume;
    }
}

class Suc implements Visitable {
    private float pretBucata;
    private int nrBucati;
    private float reducere;
    private String nume;

    // TODO 1: implementati metoda accept()
    @Override
    public float accept(Visitor visitor) {
        return visitor.visit(this);
    }

    public Suc(float pretBucata, int nrBucati, float reducere, String nume) {
        this.pretBucata = pretBucata;
        this.nrBucati = nrBucati;
        this.reducere = reducere;
        this.nume = nume;
    }

    // TODO 1: adaugati getteri si setteri pt fiecare camp
    public float getPretBucata() {
        return pretBucata;
    }

    public void setPretBucata(float pretBucata) {
        this.pretBucata = pretBucata;
    }

    public int getNrBucati() {
        return nrBucati;
    }

    public void setNrBucati(int nrBucati) {
        this.nrBucati = nrBucati;
    }

    public float getReducere() {
        return reducere;
    }

    public void setReducere(float reducere) {
        this.reducere = reducere;
    }

    public String getNume() {
        return nume;
    }

    public void setNume(String nume) {
        this.nume = nume;
    }
}


class Leguma implements Visitable {
    private float preKilogram;
    private float greutate;
    private float stoc;
    private String nume;

    // TODO 1: implementati metoda accept()
    @Override
    public float accept(Visitor visitor) {
        return visitor.visit(this);
    }

     public Leguma(float pretKilogram, float greutate, float stoc, String nume) {
        this.preKilogram = pretKilogram;
        this.greutate = greutate;
        this.stoc = stoc;
        this.nume = nume;
     }


    // TODO 1: adaugati getteri si setteri pt fiecare camp


    public float getPreKilogram() {
        return preKilogram;
    }

    public void setPreKilogram(float preKilogram) {
        this.preKilogram = preKilogram;
    }

    public float getGreutate() {
        return greutate;
    }

    public void setGreutate(float greutate) {
        this.greutate = greutate;
    }

    public float getStoc() {
        return stoc;
    }

    public void setStoc(float cantitate) {
        this.stoc = cantitate;
    }

    public String getNume() {
        return nume;
    }

    public void setNume(String nume) {
        this.nume = nume;
    }
}



class CalculatorPret implements Visitor {

    // TODO 1: implementati metodele din interfata Visitor
    @Override
    public float visit(Ciocolata ciocolata) {

        // pret (integral) ciocolata
        String nume = ciocolata.getNume();
        float reducere = ciocolata.getReducere();
        int nrBucati = ciocolata.getNrBucati();
        float pretBucata = ciocolata.getPretBucata();

        float pret = pretBucata * nrBucati;
        float reducerePret = 0;

        if (nume.equals("Milka")) {
            reducerePret += (float) (0.5 * nrBucati);
        }

        if (nrBucati >= 5) {
            reducerePret += reducere * nrBucati * pretBucata;
        }

        pret -= reducerePret;

        System.out.println(nrBucati + " bucati ciocolata " + nume + " costa " + pret + " lei.");
        return pret;
    }

    @Override
    public float visit(Suc suc) {

        // pret (integral) suc
        String nume = suc.getNume();
        int nrBucati = suc.getNrBucati();
        float pretBucata = suc.getPretBucata();
        float reducere = suc.getReducere();

        float pret = pretBucata * nrBucati;
        float reducerePret = 0;

        if (nrBucati >= 10) {
            reducerePret += (float) (nrBucati / 10) * reducere * pret;
        }

        pret -= reducerePret;

        System.out.println(nrBucati + " sticle de " + nume + " costa " + pret + " lei.");
        return pret;
    }

    @Override
    public float visit(Fruct fruct) {

        // pret (integral) fructe
        String nume = fruct.getNume();
        float pretKilogram = fruct.getPretKilogram();
        float greutate = fruct.getGreutate();

        float pret = greutate * pretKilogram;

        System.out.println(greutate + " kg de " + nume + " costa " + pret + " lei.");
        return pret;
    }

    // TODO 2: implementati visit() pt noua clasa adaugata
    @Override
    public float visit(Leguma leguma) {

        // pret (integral) legume
        String nume = leguma.getNume();
        float pretKilogram = leguma.getPreKilogram();
        float greutate = leguma.getGreutate();
        float stoc = leguma.getStoc();        // cantitatea din stoc

        float pret = pretKilogram * greutate;
        float procent = ((float)stoc / 400);
        float reducerePret = pret * procent;

        pret -= reducerePret;

        System.out.println(greutate + " kg de " + nume + " costa " + pret + " lei.");
        return pret;
    }
}

class CalculatorReducere implements Visitor {
    // TODO 2: implementati metodele din interfata Visitor

    @Override
    public float visit(Ciocolata ciocolata) {

        // reducere ciocolata
        String nume = ciocolata.getNume();
        float reducere = ciocolata.getReducere();
        int nrBucati = ciocolata.getNrBucati();
        float pretBucata = ciocolata.getPretBucata();

        float reducerePret = 0;

        if (nume.equals("Milka")) {
            reducerePret += (float) (0.5 * nrBucati);
        }

        if (nrBucati >= 5) {
            reducerePret += reducere * nrBucati * pretBucata;
        }

        if (reducerePret > 0) {
            System.out.println("Am economisit pe ciocolata " + reducerePret + " lei.");
        }

        return reducerePret;
    }

    @Override
    public float visit(Suc suc) {

        // reducere suc
        String nume = suc.getNume();
        int nrBucati = suc.getNrBucati();
        float pretBucata = suc.getPretBucata();
        float reducere = suc.getReducere();

        float pret = pretBucata * nrBucati;
        float reducerePret = 0;

        if (nrBucati >= 10) {
            reducerePret += (float) (nrBucati / 10) * reducere * pret;
        }

        System.out.println("Am economisit pe suc " + reducerePret + " lei.");
        return reducerePret;
    }

    @Override
    public float visit(Fruct fruct) {

        // reducere fruct
        return 0;           // nu exista reducere pt fructe
    }

    @Override
    public float visit(Leguma leguma) {

        // reducare leguma
        String nume = leguma.getNume();
        float pretKilogram = leguma.getPreKilogram();
        float greutate = leguma.getGreutate();
        float stoc = leguma.getStoc();        // cantitatea din stoc

        float pret = pretKilogram * greutate;
        float procent = ((float)stoc / 400);
        float reducerePret = pret * procent;

        System.out.println("Am economisit pe " + nume + " " + reducerePret + " lei.");
        return reducerePret;
    }

}

class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int task = scanner.nextInt();

        List<Visitable> listaCumparaturi = new ArrayList<>();
        listaCumparaturi.add(new Fruct(2.5f, 5f, "banane"));
        listaCumparaturi.add(new Fruct(1.25f, 1.5f, "mere"));
        listaCumparaturi.add(new Ciocolata(7f, 3, 0.3f, "Milka"));
        listaCumparaturi.add(new Ciocolata(3f, 6, 0.15f, "Heidi"));
        listaCumparaturi.add(new Suc(4.75f, 25, 0.05f, "Tymbark"));
        listaCumparaturi.add(new Suc(5f, 10, 0.1f, "Pepsi"));
        // TODO 2: decomentati codul pentru rezolvarea task-ului 2
        List<Visitable> listaCumparaturi2 = new ArrayList<>(listaCumparaturi);
        listaCumparaturi2.add(new Leguma(3.49f, 2.6f, 40, "rosii"));
        listaCumparaturi2.add(new Leguma(2.3f, 1.3f, 20, "morcovi"));
        listaCumparaturi2.add(new Leguma(5.1f, 0.5f, 5, "ciuperci"));


        float total = 0;
        float reducere = 0;

        // TODO : calculam pretul total si cel redus din prima lista de cumparaturi
        Visitor visitorPret = new CalculatorPret();
        Visitor visitorReducere = new CalculatorReducere();




        switch (task) {
            case 1:
                // TODO 1: instantiati obiectul visitor si calculati pretul total

                for (Visitable item : listaCumparaturi) {
                    total = total + item.accept(visitorPret);
                }

                System.out.println("\nTotal cheltuit: " + total + " lei.");
                break;
            case 2:
                // TODO 2: instantiati obiectul visitor si calculati reducerea totala

                for (Visitable item : listaCumparaturi2) {
                    total = total + item.accept(visitorPret);
                    reducere = reducere + item.accept(visitorReducere);
                }

                System.out.println("\nTotal cheltuit: " + total + " lei.");
                System.out.println("Total economisit: " + reducere + " lei.");
                break;
            default:
                System.out.println("Enter a valid task number!");
                break;
        }
    }

}
