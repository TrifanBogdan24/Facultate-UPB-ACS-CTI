// Press Shift twice to open the Search Everywhere dialog and type `show whitespaces`,
// then press Enter. You can now see whitespace characters in your code.

class Page {
    private String content;
    public int numberOfPages;

    public Page(String content, int numberOfPages) {
        this.content        = content;
        this.numberOfPages  = numberOfPages;
    }
}

class Book {
    private String title; 			// Compunere
    private Page[] pages; 			// Compunere
    private LibraryRow libraryRow = null; 	// Agregare

    public Book(int size, String title, LibraryRow libraryRow) {
        this.libraryRow = libraryRow;
        this.title = title;

        pages = new Page[size];

        for (int i = 0; i < size; i++) {
            pages[i] = new Page("Page " + i, i);
        }
    }
}

class LibraryRow {
    private String rowName = null; 		// Agregare

    public LibraryRow(String rowName) {
        this.rowName = rowName;
    }
}

class Library {

    public static void main(String[] args) {
        LibraryRow row = new LibraryRow("a1");
        Book book = new Book(100, "title", row);

        // După ce nu mai există nici o referință la obiectul Carte,
        // Garbage Collector-ul va șterge (la un moment dat, nu
        // neapărat imediat) acea instanță, dar obiectul LibraryRow
        // transmis constructorului nu este afectat.

        book = null;
    }
}

public class Main {
    public static void main(String[] args) {
        // Press Alt+Enter with your caret at the highlighted text to see how
        // IntelliJ IDEA suggests fixing it.
        System.out.printf("Hello and welcome!");

        // Press Shift+F10 or click the green arrow button in the gutter to run the code.
        for (int i = 1; i <= 5; i++) {

            // Press Shift+F9 to start debugging your code. We have set one breakpoint
            // for you, but you can always add more by pressing Ctrl+F8.
            System.out.println("i = " + i);
        }
    }
}