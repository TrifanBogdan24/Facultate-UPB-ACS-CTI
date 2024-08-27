import java.util.ArrayList;
import java.util.List;

class Book {
    private String title;
    private String author;
    private String genre;
    private Integer price;


    public Book() {

    }

    /**
     * copy-constructor
     *
     * @param title     titlul cartii
     * @param author    autorul cartii
     * @param genre     genul literar din care face parte
     * @param price     pretul cartii
     */
    public Book(final String title, final String author,
                final String genre, final Integer price) {
        this.title = title;
        this.author = author;
        this.genre = genre;
        this.price = price;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getTitle() {
        return title;
    }

    public void setAuthor(String author) {
        this.author = author;
    }

    public String getAuthor() {
        return author;
    }

    public void setGenre(final String genre) {
        this.genre = genre;
    }

    public String getGenre() {
        return genre;
    }

    public void setPrice(final Integer price) {
        this.price = price;
    }

    public Integer getPrice() {
        return price;
    }
}

class NotEnoughMoneyException extends Exception {

}

class NoSuchBookException extends Exception {

}


class OnlineLibrary {

    private List<Book> books;
    private Integer budget;

    public OnlineLibrary() {
        this.books = new ArrayList<Book>();
        this.budget = 0;
    }

    /**
     * copy constructor, initializeaza instanta cu bugetul
     * @param budget    budgetul utilizatorului
     */
    public OnlineLibrary(final Integer budget) {
        this.books = new ArrayList<Book>();
        this.budget = budget;
    }

    public void setBooks(List<Book> books) {
        this.books = books;
    }

    public List<Book> getBooks() {
        return books;
    }

    public void setBudget(final Integer budget) {
        this.budget = budget;
    }

    public Integer getBudget() {
        return budget;
    }


    public void addBook(final Book book) throws NotEnoughMoneyException {
        if (this.budget < book.getPrice()) {
            // caz de eroare
            throw new NotEnoughMoneyException();
        }

        // operatie executata cu succes
        this.budget = this.getBudget() - book.getPrice();
        this.books.add(book);
    }


    /***
     * @param name  numele cartii
     * @return      cartea asociata acestui nume,
     *              sau null, daca nu exista o care cu acest nume
     */
    public Book getBook(final String name) throws NoSuchBookException {
        for (Book book: this.books) {
            if (book.getTitle().equals(name)) {
                return book;
            }
        }

        throw new NoSuchBookException();
    }
}

public class Main {
    public static void main(String[] args) {

        // TODO Uncomment after implementing the classes
        OnlineLibrary onlineLibrary = new OnlineLibrary(150);

        Book book1 = new Book("Life of Pi", "Yann Martel", "General & Literary Fiction", 40);
        Book book2 = new Book("Man and Boy", "Tony Parson", "General & Literary Fiction", 200);
        Book book3 = new Book("A little life", "Hanya Yanagigihara", "General & Literary Fiction", 50);
        Book book4 = new Book("Pride and Prejudice", "Jane Austen", "Romance", 60);
        List<Book> books = List.of(book1, book2, book3);

        // TODO Add the list of books in the library
        for (Book book: books) {
            try {
                onlineLibrary.addBook(book);
                System.out.println("Added book " + book.getTitle());
            } catch (NotEnoughMoneyException err) {
                System.out.println("Not enough money for " + book.getTitle());
            }
        }

        // TODO Get book4 from the library. If not there, add it
        try {
            Book book = onlineLibrary.getBook(book4.getTitle());
        } catch (NoSuchBookException err) {
            System.out.println("Book " + book4.getTitle() + " not available");
            books = onlineLibrary.getBooks();
            books.add(book4);
            onlineLibrary.setBooks(books);
            System.out.println("Added book " + book4.getTitle());
        }
    }
}
