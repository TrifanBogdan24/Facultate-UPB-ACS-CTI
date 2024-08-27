import java.util.*;

class Book {
    private String title;
    private String author;
    private int year;

    public Book(String title, String author, int year) {
        this.title = title;
        this.author = author;
        this.year = year;
    }

    public String toString() {
        // TODO
        String ans = this.title + ", written by " + this.author;
        ans = ans + ", published in " + this.year;
        return ans;
        // return null;
    }

    void afis() {
        System.out.println(this.toString());
    }
}
