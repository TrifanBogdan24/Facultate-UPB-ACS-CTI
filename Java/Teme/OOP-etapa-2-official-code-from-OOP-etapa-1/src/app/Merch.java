package app;

import lombok.Data;

@Data
public class Merch {
    private String artistName;
    private String name;
    private String description;
    private Integer price;

    public Merch() {

    }

    public Merch(final String artistName, final String name,
                 final String description, final Integer price) {
        this.artistName = artistName;
        this.name = name;
        this.description = description;
        this.price = price;
    }
}
