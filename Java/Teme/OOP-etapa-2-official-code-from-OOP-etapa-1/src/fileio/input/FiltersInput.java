package fileio.input;

import lombok.Data;

import java.util.ArrayList;

@Data
public final class FiltersInput {
    private String name;
    private String album;
    private ArrayList<String> tags;
    private String lyrics;
    private String genre;
    private String releaseYear; // pentru search song/episode -> releaseYear
    private String artist;
    private String owner; // pentru search playlist si podcast
    private String followers; // pentru search playlist -> followers
    private String description;     // pentru album

    public FiltersInput() {
    }


    @Override
    public String toString() {
        return "FilterInput{"
                + ", name='" + name + '\''
                + ", album='" + album + '\''
                + ", tags=" + tags
                + ", lyrics='" + lyrics + '\''
                + ", genre='" + genre + '\''
                + ", releaseYear='" + releaseYear + '\''
                + ", artist='" + artist + '\''
                + ", owner='" + owner + '\''
                + ", followers='" + followers + '\''
                + '}';
    }
}
