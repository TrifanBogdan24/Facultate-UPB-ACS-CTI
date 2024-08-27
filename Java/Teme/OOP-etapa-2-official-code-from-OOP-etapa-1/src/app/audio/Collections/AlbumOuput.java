package app.audio.Collections;

import lombok.Data;

import java.util.ArrayList;

@Data
public class AlbumOuput {
    private String name;
    private ArrayList<String> songs;

    public AlbumOuput() {

    }

    /**
     *
     * @param name          album's name
     * @param songs         album's songs
     */
    public AlbumOuput(final String name, final ArrayList<String> songs) {
        this.name = name;
        this.songs = songs;
    }
}
