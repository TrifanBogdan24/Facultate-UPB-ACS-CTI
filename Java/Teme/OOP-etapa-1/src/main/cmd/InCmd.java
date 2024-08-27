package main.cmd;

import lombok.Data;

import java.util.ArrayList;

@Data
public class InCmd {
    private String command;
    private String username;
    private int timestamp;
    private String type;
    private int itemNumber;
    private String playlistName;
    private int playlistId;

    private int seed;
    private Filter filters;

    @Data
    public class Filter {
        private String name;
        private String album;
        private ArrayList<String> tags;
        private String lyrics;
        private String genre;
        private String releaseYear;
        private String artist;
        private String owner;
    }

}
