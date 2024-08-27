package main.cmd;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.ArrayList;

@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class OutCmd {
    private String command;

//    @JsonInclude(JsonInclude.Include.ALWAYS)
    private String user;

    private String username;
    private int timestamp;
    private String message;
    private Stat stats;

    private ArrayList<String> results;      // pt Search

    private ArrayList<Object> result;
    // result va fi un array de String-uri daca comanda este
    // showPreferredSong, GetTop5Playlists sau GetTop5Songs

    // result va fi un array de ResultPlaylist-uri daca comanda este
    // showPlaylists

    /**
     *
     * @param result  poate fi ArrayList ori de String-uri, ori de tipul Result Playlists
     */
    public void setResult(final ArrayList<Object> result) {
        this.result = result;
    }

    @Data
    public static class ResultPlaylists {
        private String name;
        private ArrayList<String> songs;
        private String visibility;
        private int followers;

        public ResultPlaylists() {

        }

        /**
         * constructor care creaza un obiect de tip ResultPlaylist
         * (folosit pentru comenzile de output, afisare)
         * inititalizand toate field-urile acestuia
         *
         * @param name numele utilizatorului care detine playlist-ul
         * @param songs numele cantecelor din playlist
         * @param visibility -> "private" sau "public" (by default)
         * @param followers -> numele utilizatorilor care au la follow acest playlist
         */
        public ResultPlaylists(final String name, final ArrayList<String> songs,
                               final String visibility, final int followers) {
            this.name = name;
            this.songs = songs;
            this.visibility = visibility;
            this.followers = followers;
        }
    }

    @Data
    public static class Stat {
        private String name;
        private int remainedTime;
        private String repeat;
        private boolean shuffle;
        private boolean paused;

        public Stat() {

        }
    }

    public OutCmd() {

    }

    /**
     * copiaza filed-uri de la o comanda de input la una de output
     *
     * @param inputcmd -> de la comanda de input se vor transfera field-uri la o comanda de ouput
     */
    public void setDefaultFields(final InCmd inputcmd) {
        this.setCommand(inputcmd.getCommand());
        this.setTimestamp(inputcmd.getTimestamp());
        this.setUser(inputcmd.getUsername());
    }



}
